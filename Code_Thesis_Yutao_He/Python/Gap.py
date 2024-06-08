from mpi4py import MPI
from osgeo import gdal
import os
import math
import time
import numpy as np
import struct
import matplotlib.pyplot as plt

def obtain_metadata(path, file_list):
    # Read one sample file.
    hdf_file = gdal.Open(path+file_list[0])
    subDatasets = hdf_file.GetSubDatasets()
    rad = gdal.Open(subDatasets[4][0])        
    meta = rad.GetMetadata_Dict()
        
    # Extract geoetry from headers
    year = meta['StartTime'][0:4]
    west = int(meta['WestBoundingCoord'])
    north = int(meta['NorthBoundingCoord'])
    vt = int(meta['VerticalTileNumber'])
    ht = int(meta['HorizontalTileNumber'])
    year = meta['HDFEOS_GRIDS_VNP_Grid_DNB_RangeBeginningDate'][0:4]
    return (west,north,ht,vt,year)

def create_file_name(Metadata):
    Region = "H" + Metadata[2] + "V" + Metadata[3] + "_"
    Log = "_Log"
    Extension_TIFF = ".tif"
    Extension_PNG = ".png"
    file_1_tiff = Region + Metadata[4] + Extension_TIFF
    file_1_png = Region + Metadata[4] + Extension_PNG
    file_2_tiff = Region + Metadata[4] + Log + Extension_TIFF
    file_2_png = Region + Metadata[4] + Log + Extension_PNG
    return(file_1_tiff,file_1_png,file_2_tiff,file_2_png)

def gdal_process(path, file_list, start_line, end_line):
    # number of scanlines to read across all files
    fmttypes = {'Byte':'B', 'UInt16':'H', 'Int16':'h', 'UInt32':'I', 'Int32':'i', 'Float32':'f', 'Float64':'d'}
    chunk = end_line - start_line
    rad_m = np.ones([len(file_list),chunk,2400])

    # Serially process the files.
    for f in np.arange(0,len(file_list)):
        hdf_file = gdal.Open(path+file_list[f])
        subDatasets = hdf_file.GetSubDatasets()
        
        # open radiance, lunar illumination, and cloud mask bands
        rad = gdal.Open(subDatasets[2][0])
        rad_band = rad.GetRasterBand(1)
        rad_BandType = gdal.GetDataTypeName(rad_band.DataType)

        for t in np.arange(start_line, end_line):
            index = t - start_line
            rad_scan = rad_band.ReadRaster(0,int(t),rad_band.XSize,1,rad_band.XSize,1,rad_band.DataType)
            rad_values = struct.unpack(fmttypes[rad_BandType] * rad_band.XSize, rad_scan)
            # Configure the cloud mask.
            for i in np.arange(len(rad_values)):
                if rad_values[i] < 5000:
                    rad_m[f][index][i] = rad_values[i]
                else:
                    rad_m[f][index][i] = np.nan
    rad_m = np.nanmean(rad_m, axis = 0)
    return rad_m

def mpi_gdal_tiff(path, file_list):
    #Obtain the mpi communication identifier.
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    size = comm.Get_size()

    #Split the list into chunks.
    chunk_size = int(2400/size)
    start = int(rank*chunk_size)
    end = int((rank+1)*chunk_size)

    #Perform the chunk-wise operation.
    result = gdal_process(path, file_list, start, end)
    gdal_all = None
    if rank == 0:
        gdal_all = np.empty([2400,2400])
    comm.Gatherv(sendbuf= result, recvbuf= gdal_all, root = 0)
    return gdal_all

def take_log(output_data):
    output_log = np.ones([2400,2400])
    for i in range(2400):
        for j in range(2400):
            try:
                output_log[i,j] = math.log(output_data[i,j])
            except:
                output_log[i,j] = float(np.nan)
    return output_log

def write_tiff(file_names, output_data, pos, Metadata):
    index = pos * 2
    driver = gdal.GetDriverByName("GTiff")
    out_1 = driver.Create(file_names[index+0], 2400, 2400, 1, gdal.GDT_Float32)
    out_1.SetGeoTransform((Metadata[0],10/2400,0,Metadata[1],0,-10/2400))
    out_1.GetRasterBand(1).WriteArray(output_data)
    plt.imsave(file_names[index+1],output_data,dpi=1200,cmap="gray")

def main():
    time_1 = time.time()
    # read files
    path = "./SUOMI_NPP_Data/"
    files = []
    for filename in os.listdir(path):
        if filename.endswith(".h5"):
            files.append(filename)

    #Obtain Metadata
    Metadata = obtain_metadata(path, files)

    #Output file names.
    file_names = create_file_name(Metadata)

    #Conduct parallelization.
    output_matrix = mpi_gdal_tiff(path, files)
    if output_matrix is not None:
        output_log = take_log(output_matrix)

        #Write into figures.
        write_tiff(file_names, output_matrix, 0, Metadata)
        write_tiff(file_names, output_log, 1, Metadata)
    time_2 = time.time()
    print(time_2 - time_1)

if __name__ == '__main__':
    main()
    
