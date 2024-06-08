import geopandas as gp
import json
import numpy as np
import pygeohash as pgh
import time
from shapely.geometry import Point
from mpi4py import MPI

def reverse_geo_coding(west, north, data, geo_hash_map, mpi_size):
    #Initialize the dictionary.
    dict_result = np.empty([mpi_size*2400,4])

    #Set the increment.
    increment = 10/2400

    #Start the grid search.
    for i in range(mpi_size):
        for j in range(2400):
            lon = west + increment*i
            lat = north - increment*j
            #Obtain the geohash value.
            geo_hash_val = pgh.encode(lat,lon,precision=6)
            #Obtain the index.
            index = i*2400 + j
            
            #If the hash value is in the keys, then find the location.
            if geo_hash_val in geo_hash_map.keys():
                #Create the point.
                point = Point(lon,lat)
                subset = data[data['dt_adcode'].isin(geo_hash_map[geo_hash_val])]
                subset = subset.reset_index()

                #Loop through the subset.
                for m in range(len(subset)):
                    if subset.loc[m,'geometry'].contains(point):
                        dict_result[index,0] = lon
                        dict_result[index,1] = lat
                        dict_result[index,2] = subset.loc[m,'ct_adcode']
                        dict_result[index,3] = subset.loc[m,'dt_adcode']
                        break

            #Else, it is not in China.
            else:
                dict_result[index,1] = (lon,lat)
                dict_result[index,2] = "Not In China"
                dict_result[index,3] = "Not In China"
    return (dict_result)


def mpi_reverse_coding(West,North,shp_data,geohash):
    #Get rank and size.
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    size = comm.Get_size()
  
    
    #Split the data.
    data_length = int(2400/size)

    #Generate west and north.
    West_Rank = West + 10/2400*rank*data_length
    North_Rank = North

    #Conduct the reverse geocoding.
    mapping_result = reverse_geo_coding(West_Rank, North_Rank, shp_data, geohash, data_length)

    #Gather the result.
    result_all = None
    if rank == 0:
        result_all = np.empty(2400*2400,4)
    comm.Gatherv(sendbuf=mapping_result, recvbuf=result_all, root = 0)

    if rank == 0:
        return result_all


def main():
    time_1 = time.time() 
    #Load the geohash map.
    with open('geo_hash_map.json', 'r') as json_file:
        geo_hash_map = json.load(json_file)
    
    #Close the file.
    json_file.close()

    #Load the GIS data.
    data = gp.read_file('./District/district_wgs.shp')
    
    #Input the West side and North side number.
    West = 110
    North = 40

    #Obtain the result.
    result = mpi_reverse_coding(West, North, data, geo_hash_map)
    
    #Collect the dictionary.
    dt_dict = {}
    ct_dict = {}
    for i in range(len(result)):
        try:
            dt_dict[result[i][3][0:6]].append((result[i][0],result[i][1]))
            ct_dict[result[i][2][0:6]].append((result[i][0],result[i][1]))
        except:
            dt_dict[result[i][3][0:6]] = [(result[i][0],result[i][1])]
            ct_dict[result[i][2][0:6]] = [(result[i][0],result[i][1])]
    
    #Write into json.
    file_1_string = "dist_" + str(West) + "_"+ str(North) + ".json"
    file_2_string = "city_" + str(West) + "_"+ str(North) + ".json"
    with open(file_1_string, 'w') as file_1:
        json.dump(dt_dict,file_1)
    file_1.close()
    with open(file_2_string, 'w') as file_2:
        json.dump(ct_dict,file_2)
    file_2.close()
    
    #Obtain the elased time.
    time_2 = time.time()
    print(time_2-time_1)

if __name__ == "__main__":
    main()
