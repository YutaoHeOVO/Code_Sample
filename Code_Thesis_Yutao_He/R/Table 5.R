# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

for (phase_out_year in 2016:2020){
  model = lm(log_gdp~night_light_log + night_light_log_squared
             +factor(Year)+factor(County_Code), 
             data = merged, 
             subset = merged$Phase_Out_Year == phase_out_year)
  index = phase_out_year - 2015
  var_name = paste("model_4_", as.character(index), sep = "")
  assign(var_name, model)
}
