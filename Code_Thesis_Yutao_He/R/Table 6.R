# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

## Heterogenous Effects.
for (phase_out_year in 2016:2020){
  # GDP
  # Baseline DID
  model = lm(log_gdp ~ Poverty + Post + DID + factor(Year) + factor(County_Code), 
             merged,
             subset = merged$Phase_Out_Year == phase_out_year | merged$Poverty == 0)
  var_name <- paste("model_5_gdp_", as.character(phase_out_year), sep = "")
  assign(var_name, model)
  # DRDID
  model<- att_gt(yname = "log_gdp", tname = "year", idname = "County_Code",
                 gname = "First_Treated", xformla = ~ non_param, 
                 merged[merged$Phase_Out_Year == phase_out_year | merged$Poverty == 0,],
                 control_group = c("nevertreated"))
  var_name <- paste("drdid_gdp_", as.character(phase_out_year), sep = "")
  assign(var_name, model)
  # Night light
  # Baseline DID
  model = lm(night_light_log ~ Poverty + Post + DID + factor(Year) + factor(County_Code), 
             merged,
             subset = merged$Phase_Out_Year == phase_out_year | merged$Poverty == 0)
  var_name <- paste("model_5_nl", as.character(phase_out_year), sep = "")
  assign(var_name, model)
  # DRDID
  model<- att_gt(yname = "night_light_log", tname = "year", idname = "County_Code",
                 gname = "First_Treated", xformla = ~ non_param, 
                 merged[merged$Phase_Out_Year == phase_out_year | merged$Poverty == 0,],
                 control_group = c("nevertreated"))
  var_name <- paste("drdid_nl_", as.character(phase_out_year), sep = "")
  assign(var_name, model)
}
