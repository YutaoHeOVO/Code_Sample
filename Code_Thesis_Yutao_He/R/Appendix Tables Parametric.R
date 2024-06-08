# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

library("stargazer")
stargazer(logit_regression, phase_out_logit_2016, phase_out_logit_2017, 
          phase_out_logit_2018,phase_out_logit_2019,
          title = "Logistic Model for Endogenous Phasing In and Out",
          out = "Logistic.tex")
stargazer(probit_regression, phase_out_probit_2016, phase_out_probit_2017, 
          phase_out_probit_2018, phase_out_probit_2019,
          title = "Probit Model for Endogenous Phasing In and Out",
          out = "Probit.tex")