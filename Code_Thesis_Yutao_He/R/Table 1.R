# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

## Table 0:
sum_1 <- summary(merged[merged$Year == 2012,])
sum_2 <- summary(merged[merged$Poverty == 0 & merged$Year == 2012,])
sum_3 <- summary(merged[merged$Poverty == 1 & merged$Year == 2012,])
## Standard Deviation
sqrt(var(merged[merged$Year == 2012,]$hospital_bed))
sqrt(var(merged[merged$Poverty == 1 & merged$Year == 2012,]$hospital_bed))
sqrt(var(merged[merged$Poverty == 0 & merged$Year == 2012,]$hospital_bed))
## t.test
t.test(merged[merged$Poverty == 0 & merged$Year == 2012,]$hospital_bed,
       merged[merged$Poverty == 1 & merged$Year == 2012,]$hospital_bed)
