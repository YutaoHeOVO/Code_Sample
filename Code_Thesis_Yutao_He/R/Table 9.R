# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

## Table 9:
balanced$length = balanced$Poverty * (balanced$Phase_Out_Year - 2015)
cor(balanced[,c('logit','probit','non_param','length')])
## Simple OLS
model_0_1 <- lm(length~logit,data=balanced[balanced$year==2012,])
model_0_2 <- lm(length~probit,data=balanced[balanced$year==2012,])
model_0_3 <- lm(length~non_param,data=balanced[balanced$year==2012,])
## Poisson regression
model_0_4 <- glm(length~logit, family = 'poisson', data = balanced[balanced$year == 2012,])
model_0_5 <- glm(length~probit, family = 'poisson', data = balanced[balanced$year == 2012,])
model_0_6 <- glm(length~non_param, family = 'poisson', data = balanced[balanced$year == 2012,])
