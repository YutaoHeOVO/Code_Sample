# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")
merged$log_gdp_squared = merged$log_gdp^2


## Table 1:
# Panel 1.A
# Run the regression.
#model_1_1 = lm(log_gdp~night_light_log+night_light_log_squared+factor(Year)+factor(County_Code), data = merged)
#summary(model_1_1)
#model_1_2 = lm(log_gdp~night_light_log+night_light_log_squared+Gini+factor(Year)+factor(County_Code), data = merged)
#summary(model_1_2)
#model_1_3 = lm(log_gdp~night_light_log+night_light_log_squared+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 0)
#summary(model_1_3)
#model_1_4 = lm(log_gdp~night_light_log+night_light_log_squared+Gini+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 0)
#summary(model_1_4)
#model_1_5 = lm(log_gdp~night_light_log+night_light_log_squared+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 1)
#summary(model_1_5)
#model_1_6 = lm(log_gdp~night_light_log+night_light_log_squared+Gini+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 1)
#summary(model_1_6)
model_1_1 = lm(night_light_log~log_gdp+factor(Year)+factor(County_Code), data = merged)
summary(model_1_1)
model_1_2 = lm(night_light_log~log_gdp+Gini+factor(Year)+factor(County_Code), data = merged)
summary(model_1_2)
model_1_3 = lm(night_light_log~log_gdp+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 0)
summary(model_1_3)
model_1_4 = lm(night_light_log~log_gdp+Gini+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 0)
summary(model_1_4)
model_1_5 = lm(night_light_log~log_gdp+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 1)
summary(model_1_5)
model_1_6 = lm(night_light_log~log_gdp+Gini+factor(Year)+factor(County_Code), data = merged, subset = merged$Poverty == 1)
summary(model_1_6)

# Panel 1.B
# Generate the variables.
merged$night_light = log(merged $ Mean)
merged$night_light_squared = merged$night_light^2
merged$log_per_capita = log(merged$per_capita)
# Run the regression.
model_1_7 = lm(night_light~log_per_capita + factor(Year) + factor(County_Code), data = merged)
summary(model_1_7)
model_1_8 = lm(night_light~log_per_capita + Gini + factor(Year) + factor(County_Code), data = merged)
summary(model_1_8)
model_1_9 = lm(night_light~log_per_capita + factor(Year) + factor(County_Code), data = merged, subset = merged$Poverty==0)
summary(model_1_9)
model_1_10 = lm(night_light~log_per_capita + Gini + factor(Year) + factor(County_Code), data = merged, subset = merged$Poverty==0)
summary(model_1_10)
model_1_11 = lm(night_light~log_per_capita + factor(Year) + factor(County_Code), data = merged, subset = merged$Poverty==1)
summary(model_1_11)
model_1_12 = lm(night_light~log_per_capita + Gini + factor(Year) + factor(County_Code), data = merged, subset = merged$Poverty==1)
summary(model_1_12)
