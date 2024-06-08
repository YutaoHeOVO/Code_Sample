# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")# Baseline DID
model_2_1_1 <- lm(log_gdp ~ Poverty + Post + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_1_1)
model_2_1_2 <- lm(log_gdp ~ Poverty + Post + DID + Phase_Out + factor(Year) + factor(County_Code), merged)
summary(model_2_1_2)
model_2_2_1 <- lm(night_light_log ~ Poverty + Post + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_2_1)
model_2_2_2 <- lm(night_light_log ~ Poverty + Post + DID + Phase_Out + factor(Year) + factor(County_Code), merged)
summary(model_2_2_2)

library("did")
att_gdp<- att_gt(yname = "log_gdp", tname = "year", idname = "County_Code",
                 gname = "First_Treated", xformla = ~ non_param, merged_new,
                 control_group = c("nevertreated"), bstrap = TRUE)
figure_1_1 <- ggdid(att_gdp)
aggte(att_gdp)
att_gdp

att_nl <- att_gt(yname = "night_light_log", tname = "year", idname = "County_Code",
                 gname = "First_Treated", xformla = ~ non_param, merged_new,
                 control_group = c("nevertreated"), bstrap = TRUE)
figure_1_2 <- ggdid(att_nl)
aggte(att_nl)
att_nl


# Effect in distribution.
# Baseline
model_2_3_1 <- lm(nl_10_ratio ~ Poverty + Post + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_3_1)
model_2_3_2 <- lm(nl_10_ratio ~ Poverty + Post + Phase_Out + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_3_2)
model_2_4_1 <- lm(nl_20_ratio ~ Poverty + Post + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_4_1)
model_2_4_2 <- lm(nl_20_ratio ~ Poverty + Post + Phase_Out + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_4_2)
model_2_5_1 <- lm(nl_50_ratio ~ Poverty + Post + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_5_1)
model_2_5_2 <- lm(nl_50_ratio ~ Poverty + Post + Phase_Out + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_5_2)
model_2_6_1 <- lm(Gini ~ Poverty + Post + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_6_1)
model_2_6_2 <- lm(Gini ~ Poverty + Post + Phase_Out + DID + factor(Year) + factor(County_Code), merged)
summary(model_2_6_2)
# DR-DID
# 10 fraction
att_nl_10 <- att_gt(yname = "nl_10_ratio", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ non_param, merged_new,
                    control_group = c("nevertreated"),bstrap = FALSE)
aggte(att_nl_10)
att_nl_10
# 20 fraction
att_nl_20 <- att_gt(yname = "nl_20_ratio", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ non_param, merged_new,
                    control_group = c("nevertreated"), bstrap = FALSE)
aggte(att_nl_20)
att_nl_20
# 50 fraction
att_nl_50 <- att_gt(yname = "nl_50_ratio", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ non_param, merged_new,
                    control_group = c("nevertreated"), bstrap = FALSE)
aggte(att_nl_50)
att_nl_50
# Overall gini effect.
att_gini <- att_gt(yname = "Gini", tname = "year", idname = "County_Code",
                   gname = "First_Treated", xformla = ~ non_param, merged_new,
                   control_group = c("nevertreated"), bstrap = FALSE)
aggte(att_gini)
att_gini

ggsave("Program_Effect_GDP.png", plot = figure_1_1, width = 10, height = 6, dpi = 800)
ggsave("Program_Effect_NL.png", plot = figure_1_2, width = 10, height = 6, dpi = 800)

