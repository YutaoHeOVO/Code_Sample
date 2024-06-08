# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

## Anticipation of treatment
att_gdp_a1<- att_gt(yname = "log_gdp", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ phase_in_logit, merged,
                    control_group = c("nevertreated"), bstrap = FALSE,
                    anticipation = 1)
figure_4_1 <- ggdid(att_gdp_a1)
aggte(att_gdp_a1)
att_gdp_a1

att_nl_a1 <- att_gt(yname = "night_light_log", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ phase_in_logit, merged,
                    control_group = c("nevertreated"), bstrap = FALSE,
                    anticipation = 1)
figure_4_2 <- ggdid(att_nl_a1)
aggte(att_nl_a1)

att_gdp_a2<- att_gt(yname = "log_gdp", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ phase_in_logit, merged,
                    control_group = c("nevertreated"), bstrap = FALSE,
                    anticipation = 2)
figure_4_3 <- ggdid(att_gdp_a2)
aggte(att_gdp_a2)
att_gdp_a2

att_nl_a2 <- att_gt(yname = "night_light_log", tname = "year", idname = "County_Code",
                    gname = "First_Treated", xformla = ~ phase_in_logit, merged,
                    control_group = c("nevertreated"), bstrap = FALSE,
                    anticipation = 2)
figure_4_4 <- ggdid(att_nl_a2)
aggte(att_nl_a2)
ggsave("DRDID_GDP_a1.png", plot = figure_4_1, width = 10, height = 6, dpi = 800)
ggsave("DRDID_NL_a1.png", plot = figure_4_2, width = 10, height = 6, dpi = 800)
ggsave("DRDID_GDP_a2.png", plot = figure_4_3, width = 10, height = 6, dpi = 800)
ggsave("DRDID_NL_a2.png", plot = figure_4_4, width = 10, height = 6, dpi = 800)
