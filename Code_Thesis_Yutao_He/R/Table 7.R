# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

## Phasing-out effect.
att_gdp_phase_out <- att_gt(yname = "log_gdp", tname = "year", idname = "County_Code",
                            gname = "First_Phase_Out", xformla = ~ phase_in_logit, 
                            merged[merged$Poverty == 1,],
                            control_group = c("notyettreated"), bstrap = TRUE)
aggte(att_gdp_phase_out)
figure_3_1 <- ggdid(att_gdp_phase_out)
att_nl_phase_out <- att_gt(yname = "night_light_log", tname = "year", idname = "County_Code",
                           gname = "First_Phase_Out", xformla = ~ phase_in_logit, 
                           merged[merged$Poverty == 1,],
                           control_group = c("notyettreated"), bstrap = TRUE)
aggte(att_nl_phase_out)
figure_3_2 <- ggdid(att_nl_phase_out)

ggsave("Phasing_Out_GDP.png", plot = figure_3_1, width = 8, height = 12, dpi = 800)
ggsave("Phasing_Out_NL.png", plot = figure_3_2, width = 8, height = 12, dpi = 800)
