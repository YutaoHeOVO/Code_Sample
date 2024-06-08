# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

# Probability
library("ggplot2")
subset_2012 <- merged[merged$year==2012,]
plot_data <- subset_2012[c("log_per_capita","night_light_log","phase_in_logit", 
                           "Province", "County_Code")]
figure_2_1 <- ggplot()+
  geom_point(data = plot_data,aes(x = log_per_capita, y = phase_in_logit))

subset_2015 <- merged[merged$year==2015,]
plot_data <- subset_2015[c("log_per_capita","night_light_log","phase_in_logit", 
                           "Province", "County_Code")]
figure_2_2 <- ggplot()+
  geom_point(data = plot_data,aes(x = log_per_capita, y = phase_in_logit))

subset_2018 <- merged[merged$year==2018,]
plot_data <- subset_2018[c("log_per_capita","night_light_log","phase_in_logit", 
                           "Province", "County_Code")]
figure_2_3 <- ggplot()+
  geom_point(data = plot_data,aes(x = log_per_capita, y = phase_in_logit))

subset_2021 <- merged[merged$year==2021,]
plot_data <- subset_2021[c("log_per_capita","night_light_log","phase_in_logit", 
                           "Province", "County_Code")]
figure_2_4 <- ggplot()+
  geom_point(data = plot_data,aes(x = log_per_capita, y = phase_in_logit))

ggsave("Logit_2012.png", plot = figure_2_1, width = 8, height = 8, dpi = 800)
ggsave("Logit_2015.png", plot = figure_2_2, width = 8, height = 8, dpi = 800)
ggsave("Logit_2018.png", plot = figure_2_3, width = 8, height = 8, dpi = 800)
ggsave("Logit_2021.png", plot = figure_2_4, width = 8, height = 8, dpi = 800)