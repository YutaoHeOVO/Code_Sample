# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

# Heterogeneity Elasticity
plot_data_2 <- merged[c("log_gdp","night_light_log", 
                        "Province", "County_Code")] 
figure_2_5 <- ggplot()+
  geom_point(data = plot_data_2[plot_data_2$Province == 64,], 
             aes(x = night_light_log, y = log_gdp, color = as.factor(County_Code)))

heterogenous_result <- c()
for (province_id in unique(merged$Province)){
  model_fit <- lm(log_gdp~night_light_log + night_light_squared + factor(Year)+
                    factor(County_Code), 
                  data = merged, 
                  subset = merged$Province == province_id)
  coef <- model_fit$coefficients[2]
  poverty_ratio <- sum(merged[merged$year == 2012 & 
                                merged$Province == province_id,]$Poverty)/dim(merged[merged$year==2012 & merged$Province == province_id,])[1]
  number_obs <- length(unique(merged[merged$Province == province_id,]$name))
  result <- cbind(province_id, number_obs, coef, poverty_ratio)
  heterogenous_result <- rbind(heterogenous_result, result)
}

ggsave("Heterogeneous_Elasticity.png", plot = figure_2_5, width = 8, height = 8, dpi = 800)