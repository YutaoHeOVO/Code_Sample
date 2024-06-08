# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

library("grf")

## Endogenoue phasing-out.
balanced$phase_out_2016 <- predict(phase_out_nonparam_2016, as.matrix(balanced[, controls]))
#balanced$phase_out_2016 <- predict(phase_out_nonparam_2016, balanced, type = "response")
balanced$phase_out_2017 <- predict(phase_out_nonparam_2017, as.matrix(balanced[, controls]))
#balanced$phase_out_2017 <- predict(phase_out_nonparam_2017, balanced, type = "response")
balanced$phase_out_2018 <- predict(phase_out_nonparam_2018, as.matrix(balanced[, controls]))
#balanced$phase_out_2018 <- predict(phase_out_nonparam_2018, balanced, type = "response")
balanced$phase_out_2019 <- predict(phase_out_nonparam_2019, as.matrix(balanced[, controls]))
#balanced$phase_out_2019 <- predict(phase_out_nonparam_2019, balanced, type = "response")
# Create the panel data.
endo_phase_out <- c()
for (year in 2016:2019){
  var_name <- paste("phase_out_",as.character(year),sep="")
  balanced$dummy <- as.numeric(year > 2017)
  balanced$prob <- year
  data_subset <- balanced[c("name","year","dummy","prob",var_name,"Poverty","Phase_Out_Year")]
  names(data_subset) <- c("name", "year", "dummy", "mod_year", "prob","Poverty","Phase_Out_Year")
  endo_phase_out <- rbind(endo_phase_out, data_subset)
}
names(endo_phase_out) <- c("name", "year", "dummy", "mod_year", "prob", "Poverty", "Phase_Out_Year")
endo_phase_out[endo_phase_out$year > 2017,]$dummy <- 1

model_6_1 <- lm(prob$predictions~dummy + factor(name) + factor(year),
                endo_phase_out)
summary(model_6_1)
model_6_2 <- lm(prob$predictions~dummy + factor(name) + factor(year),
                endo_phase_out[endo_phase_out$Poverty == 1,])
summary(model_6_2)
model_6_3 <- lm(prob$predictions~dummy + factor(name) + factor(year),
                endo_phase_out[endo_phase_out$Poverty == 0,])
summary(model_6_3)
endo_phase_out$dummy_2017 <- as.numeric(endo_phase_out$mod_year == 2017)
endo_phase_out$dummy_2018 <- as.numeric(endo_phase_out$mod_year == 2018)
endo_phase_out$dummy_2019 <- as.numeric(endo_phase_out$mod_year == 2019)

model_6_4 <- lm(prob$predictions ~ dummy_2017 + factor(name) + factor(year),
                endo_phase_out[endo_phase_out$dummy == 0,])
summary(model_6_4)
model_6_5 <- lm(prob$predictions ~ dummy_2019 + factor(name) + factor(year),
                endo_phase_out[endo_phase_out$dummy == 1,])
summary(model_6_5)

t.test(endo_phase_out[endo_phase_out$dummy == 0 & endo_phase_out$dummy_2017 == 1& endo_phase_out$Poverty  == 1,]$prob,
       endo_phase_out[endo_phase_out$dummy == 0 & endo_phase_out$dummy_2017 == 0& endo_phase_out$Poverty  == 1,]$prob)

t.test(endo_phase_out[endo_phase_out$dummy == 1 & endo_phase_out$dummy_2019 == 1& endo_phase_out$Poverty  == 1,]$prob,
       endo_phase_out[endo_phase_out$dummy == 1 & endo_phase_out$dummy_2019 == 0& endo_phase_out$Poverty  == 1,]$prob)

t.test(endo_phase_out[endo_phase_out$dummy_2017 == 1 & endo_phase_out$Poverty  == 1,]$prob,
       endo_phase_out[endo_phase_out$dummy_2018 == 1 & endo_phase_out$Poverty  == 1,]$prob)
