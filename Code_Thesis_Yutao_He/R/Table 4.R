# Clear the workspace.
rm(list=ls())
# Set the working directory.
setwd("C:/Users/yutao/OneDrive/Desktop/GradSchool/2023 Summer/Research Project/Regression_Data")
# Load the data.
load(file = "Data_Baseline.RData")

merged_new$local_budget_log <- log(merged_new$local_budget)
merged_new$local_expend_log <- log(merged_new$local_expend)
merged_new$mortgage_log <- log(merged_new$local_mort)
merged_new$saving_log <- log(merged_new$local_save)

# Effects in other variables.
variable_list <- c("local_budget_log", "local_expend_log", "mortgage_log", 
                   "saving_log", "elementary_ratio", "high_school_ratio", 
                   "landline_ratio", "hospital_bed", "social_security")
for (i in 1:length(variable_list)){
  outcome <- variable_list[i]
  print(outcome)
  subset_data <- merged[merged[outcome]!= log(0),]
  #formula_did <- paste(outcome,
  #                     "~ Poverty + Post + DID + factor(year) + factor(name)",
  #                     sep="")
  #model <- lm(formula_did, data = subset_data)
  #var_name <- paste("model_3_", as.character(i), sep = "")
  #assign(var_name, model)
  model <- att_gt(yname = outcome, tname = "Year",idname = "County_Code",
                  gname = "First_Treated", xformla = ~ phase_in_non_param, subset_data,
                  control_group = c("nevertreated"), bstrap = FALSE)
  var_name <- paste("att_other_", as.character(i), sep = "")
  assign(var_name, model)
  rm(subset_data, model, var_name)
}
aggte(att_other_9)