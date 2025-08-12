################################################################################
# Combine datasets 
################################################################################
# libraryies
library(tidyverse)
################################################################################
setwd("C:\\Users\\maecj\\Documents\\Simulation Data Study 3")

datasets <- list()
# Define parameter combinations
mechanisms <- c("MCAR", "MAR", "MNAR")
samplesizes <- c("500","10000", "1e+05")
missing <-c("0.25","0.5","0.75")

# Load and store in list
for (mech in mechanisms) {
  for (ss in samplesizes) {
    for (miss in missing) {
      fname <- paste0("ValData_", mech,"_Nval_", ss, "_Yprev_0.1_Rprev_", miss, ".Rdata")
      load(fname)  # loads simulation_results
      key <- paste0(mech, "_", ss, "_Rprev", miss)
      datasets[[key]] <- simulation_results
    }
  }
}

## Define the number of iterations NOTE MANUAL
num_iterations <- 100
methods <- c("Prediction_SubModel", "Prediction_Ref", "Prediction_XG", "Predictions_CRI", "Prediction_RI")




################################################################################
## For each Dataset compare X1 and X3
################################################################################




################################################################################
## Extract Target Measures  into one dataset
################################################################################
# Initialize empty list to collect target measures
target_measures_all <- list()

for (dataname in names(datasets)) {
  sim_result <- datasets[[dataname]]
  
  for (i in 1:num_iterations) {
    df <- sim_result$iterations[[i]]$target_measures
    
    df <- cbind(
      iteration = i,
      dataset = dataname,
      df  # <- this already includes a "model" column
    )
    
    target_measures_all[[length(target_measures_all) + 1]] <- df
  }
}

# Combine into a single data frame
target_measures_df <- do.call(rbind, target_measures_all)




################################################################################
## Extract bias predictions into one dataset
################################################################################
# Initialize bias summary list
bias_summaries <- list()


for (dataname in names(datasets)) {
  sim_result <- datasets[[dataname]]
  
  for (method_id in methods) {
    
    for (i in 1:num_iterations) {
      predictions <- sim_result$iterations[[i]]$predictions
      
      true_Y <- predictions$Y
      estimated_Y <- predictions[[method_id]]
      
      # Number of observations
      n <- length(true_Y)
      
      # Bias, MSE, RMSE
      bias <- mean(true_Y - estimated_Y)
      mse  <- mean((true_Y - estimated_Y)^2)
      rmse <- sqrt(mse)
      
      # Create a row of results
      new_row <- data.frame(
        iteration = i,
        dataset = dataname,
        method = method_id,
        n = n,
        bias = bias,
        mse = mse,
        rmse = rmse
      )
      
      # Add to output list
      bias_summaries[[length(bias_summaries) + 1]] <- new_row
    }
  }
}

# Combine all rows into a single data frame
bias_summary_df <- do.call(rbind, bias_summaries)


### JOIN BIAS AND TARGET
combined_df <- full_join(target_measures_df, bias_summary_df, by = c("iteration" = "iteration", 
                                                                     "Model" = "method",
                                                                     "dataset"="dataset"))
combined_df <- combined_df %>%
  mutate(Method = factor(case_when(
    Model == "Prediction_RI" ~"Regression Imputation",
    Model == "Prediction_SubModel" ~"Sub Model", 
    Model == "Prediction_Ref" ~"Reference Values", 
    Model == "Prediction_XG" ~ "XGBoost",
    Model == "Predictions_CRI" ~"Completely Recorded Information")),
    Missingness = factor(case_when(
      grepl("0.25", dataset) ~ "25%",
      grepl("0.5", dataset) ~ "50%",
      grepl("0.75", dataset) ~ "75%")), 
    samplesize =factor(case_when(
      grepl("500", dataset) ~ "N=500",
      grepl("10000", dataset) ~ "N=10,000",
      grepl("1e", dataset) ~ "N=100,000")),
    Mechanism = factor(case_when(
      grepl("MCAR", dataset) ~ "MCAR",
      grepl("MAR", dataset) ~ "MAR",
      grepl("MNAR", dataset) ~ "MNAR")))




################################################################################
## This next part of the code keeps combines the average 
###############################################################################

# Function to summarize the data
summarised_df <-   combined_df %>%
  group_by(Model, Missingness, samplesize, Mechanism) %>%
  summarise(across(c(Cal_Int, Cal_Slope, AUC, Brier, Brier_scaled, bias, mse, rmse), 
                   list(AVG = ~ mean(.x, na.rm = TRUE),  
                        LCI = ~ quantile(.x, 0.025, na.rm = TRUE), 
                        UCI = ~ quantile(.x, 0.975, na.rm = TRUE), 
                        NACount = ~ sum(is.na(.x)))))

# Convert to long format
summarised_df_long <- summarised_df %>%
  pivot_longer(cols = ends_with(c("AVG", "LCI", "UCI", "NACount")),
               names_to = c("Measure", ".value"), 
               names_pattern = "^(.*)_(AVG|LCI|UCI|NACount)$")


## Create variables as factors
summarised_df_long <- summarised_df_long %>%
  mutate(Method = factor(case_when(
    Model == "Prediction_RI" ~"Regression Imputation",
    Model == "Prediction_SubModel" ~"Sub Model", 
    Model == "Prediction_Ref" ~"Reference Values", 
    Model == "Prediction_XG" ~ "XGBoost",
    Model == "Predictions_CRI" ~"Completely Recorded Information")))

summarised_df_long <- summarised_df_long %>% 
  mutate(Measure =factor(Measure,
                         levels = c("AUC", "Brier_scaled", "Brier",
                                    "bias", "mse",
                                    "Cal_Int", "Cal_Slope", "rmse")))

setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\Validation Datasets")
save(summarised_df_long,file= "LongFormat.Rdata")
save(combined_df,file= "AllIter.Rdata")
