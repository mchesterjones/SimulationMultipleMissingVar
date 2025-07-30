################################################################################
# 02_Run01Functions_MNAR.R
################################################################################
# Created: Modified for parallel processing
# Aim: Run functions from 01 with parallel processing
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)
library(parallel)
library(foreach)
library(doParallel)
library(MASS)  # For mvrnorm function
library(xgboost)  # For XGBoost functions

################################################################################
# Setup parallel processing
################################################################################
# Use 6 cores (leave 1 for system)
cl <- makeCluster(6)
registerDoParallel(cl)

# Export necessary objects to all cores
clusterEvalQ(cl, {
  library(dplyr)
  library(tidyverse)
  library(mice)
  library(broom)
  library(pROC)
  library(MASS)  # Important: Load MASS for mvrnorm
  library(xgboost)  # Important: Load xgboost for XGBoost functions
})

################################################################################
# Load development datasets
################################################################################
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\Development Datasets")

load("DevData_MNAR_Yprev_0.1.Rdata")
submodel <- development_dataset[["submodel"]]
refmodel <- development_dataset[["refmodel"]]
xgboost_model <- development_dataset[["xgmodel"]]  # Fixed: renamed to match function call
model <- development_dataset[["model"]]
rimodel <- development_dataset[["rimodel"]]

################################################################################
# Load and export the simulation functions
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 3//01_Functions_GenerateData_MNAR.R")

# Export ALL necessary functions and objects to clusters
clusterExport(cl, c("submodel", "refmodel", "xgboost_model", "model", "rimodel",
                    "simulation_nrun_fnc", "simulation_singlerun_fnc", 
                    "simulation_function", "prediction_function", 
                    "predictive.performance.function", "expit_function"))

################################################################################
# Simulation Parameters
################################################################################
sims_parameters <- crossing(
  n_iter = 200,  # Changed to 200 iterations
  N_val = c(10000),
  Y_prev = c(0.1), 
  R_prev = c(0.25, 0.5, 0.75), 
  gamma_x1 = c(0.5), 
  gamma_x2 = c(0.5), 
  gamma_x3 = c(0.5),
  gamma_x4 = c(0.5), 
  gamma_x5 = c(0.5), 
  gamma_U = c(0.5) 
) %>%
  mutate(scenario_id = row_number())  # Add ID for tracking

################################################################################
# PARALLEL EXECUTION WITH ERROR HANDLING
################################################################################
results_list <- foreach(i = 1:nrow(sims_parameters), 
                        .packages = c("dplyr", "tidyverse", "mice", "broom", "pROC", "MASS", "xgboost"),
                        .errorhandling = "pass") %dopar% {
                          
                          # Extract parameters
                          params <- sims_parameters[i, ]
                          
                          # Add error handling
                          tryCatch({
                            # Run simulation - FIXED: Added rimodel parameter
                            simulation_results <- simulation_nrun_fnc(
                              n_iter = params$n_iter,
                              N_val = params$N_val,
                              Y_prev = params$Y_prev,
                              R_prev = params$R_prev,
                              gamma_x1 = params$gamma_x1,
                              gamma_x2 = params$gamma_x2,
                              gamma_x3 = params$gamma_x3,
                              gamma_x4 = params$gamma_x4,
                              gamma_x5 = params$gamma_x5, 
                              gamma_U = params$gamma_U, 
                              submodel = submodel,
                              refmodel = refmodel,
                              xgboost_model = xgboost_model, 
                              rimodel = rimodel,
                              model = model
                            )
                            
                            # Return results with parameter info
                            list(parameters = params, results = simulation_results, error = NULL)
                            
                          }, error = function(e) {
                            # Return error information
                            list(parameters = params, results = NULL, 
                                 error = paste("Scenario", i, "failed:", e$message))
                          })
                        }

################################################################################
# SAVE RESULTS WITH ERROR CHECKING
################################################################################
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\Validation Datasets")

# Track successful and failed scenarios
successful_scenarios <- 0
failed_scenarios <- 0

for(j in 1:length(results_list)) {
  result <- results_list[[j]]
  
  # Check for errors
  if(!is.null(result$error)) {
    cat("ERROR:", result$error, "\n")
    failed_scenarios <- failed_scenarios + 1
    next
  }
  
  # Save successful results
  params <- result$parameters
  simulation_results <- result$results
  
  filename <- paste0("ValData_MNAR_Nval_", params$N_val, 
                     "_Yprev_", params$Y_prev, "_Rprev_", params$R_prev, ".Rdata")
  
  tryCatch({
    save(simulation_results, file = filename)
    cat("Saved:", filename, "\n")
    successful_scenarios <- successful_scenarios + 1
  }, error = function(e) {
    cat("Failed to save", filename, ":", e$message, "\n")
    failed_scenarios <- failed_scenarios + 1
  })
}

################################################################################
# Clean up and summary
################################################################################
stopCluster(cl)

print(paste("Simulation completed!"))
print(paste("Successful scenarios:", successful_scenarios))
print(paste("Failed scenarios:", failed_scenarios))
print(paste("Total scenarios:", length(results_list)))

# Show any warnings
if(length(warnings()) > 0) {
  print("Warnings:")
  print(warnings())
}