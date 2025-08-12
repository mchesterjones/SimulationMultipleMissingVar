################################################################################
# 02_GenerateData_Run01Functions.R
################################################################################
# Created: 11Jun2024
# Aim: Run functions from 01
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)

################################################################################
################################################################################
#Load development_dataset
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\Development Datasets")
load("DevData_MAR_Yprev_0.1.Rdata")
development_dataset[["refmodel"]][["reference_values"]]
submodel <- development_dataset[["submodel"]]
refmodel <- development_dataset[["refmodel"]]
xgboost_model <- development_dataset[["xgmodel"]]
model <- development_dataset[["model"]]
rimodel <- development_dataset[["rimodel"]]



################################################################################
# Dependent Code
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 3//01_Functions_GenerateData_MAR.R") 
################################################################################
# Simulation Parameters
################################################################################


sims_parameters <- crossing(
            n_iter = 2, 
           # N_val = c(500, 10000, 100000),
          N_val = 10000,
            # Y_prev = c(0.01,0.05, 0.1), 
            Y_prev = c(0.1), 
           # R_prev = c(0.25,0.50,0.75), 
          R_prev = c(0.75),
            # Gamma = affect on Y
            gamma_x1 = c(0.5), 
            gamma_x2 = c(0.5), 
            gamma_x3 = c(0.5),
            gamma_x4 = c(0.5), 
            gamma_x5 = c(0.5), 
            gamma_U = c(0.5) 
  
)
  


###############################################################################
# Run through combinations 
###############################################################################
for (i in 1:nrow(sims_parameters)) {
  # Extract parameters for the current iteration
  n_iter <- sims_parameters$n_iter[i]
  N_val <- sims_parameters$N_val[i]
  Y_prev <- sims_parameters$Y_prev[i]
  R_prev <- sims_parameters$R_prev[i]
  gamma_x1 <- sims_parameters$gamma_x1[i]
  gamma_x2 <- sims_parameters$gamma_x2[i]
  gamma_x3 <- sims_parameters$gamma_x3[i]
  gamma_x4 <- sims_parameters$gamma_x4[i]
  gamma_x5 <- sims_parameters$gamma_x5[i]
  gamma_U <- sims_parameters$gamma_U[i]
  
  # Select the appropriate model based on Y_prev
  # Select the appropriate model based on missing data mechanism
  
 
################################################################################
# Store Simulation Results
################################################################################

s <-1

## Simulation Results
simulation_results <- simulation_nrun_fnc(
  n_iter = n_iter,
  N_val = N_val,
  Y_prev = Y_prev,
  R_prev = R_prev,
  gamma_x1 = gamma_x1,
  gamma_x2 = gamma_x2,
  gamma_x3 = gamma_x3,
  gamma_x4 = gamma_x4,
  gamma_x5 = gamma_x5, 
  gamma_U = gamma_U, 
  submodel=submodel,
  refmodel = refmodel,
  xgboost_model = xgboost_model,
  rimodel=rimodel,
  model=model)


# Get today's date Y Prev and R Previn a format suitable for filenames
today <- format(Sys.Date(), "%d%b%Y")  

# Construct the filename with today's date
filename <- paste0("ValData_", sims_parameters$label[i], "_Nval_", sims_parameters$N_val[i], "_Yprev_", sims_parameters$Y_prev[i], "_Rprev_", sims_parameters$R_prev[i], ".Rdata")
# Save results
#setwd("C:\\Users\\maecj\\Documents\\Simulation_Data")
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\Validation Datasets")
#save(simulation_results, file = filename)

}

warnings()


