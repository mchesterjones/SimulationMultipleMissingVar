################################################################################
# 00_SimulateDevelopmentData.R
################################################################################
# Created: 29Jul2025
# Aim: Simulate Development Data and Build MOdels
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(broom)
library(pROC)
library(MASS) 
library(xgboost)
library(mice)

###############################################################################
## Functions for running iterations
################################################################################

set.seed(132) # Note New Seed

sims_parameters <- crossing(
  N_dev = 100000,
  Y_prev = c(0.1), ## Set Yprev as 10% for now
  # Gamma = affect on Y
  gamma_x1 = c(0.5), 
  gamma_x2 = c(0.5), 
  gamma_x3 = c(0.5),
  gamma_x4 = c(0.5), 
  gamma_x5 = c(0.5), 
  gamma_U = c(0.5) 
  
)



###############################################################################
## 0. Function that takes the inverse logistic link
###############################################################################

## Function to extract the OR from the coeffiction
expit_function <- function(x) {1/(1+exp(-x))} ## Recal ex/1+ex this is the same to get the P(Y=1)

###############################################################################
## 1. Function that creates development dataset
###############################################################################


dev_data_simulation_function <- function(
    N_dev,
    Y_prev,
    gamma_x1,
    gamma_x2,
    gamma_x3,
    gamma_x4,
    gamma_x5,
    gamma_U) {
  

  # 1. Create relationship between x_1 and x_4 and x_5 
  dev_data_IPD <- as.data.frame(MASS::mvrnorm(n = N_dev, 
                              mu = c(0, 0, 0,0,0,0 ), 
                              Sigma = matrix(data = c(1.0, 0.2, 0.2, 0.7, 0.7, 0,
                                                      0.2, 1, 0.2, 0.2, 0.2, 0,
                                                      0.2,0.2,  1, 0.2, 0.2, 0, 
                                                      0.7, 0.2, 0.2, 1.0, 0.5, 0, 
                                                      0.7, 0.2, 0.2, 0.5, 1.0, 0, 
                                                      0, 0, 0, 0, 0, 1),
                                             nrow = 6, 
                                             byrow = T)))
  
  colnames(dev_data_IPD) <- c("x_1", "x_2", "x_3", "x_4", "x_5", "U")
  dev_data_IPD <- dev_data_IPD %>%
    mutate(x_5 = rbinom(n = N_dev, size = 1, prob = plogis(x_5)))

  #2. Remove X1 and X3 
  myfreq <- c(0.25, 0, 0, 0,0,0)
  pattern <- matrix(c(1, 1, 1, 1, 1, 1,
                      0, 1, 1, 1, 1, 1, 
                      0, 1, 0, 1, 1, 1,
                      1, 1, 0, 1, 1, 1), 
                    nrow = 4, byrow = TRUE)
  
  # MANUAL SWITCH
  ##---------------------------
  # MCAR
  data_miss <- ampute(dev_data_IPD, patterns = pattern, mech = "MCAR")
  head(data_miss$amp)
  md.pattern(data_miss$amp)
  
  
  # 
  #
  # ## MAR
  # #--------------------------------------------------------------------
  # mar_weights <- matrix(0, nrow = 4, ncol = 6)
  # mar_weights[2, 2] <- 0.5  # Pattern 2: V2 → V1
  # mar_weights[3, 2] <- 0.5  # Pattern 3: V2 → V1 & V3
  # mar_weights[4, 2] <- 0.5  # Pattern 4: V2 → V3
  # 
  # data_miss <- ampute(dev_data_IPD, patterns = pattern, mech="MAR", weights=mar_weights)
  # head(data_miss$amp)
  # md.pattern(data_miss$amp)
  # 
  # 
  # ## MNAR 
  # mnar_weights <- matrix(0, nrow = 4, ncol = 6)
  # mnar_weights[2, 1] <- 0.5  #  V1 → V1
  # mnar_weights[2, 2] <- 0.5  #  V2 → V1
  # mnar_weights[2, 6] <- 0.5  #  V6 (U) → V1
  # 
  # mnar_weights[3, 1] <- 0.5  #  V1 → V1 & V3
  # mnar_weights[3, 2] <- 0.5  #  V2 → V1 & V3
  # mnar_weights[3, 6] <- 0.5  #  V6 → V1 & V3
  # 
  # mnar_weights[4, 1] <- 0.5  #  V1 → V3
  # mnar_weights[4, 2] <- 0.5  #  V2 → V3
  # mnar_weights[4, 6] <- 0.5  #  V6 → V3
  # 
  # data_miss <- ampute(dev_data_IPD, patterns = pattern, mech="MNAR", weights=mnar_weights)
  # head(data_miss$amp)
  # md.pattern(data_miss$amp)
  # 
  
  
  #3. Determine the prevalence of the outcome based on the gamma_0
  gamma_0 <- as.numeric(coef(glm(rbinom(N_dev, 1, prob = Y_prev) ~
                                   offset(gamma_x1*x_1 +
                                            gamma_x2*x_2 +
                                            gamma_x3*x_3 +
                                            gamma_x4*x_4 +
                                            gamma_x5*x_5),
## NOTE: REMOVED GAMMA U
                                  family = binomial(link = "logit"),
                                  data = dev_data_IPD))[1])
  
  dev_data_IPD$Y = rbinom(N_dev, size = 1,
                          prob = expit_function(gamma_0 +
                                                  gamma_x1*dev_data_IPD$x_1 +
                                                  gamma_x2*dev_data_IPD$x_2 +
                                                  gamma_x3*dev_data_IPD$x_3 +
                                                  gamma_x4*dev_data_IPD$x_4 +
                                                  gamma_x5*dev_data_IPD$x_5 + 
                                                  gamma_U*dev_data_IPD$U))
  
  
  # 4. Combine with non missing
  colnames(dev_data_IPD) <- c("x_1true", "x_2", "x_3true", "x_4", "x_5", "U","Y")
  dev_data <- cbind(dev_data_IPD, data_miss[["amp"]][,c("x_1","x_3")] )
 
  # 5. Create group variable of X_5 and X_4 for reference values 
  dev_data <- dev_data %>% mutate(x_4group = case_when(x_4<=0 ~ "Group 1", 
                                                       x_4 >0 ~ "Group 2"))
  
  # 6. Mark patterns 
  dev_data <- dev_data %>%
            mutate(pattern = case_when(!is.na(x_1) & !is.na(x_3)  ~ 0,
                                       is.na(x_1) & !is.na(x_3)  ~ 1,
                                       is.na(x_1) & is.na(x_3)  ~ 2,
                                       !is.na(x_1) & is.na(x_3) ~ 3))
  
  
  print("Y Prev")
  print(table(dev_data$Y))
  print(prop.table(table(dev_data$Y)) * 100)
  
  print(prop.table(table(dev_data$pattern)) * 100)
  
  print(prop.table(table(is.na(dev_data$x_1))) * 100)
  print(prop.table(table(is.na(dev_data$x_3))) * 100)
  
  
  
  
  return(dev_data)
}

##############################################################################
## MODEL BUILDING FUNCTIONS
##############################################################################


####---------------------------------------
## 1. Completely Recorded Information 
####---------------------------------------
complete_mod_function <- function(dev_data) {
  
  
  # Create model
  model <- glm(Y~ x_1true + x_2 + x_3true + x_4 + x_5, data=dev_data, family=binomial(link="logit"))
  
  #  Return the  coefficients and intercept
  return(model)
}

###--------------------------------------------------  
# 2. Reference Values 
###--------------------------------------------------  
ref_mod_function <- function(dev_data) {
  
  ## Reference values 
  reference_values <- dev_data %>%   
    group_by(x_5, x_4group) %>% summarise(
      mean_x1 = mean(x_1, na.rm = TRUE),
      mean_x3 = mean(x_3, na.rm=TRUE),
      .groups = "drop")
  
  dev_data <- dev_data %>%
    left_join(reference_values, by = c("x_5", "x_4group")) 
  
  dev_data <- dev_data %>%
    mutate(x1_refimp = coalesce(x_1, mean_x1),
           x3_refimp = coalesce(x_3, mean_x3))
  
  # Create model
  model_ref <- glm(Y~ x1_refimp + x_2 + x3_refimp + x_4 + x_5, data=dev_data , family=binomial(link="logit"))
  
  #  Return the  coefficients and intercept
  return(list(
    model_ref = model_ref,
    reference_values = reference_values
  ))
}

####---------------------------------------
# 3. Pattern Sub model as per Mercaldo Paper: https://academic.oup.com/biostatistics/article/21/2/236/5092384
####---------------------------------------

sub_mod_function <- function(dev_data) {
  
  ## No U included 
  data_pattern_allobserved <- dev_data %>% filter(pattern==0)
  data_pattern_x1missing <- dev_data %>% filter(pattern==1)
  data_pattern_x1x3missing <- dev_data %>% filter(pattern==2)
  data_pattern_x3missing <- dev_data %>% filter(pattern==3)
  
  model_allobserved <- glm(Y ~ x_1+x_2+x_3+x_4+x_5, data=data_pattern_allobserved, family=binomial(link="logit")) 
  model_x1missing <- glm(Y ~ x_2+x_3+x_4+x_5, data=data_pattern_x1missing, family=binomial(link="logit")) 
  model_x1x3missing <- glm(Y ~ x_2+x_4+ x_5, data=data_pattern_x1x3missing, family=binomial(link="logit")) 
  model_x3missing <- glm(Y ~ x_1+ x_2+ x_4+ x_5, data=data_pattern_x3missing, family=binomial(link="logit")) 
  
  
  
  #  Return the submodel coefficients and intercept
  return(list(
    model_allobserved = model_allobserved,
    model_x1missing = model_x1missing,
    model_x1x3missing = model_x1x3missing,
    model_x3missing = model_x3missing
    
    
  ))
}


####---------------------------------------
## 4. XG Boost  Model fitting function
####---------------------------------------

xg_function  <- function(dev_data) {
  
  ## Convert to matrix 
  xg_train_matrix <- as.matrix(dev_data[, c("x_1", "x_2", "x_3", "x_4", "x_5")])
  xg_train_label <- dev_data[["Y"]]
  
  ## xgb.DMatrix optimised data structure XGBoost uses 
  xgtrain <- xgb.DMatrix(data = xg_train_matrix, label = xg_train_label, missing = NA)
  
  
  params <- list(
    objective = "binary:logistic",  # because your outcome Y is binary (0/1)
    eval_metric = "auc",            # a good overall metric for classification performance, especially with imbalanced data
    max_depth = 4,                  # limits complexity; prevents overfitting
    eta = 0.1                       # learning rate: small enough to allow steady convergence
  )
  
  
  xgmodel <- xgb.train(
    params = params,
    data = xgtrain,
    nrounds = 100,
    verbose = 1)
  
  return(xgmodel)
  
}


####---------------------------------------
## 5.Regression Imputation 
####---------------------------------------

ri_function  <- function(dev_data) {
  
  ## Create model to predict x_1
  x1_ri_model  <- lm(x_1  ~ x_2 + x_3 + x_4 + x_5, data = dev_data)
  x1x3_ri_model  <- lm(x_1  ~ x_2 + x_4 + x_5, data = dev_data)
  x3_ri_model  <- lm(x_3  ~ x_1 + x_2 + x_4 + x_5, data = dev_data)
  x3x1_ri_model  <- lm(x_3  ~ x_2 + x_4 + x_5, data = dev_data) # No x_1
  
  ## Impute the missing values of x_1
  dev_data <- dev_data %>% 
    mutate(x_1_ri = case_when(is.na(x_1) & !is.na(x_3) ~ predict(x1_ri_model, newdata=dev_data, type="response"), 
                              is.na(x_1) & is.na(x_3) ~ predict(x1x3_ri_model, newdata=dev_data, type="response"),
                              !is.na(x_1) ~ x_1),
           x_3_ri = case_when(
                              is.na(x_3) & !is.na(x_1) ~ predict(x3_ri_model, newdata=dev_data, type ="response"), 
                              is.na(x_3) & is.na(x_1) ~ predict(x3x1_ri_model, newdata=dev_data, type="response"),
                              !is.na(x_3) ~ x_3))
  
  ## Build Model  
  model_ri <- glm(Y ~ x_1_ri +x_2+ x_3_ri + x_4 + x_5, data=dev_data, family=binomial(link="logit")) 
  
  
  
  
  return(list(x1_ri_model=x1_ri_model,
              x1x3_ri_model = x1x3_ri_model,
              x3_ri_model=x3_ri_model,
              x3x1_ri_model = x3x1_ri_model,
              model_ri=model_ri))
}


###############################################################################
## CALL Functions 
###############################################################################

for (i in 1:nrow(sims_parameters)) {
  # Extract parameters for the current iteration
  N_dev <- sims_parameters$N_dev[i]
  Y_prev <- sims_parameters$Y_prev[i]
  gamma_x1 <- sims_parameters$gamma_x1[i]
  gamma_x2 <- sims_parameters$gamma_x2[i]
  gamma_x3 <- sims_parameters$gamma_x3[i]
  gamma_x4 <- sims_parameters$gamma_x4[i]
  gamma_x5 <- sims_parameters$gamma_x5[i]
  gamma_U <- sims_parameters$gamma_U[i]

  
  #1.dev_data function------------
  dev_data <- dev_data_simulation_function(N_dev = sims_parameters$N_dev[i],
                                           Y_prev = sims_parameters$Y_prev[i],
                                           gamma_x1 = sims_parameters$gamma_x1[i],
                                           gamma_x2 = sims_parameters$gamma_x2[i],
                                           gamma_x3 = sims_parameters$gamma_x3[i],
                                           gamma_x4 = sims_parameters$gamma_x4[i],
                                           gamma_x5 = sims_parameters$gamma_x5[i],
                                           gamma_U = sims_parameters$gamma_U[i])
  
  
  
  #2.dev_mod_function------------------
  submodel <- sub_mod_function(dev_data = dev_data)
  refmodel <- ref_mod_function(dev_data = dev_data)
  xgmodel <- xg_function(dev_data = dev_data)
  model <- complete_mod_function(dev_data=dev_data)
  rimodel <- ri_function(dev_data=dev_data)
  
  #3. Store development dataset and model
  development_dataset <- list("dev_data" = dev_data,
                              "submodel" = submodel,
                              "refmodel" = refmodel,
                              "xgmodel" = xgmodel, 
                              "rimodel" = rimodel,
                              "model"=model)
  
  #4. Save
  setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\Development Datasets") 
  # Construct the filename with today's date
  filename <- paste0("DevData_MCAR", "_Yprev_", sims_parameters$Y_prev[i], ".Rdata")
#  filename <- paste0("DevData_MAR", "_Yprev_", sims_parameters$Y_prev[i],  ".Rdata")
# filename <- paste0("DevData_MNAR", "_Yprev_", sims_parameters$Y_prev[i], ".Rdata")
  save(development_dataset, file = filename)
}
