## 01_Functions_GenerateData.R
################################################################################
# Created: 29Jul2025
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(broom)
library(pROC)
library(xgboost)
library(MASS) # for bivariate normal distribution 



###############################################################################
## Functions for running iterations
################################################################################
####-----------------------------------------------------------------------------------------
## Define a function to repeat the simulation across all iterations (for a given scenario)
####-----------------------------------------------------------------------------------------

simulation_nrun_fnc <- function(n_iter,
                                N_val,
                                Y_prev,
                                gamma_x1,
                                gamma_x2,
                                gamma_x3,
                                gamma_x4,
                                gamma_x5,
                                gamma_U,
                                submodel,
                                refmodel,
                                xgboost_model,
                                model) {
  
  all_iterations <- list()
  
  ## Repeat through number of iterations
  for (iter in 1:n_iter) {
    
    ## Set unique seed per iteration
    set.seed(918273 + iter)
    
    iter_current <- simulation_singlerun_fnc(
      N_val = N_val,
      Y_prev = Y_prev,
      gamma_x1 = gamma_x1,
      gamma_x2 = gamma_x2,
      gamma_x3 = gamma_x3,
      gamma_x4 = gamma_x4,
      gamma_x5 = gamma_x5,
      gamma_U = gamma_U,
      submodel = submodel,
      refmodel = refmodel,
      xgboost_model = xgboost_model,
      model = model
    )
    
    all_iterations[[iter]] <- iter_current
    print(paste("iteration", iter, "complete"))
  }
  
  return(list(iterations = all_iterations))
}



#-------------------------------------------------------------------------------


####---------------------------------------------
## Function that gives a single run per scenario
####---------------------------------------------

simulation_singlerun_fnc <- function(Y_prev,
                                     N_val,
                                     gamma_x1,
                                     gamma_x2,
                                     gamma_x3,
                                     gamma_x4,
                                     gamma_x5,
                                     gamma_U,
                                     submodel,
                                     refmodel,
                                     xgboost_model,
                                     model) {
  
  
  parameters <- list(N_val = N_val,
                     Y_prev = Y_prev,
                     gamma_x1 = gamma_x1,
                     gamma_x2 = gamma_x2,
                     gamma_x3 = gamma_x3,
                     gamma_x4 = gamma_x4,
                     gamma_x5 = gamma_x5,
                     gamma_U = gamma_U)
  
  
  #1.val_imp_data function-------------
  val_data <- simulation_function(N_val = N_val,
                                  Y_prev = Y_prev,
                                  gamma_x1 = gamma_x1,
                                  gamma_x2 = gamma_x2,
                                  gamma_x3 = gamma_x3,
                                  gamma_x4 = gamma_x4,
                                  gamma_x5 = gamma_x5,
                                  gamma_U = gamma_U)
  
  
  #2.  Make predictions
  predictions <- prediction_function(val_data = val_data, 
                                     submodel=submodel,
                                     refmodel = refmodel, 
                                     xgboost_model = xgboost_model,
                                     rimodel=rimodel,
                                     model=model)
  
  #3. Compare Performance 
  target_measures <- predictive.performance.function(predictions=predictions)
  
  return(list("Parameters" = parameters,
              "model"=model,
              "val_data" = val_data,
              "predictions" = predictions,
              "target_measures" = target_measures))
  
  
}




###############################################################################
## 0. Function that takes the inverse logistic link
###############################################################################

## Function to extract the OR from the coeffiction
expit_function <- function(x) {1/(1+exp(-x))} ## Recal ex/1+ex this is the same to get the P(Y=1)



####---------------------------------------
## 2. Function that simulates Validation data
####---------------------------------------

simulation_function <- function(N_val,
                                Y_prev, ## Prevalence of Y
                                gamma_0,  ## Intercept in Y model
                                gamma_x1,  ## Coefficient of X1 on outcome Y
                                gamma_x2,  ## Coefficient of X2 on outcome Y
                                gamma_x3,  ## Coefficient of X3 on outcome Y
                                gamma_x4,  ## Coefficient of X4 on outcome Y
                                gamma_x5,  ## Coefficient of X5 on outcome Y
                                gamma_U)   ## Coefficient on U on outcome Y 
{
  
  
  
  # 1. Generate Dataset 
  IPD <- as.data.frame(MASS::mvrnorm(n = N_val, 
                                     mu = c(0, 0, 0,0,0,0 ), 
                                     Sigma = matrix(data = c(1.0, 0.2, 0.2, 0.7, 0.7, 0,
                                                             0.2, 1, 0.2, 0.2, 0.2, 0,
                                                             0.2, 0.2,  1, 0.2, 0.2, 0, 
                                                             0.7, 0.2, 0.2, 1.0, 0.5, 0, 
                                                             0.7, 0.2, 0.2, 0.5, 1.0, 0, 
                                                             0, 0, 0, 0, 0, 1),
                                     nrow = 6, 
                                    byrow = T)))
  
  colnames(IPD) <- c("x_1", "x_2", "x_3", "x_4", "x_5", "U")
  IPD <- IPD %>%
    mutate(x_5 = rbinom(n = N_val, size = 1, prob = plogis(x_5)))
  
  #2. Remove X1 and X3 
  myfreq <- c(0.25, 0, 0, 0,0,0)
  pattern <- matrix(c(1, 1, 1, 1, 1, 1,
                      0, 1, 1, 1, 1, 1, 
                      0, 1, 0, 1, 1, 1,
                      1, 1, 0, 1, 1, 1), 
                    nrow = 4, byrow = TRUE)
  
  
  ## MCAR
  data_miss <- ampute(IPD, patterns = pattern, mech = "MCAR")
  head(data_miss$amp)
  md.pattern(data_miss$amp)


  #determine the prevalence of Y through _0
  gamma_0 <- as.numeric(coef(glm(rbinom(N_val, 1, prob = Y_prev) ~
                                   offset(gamma_x1*x_1 +
                                            gamma_x2*x_2 +
                                            gamma_x3*x_3 +
                                            gamma_x4*x_4 +
                                            gamma_x5*x_5 + 
                                            gamma_U*U),
                                 family = binomial(link = "logit"),
                                 data = IPD))[1])
  
  ########
  IPD$Y = rbinom(N_val, size = 1,
                 prob = expit_function(gamma_0 +
                                         gamma_x1*IPD$x_1 +
                                         gamma_x2*IPD$x_2 +
                                         gamma_x3*IPD$x_3 +
                                         gamma_x4*IPD$x_4 +
                                         gamma_x5*IPD$x_5))
 
  
  # 4. Combine with non missing
  colnames(IPD) <- c("x_1true", "x_2", "x_3true", "x_4", "x_5", "U","Y")
  val_data <- cbind(IPD, data_miss[["amp"]][,c("x_1","x_3")] )
 
  # 5. Create group variable of X_5 and X_4 for reference values 
  val_data <- val_data %>% mutate(x_4group = case_when(x_4<=0 ~ "Group 1", 
                                                       x_4 >0 ~ "Group 2"))
  
  # 6. Mark patterns 
  val_data <- val_data %>%
            mutate(pattern = case_when(!is.na(x_1) & !is.na(x_3)  ~ 0,
                                       is.na(x_1) & !is.na(x_3)  ~ 1,
                                       is.na(x_1) & is.na(x_3)  ~ 2,
                                       !is.na(x_1) & is.na(x_3) ~ 3))
  
  ## Print output
  print(sims_parameters$label[i])
  print("Y Prev")
  print(table(val_data$Y))
  print(prop.table(table(val_data$Y)) * 100)
  
  
  print("R Prev")
  print(prop.table(table(is.na(val_data$x_1))) * 100)
  

  
  return(val_data)
  
}

####------------------------------------------------------
##  3. Function that returns prediction from each of our models 
####-----------------------------------------------------

prediction_function <- function(val_data, submodel, refmodel, xgboost_model, model, rimodel) {
  
  
  
  ##  Completely Recorded Information Predictions
  #------------------------------------------------------
  print(model)
  predictions_cri <- predict(model,newdata=val_data, type="response")
  
  
  
  #  Reference value predictions 
  #------------------------------------------------------
  print(refmodel[["reference_values"]])
  
  val_data <- val_data %>% 
    left_join(refmodel[["reference_values"]], by = c("x_5", "x_4group")) 
  
  val_data <- val_data %>%
    mutate(x1_refimp= coalesce(x_1, mean_x1),
           x3_refimp = coalesce(x_3,mean_x3))
  
  
  predictions_ref <- predict(refmodel[["model_ref"]], newdata=val_data, type = 'response')
  
  
  
  
  
  # Submodel Predictions
  #------------------------------------------------------
  predictions_sm <- rep(NA, nrow(val_data))
  
  # Apply model for complete cases
  complete_cases <- val_data$pattern == 0
  if(sum(complete_cases) > 0) {
    predictions_sm[complete_cases] <- predict(submodel[["model_allobserved"]],
                                              newdata = val_data[complete_cases, ],
                                              type = "response")
  }
  
  # Apply model for missing cases  in x1
  missing_casesx1 <- val_data$pattern == 1
  if(sum(missing_casesx1) > 0) {
    predictions_sm[missing_casesx1] <- predict(submodel[["model_x1missing"]],
                                             newdata = val_data[missing_casesx1, ],
                                             type = "response")
  }
  
  
  # Apply model for missing cases  in x1x3
  missing_casesx1x3 <- val_data$pattern == 2
  if(sum(missing_casesx1x3) > 0) {
    predictions_sm[missing_casesx1x3] <- predict(submodel[["model_x1x3missing"]],
                                               newdata = val_data[missing_casesx1x3, ],
                                               type = "response")
  }
  
  # Apply model for missing cases  in x3
  missing_casesx3 <- val_data$pattern == 3
  if(sum(missing_casesx3) > 0) {
    predictions_sm[missing_casesx3] <- predict(submodel[["model_x3missing"]],
                                                 newdata = val_data[missing_casesx3, ],
                                                 type = "response")
  }
  
  
  # XG Boost predictions 
  #------------------------------------------------------
  xgb_model <- xgboost_model
  val_matrix <- as.matrix(val_data[, c( "x_1","x_2", "x_3", "x_4", "x_5")])
  val_label <- val_data$Y    
  
  dval <- xgb.DMatrix(data = val_matrix, label = val_label, missing = NA)
  
  predictions_xg <- predict(xgb_model, dval)

  
    
  # Regression imputation Predictions  
  #------------------------------------------------------
  
  data_ri <- val_data %>% 
    mutate(x_1_ri = case_when(is.na(x_1) ~ predict(rimodel[["x1_ri_model"]], newdata=val_data, type="response"), 
                              !is.na(x_1) ~ x_1),
           x_3_ri = case_when(is.na(x_3) ~ predict(rimodel[["x3_ri_model"]], newdata=val_data, type="response"), 
                              !is.na(x_3) ~ x_3),)
  
  ## Make predictions
  predictions_ri = predict(rimodel[["model_ri"]], newdata=data_ri, type="response")
  
  
  
  
  
  # Output  
  
  output_predictions <- data.frame("Y" = val_data$Y) #defining the Y as the Y column from the imputed datasets
  
  ## Combine Predictions 
  final_predictions <- cbind(output_predictions, predictions_cri,predictions_ref, predictions_sm , predictions_xg, predictions_ri)  
  names(final_predictions)[2] <- "Predictions_CRI"
  names(final_predictions)[3] <- "Prediction_Ref"
  names(final_predictions)[4] <- "Prediction_SubModel"
  names(final_predictions)[5] <- "Prediction_XG"
  names(final_predictions)[6] <- "Prediction_RI"
  
  return(predictions=final_predictions)
}

# 
# ####---------------------------------------------------------------
# ## 7. Function to calculate the predictive performance of the models
# ####---------------------------------------------------------------
predictive.performance.function <- function(predictions) {
  library(pROC)  # Ensure pROC is loaded
  
  # Extract Y (true labels)
  Y <- predictions$Y
  
  # Initialize an empty list to store results for each model
  results_list <- list()
  
  # Loop through each prediction column (excluding Y)
  for (col_name in colnames(predictions)[-1]) {  
    Predicted_Risks <- predictions[[col_name]]  # Extract predictions for current model
    
    ## Calculate Brier Score
    Brier_individuals <- (Predicted_Risks - Y)^2  
    Brier <- mean(Brier_individuals) 
    Brier_var <- var(Brier_individuals) / length(Predicted_Risks)
    
    # Calculate baseline Brier Score
    p_baseline <- mean(Y)
    Brier_baseline <- mean((p_baseline - Y)^2)
    
    # Scale the Brier Score
    Brier_scaled <- ifelse(mean(Y) %in% c(0, 1), NA, 1 - (Brier / Brier_baseline))
    
    ## Calibration Measures
    LP <- log(Predicted_Risks / (1 - Predicted_Risks))  # Log odds of predictions
    variance_LP <- var(LP)
    variance_PR <- var(Predicted_Risks)
    
    # Check if calibration can be computed
    if (sum(Y) == 0 || variance_PR == 0 || variance_PR < 1e-10 || variance_LP > 8) {
      Cal_Int <- NA
      Cal_Int_var <- NA
      Cal_Slope <- NA
      Cal_Slope_var <- NA
      message("Calibration intercept and slope cannot be calculated for ", col_name)
    } else {
      Cal_Int_model <- glm(Y ~ offset(LP), family = binomial(link = "logit"))
      Cal_Int_var <- vcov(Cal_Int_model)[1, 1]
      Cal_Int <- coef(Cal_Int_model)[1]
      
      Cal_Slope_model <- glm(Y ~ LP, family = binomial(link = "logit"))
      Cal_Slope_var <- vcov(Cal_Slope_model)[2, 2]
      Cal_Slope <- coef(Cal_Slope_model)[2]
    }
    
    ## AUC Calculation
    if (sum(Y) == 0 || variance_PR == 0 || variance_LP > 9) {
      AUC <- NA
      AUC_var <- NA
    } else {
      AUC <- tryCatch({
        roc_obj <- roc(response = Y, predictor = as.vector(Predicted_Risks), direction = "<", levels = c(0, 1))
        roc_obj$auc
      }, error = function(e) {
        message("Error in ROC calculation for ", col_name, ": ", e$message)
        NA
      })
      
      AUC_var <- tryCatch({
        if (!is.na(AUC)) {
          var(roc_obj, method = "delong")
        } else {
          NA
        }
      }, error = function(e) {
        message("Error in variance calculation for ", col_name, ": ", e$message)
        NA
      })
    }
    
    ## Store results in a data frame
    results_list[[col_name]] <- data.frame(
      "Model" = col_name,
      "Cal_Int" = Cal_Int,
      "Cal_Int_var" = Cal_Int_var,
      "Cal_Slope" = Cal_Slope,
      "Cal_Slope_var" = Cal_Slope_var,
      "AUC" = AUC,
      "AUC_var" = AUC_var,
      "Brier" = Brier,
      "Brier_scaled" = Brier_scaled
    )
  }
  
  # Combine results into a single data frame
  Target_measures <- do.call(rbind, results_list)
  
  return(Target_measures)
}
