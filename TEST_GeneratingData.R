library(mice)
set.seed(457)
N_val <- 10000

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
pattern <- matrix(c(0, 1, 1, 1, 1, 1, 
                    0, 1, 0, 1, 1, 1,
                    1, 1, 0, 1, 1, 1), 
                  nrow = 3, byrow = TRUE)


## MAR
mar_weights <- matrix(0, nrow = 3, ncol = 6)
mar_weights[1, 2] <- 0.5  # Pattern 2: V2 → V1
mar_weights[2, 2] <- 0.5  # Pattern 3: V2 → V1 & V3
mar_weights[3, 2] <- 0.5  # Pattern 4: V2 → V3

data_miss <- ampute(
  IPD, 
  prop = 0.25,  # 100% of rows are eligible
  patterns = pattern,
  mech = "MAR",
  weights = mar_weights
)

head(data_miss$amp)
md.pattern(data_miss$amp)
print("R Prev")

val_data <- data_miss[["amp"]]
print(prop.table(table(!is.na(val_data$x_1) & !is.na(val_data$x_3))) * 100) # pattern 1
print(prop.table(table(is.na(val_data$x_1))) * 100) # pattern 2
print(prop.table(table(is.na(val_data$x_3) & is.na(val_data$x_1))) * 100) # pattern 3
print(prop.table(table(is.na(val_data$x_3))) * 100) # pattern 4

