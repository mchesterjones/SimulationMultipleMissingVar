################################################################################
## 04_FiguresforThesis 
################################################################################
## Date Created: 19Sep2026
## Author: MCJ
## Date last updated: 
## Purpose: Create graphs for the thesis 
################################################################################
## Libraries
################################################################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(pROC)
library(DT)
library(patchwork)

################################################################################
## Folders
################################################################################
folder = "C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\"
figures = paste0(folder, "Figures\\")
val_data = paste0(folder, "Validation Datasets\\")



################################################################################
## Datasets 
################################################################################
load(paste0(val_data,"CompatabilityofMechanisms_LongFormat.Rdata"))
load(paste0(val_data,"ComtabilityofMechanisms_AllIterations.Rdata"))


df_long <- compatability_df_long %>%
              mutate(method_graph = case_when(Method == "Completely Recorded Information" ~ "Fully observed", 
                                              Method == "XGBoost" ~ "XGBoost", 
                                              Method == "Sub Model" ~ "Pattern sub-model", 
                                              Method == "Reference Values"~ "Reference values", 
                                              Method == "Regression Imputation" ~ "Regression Imputation"))
df_long <- df_long %>% mutate(method_graph= as.factor(method_graph))
################################################################################
## otputs 
################################################################################
bias_comp <- ggplot(df_long %>% filter(Measure == "bias" & 
                                          samplesize=="N=100,000"),
       aes(x = AVG, y = method_graph, colour = Mechanism, shape=Mechanism)) +
  geom_point(size = 3) +
 # geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  labs(x = "Bias of predictions vs true outcome",
       y = "Proportion of Rows Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  scale_x_continuous(limits=c(-0.005,0.01), breaks=seq(-0.06,0.06, by=0.005))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ . ,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.ticks.y = element_blank()) +
  scale_shape_manual(values = c("MAR" = 17, "MCAR" = 16, "MNAR" = 15)) +
  guides(
    colour = guide_legend("Mechanism"),
    shape  = guide_legend("Mechanism")
  )


## Discrimination 
##----------------------
auc_comp <- ggplot(df_long %>% filter(Measure == "AUC" & 
                              samplesize=="N=100,000"),
         aes(x = AVG, y = method_graph, colour = Mechanism, shape=Mechanism)) +
  geom_point(size = 3) +
  labs(x = "Discrimination",
       y = "Proportion of Rows Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  scale_x_continuous(limits=c(0.79, 0.83), breaks=seq(-1, 1, by=0.01)) +
  facet_grid(Missingness ~ . ,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.ticks.y = element_blank()) +
  scale_shape_manual(values = c("MAR" = 17, "MCAR" = 16, "MNAR" = 15)) +
  guides(
    colour = guide_legend("Mechanism"),
    shape  = guide_legend("Mechanism")
  )




### Calibration Intercept 
##--------------------------------------------------
calint_comp <- ggplot(df_long %>% filter(Measure == "Cal_Int" & 
                              samplesize=="N=100,000"),
         aes(x = AVG, y = method_graph, colour = Mechanism, shape=Mechanism)) +
  geom_point(size = 3) +
  labs(x = "Calibration in the Large",
       y = "Proportion of Rows Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  scale_x_continuous(limits=c(-0.05, 0.1), breaks=seq(-1, 1, by=0.05)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  facet_grid(Missingness ~ . ,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.ticks.y = element_blank()) +
  scale_shape_manual(values = c("MAR" = 17, "MCAR" = 16, "MNAR" = 15)) +
  guides(
    colour = guide_legend("Mechanism"),
    shape  = guide_legend("Mechanism")
  )




### Calibration Slope 
##--------------------------------------------------
calslope_comp <- ggplot(df_long %>% filter(Measure == "Cal_Slope" & 
                                           samplesize=="N=100,000"),
                      aes(x = AVG, y = method_graph, colour = Mechanism, shape=Mechanism)) +
  geom_point(size = 3) +
  labs(x = "Calibration Slope",
       y = "Proportion of Rows Missing") +
  theme_minimal() +
  scale_x_continuous(limits=c(0.95, 1.025), breaks=seq(-1, 2, by=0.025)) +
  geom_vline(aes(xintercept = 1), linetype = "dashed", colour = "grey30") +
  facet_grid(Missingness ~ . ,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.ticks.y = element_blank()) +
  scale_shape_manual(values = c("MAR" = 17, "MCAR" = 16, "MNAR" = 15)) +
  guides(
    colour = guide_legend("Mechanism"),
    shape  = guide_legend("Mechanism")
  )



### RMSE 
##--------------------------------------------------
rmse_comp <- ggplot(df_long %>% filter(Measure == "rmse" & 
                                         samplesize=="N=100,000"),
                    aes(x = AVG, y = method_graph, colour = Mechanism, shape=Mechanism)) +
  geom_point(size = 3) +
  labs(x = "Root Mean Square Error",
       y = "Proportion of Rows Missing") +
  theme_minimal() +
  scale_x_continuous(limits=c(0.272, 0.28), breaks=seq(0, 2, by=0.002)) +
  facet_grid(Missingness ~ . ,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.ticks.y = element_blank()) +
  scale_shape_manual(values = c("MAR" = 17, "MCAR" = 16, "MNAR" = 15)) +
  guides(
    colour = guide_legend("Mechanism"),
    shape  = guide_legend("Mechanism")
  )



### Brier Score Scaled 
##--------------------------------------------------
brisc_comp <- ggplot(df_long %>% filter(Measure == "Brier_scaled" & 
                                         samplesize=="N=100,000"),
                    aes(x = AVG, y = method_graph, colour = Mechanism, shape=Mechanism)) +
  geom_point(size = 3) +
  labs(x = "Brier Score Scaled",
       y = "Proportion of Rows Missing") +
  theme_minimal() +
  scale_x_continuous(limits=c(0.12, 0.18), breaks=seq(0, 2, by=0.01)) +
  facet_grid(Missingness ~ . ,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.ticks.y = element_blank()) +
  scale_shape_manual(values = c("MAR" = 17, "MCAR" = 16, "MNAR" = 15)) +
  guides(
    colour = guide_legend("Mechanism"),
    shape  = guide_legend("Mechanism")
  )

################################################################################
## Patchwork
################################################################################
# Top row - keep y-axis only on auc_comp, remove from others
auc_comp <- auc_comp + theme(legend.position = "none")  # Keep y-axis as is

calint_comp <- calint_comp + theme(
  legend.position = "none",
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  strip.text.y = element_blank()  # This removes the facet labels (25%, 50%, 75%)
)

calslope_comp <- calslope_comp + theme(
  legend.position = "none", 
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  strip.text.y = element_blank()  # This removes the facet labels (25%, 50%, 75%)
)

# Bottom row - keep y-axis only on brisc_comp, remove from others
brisc_comp <- brisc_comp + theme(legend.position = "none")  # Keep y-axis as is

bias_comp <- bias_comp + theme(
  legend.position = "none",
  axis.text.y = element_blank(), 
  axis.title.y = element_blank(),
  strip.text.y = element_blank()  # This removes the facet labels (25%, 50%, 75%)
)

rmse_comp <- rmse_comp + theme(
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  strip.text.y = element_blank(),
  legend.position = "bottom",        # Keep legend for collection
  legend.direction = "horizontal",
  legend.box = "horizontal"
)

# Create patchwork
patchwork <- (auc_comp / brisc_comp | calint_comp / bias_comp | calslope_comp/rmse_comp) +
  plot_layout(
    guides = "collect", 
    widths = c(2.5, 2, 2)
  ) +
  plot_annotation(
    title = "MCAR, MAR, MNAR at validation vs. MAR at development",
    theme = theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.8, "cm"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    )
  )

patchwork


ggsave(
  filename = paste0(figures, "Compatability of Mechanisms.pdf"),
  plot = patchwork,
  width = 14, height = 10
)




