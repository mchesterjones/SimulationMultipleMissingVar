################################################################################
## 04_FiguresforThesis 
################################################################################
## Date Created: 16Sep2026
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
## Folders C:\Users\maecj\OneDrive - Nexus365\A DPhil\Simulation studies\Programs\Study 3
################################################################################
folder = "C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 3\\"
figures = paste0(folder, "Figures\\")
val_data = paste0(folder, "Validation Datasets\\")



################################################################################
## Datasets 
################################################################################
load(paste0(val_data,"AllIter.Rdata"))
load(paste0(val_data,"LongFormat.Rdata"))


#### Update completely recorded to fully observed model 
################################################################################
combined_df <- combined_df %>% mutate(Method = case_when(Method == "Completely Recorded Information" ~ "Fully Observed", 
                                                         Method == "Sub Model" ~ "Pattern sub-model",
                                                         TRUE ~ Method))

combined_df$Method <- factor(combined_df$Method, 
                             levels = c( "Regression Imputation", "Reference Values", "Pattern sub-model", "XGBoost", "Fully Observed"))


summarised_df_long <- summarised_df_long %>% mutate(Method = case_when(Method == "Completely Recorded Information" ~ "Fully Observed", 
                                                                       Method == "Sub Model" ~ "Pattern sub-model",
                                                                       TRUE ~ Method))
summarised_df_long$Method <- factor(summarised_df_long$Method, 
                                    levels = c( "Regression Imputation", "Reference Values", "Pattern sub-model", "XGBoost", "Fully Observed"))


# 1. Update summarised_df_long to make NAs
summarised_df_long <- summarised_df_long %>% 
  mutate(across(c("AVG", "LCI", "UCI"), ~ case_when(
    Method == "Fully Observed" & (Missingness %in% c("50%", "75%")) ~ NA_real_,
    TRUE ~ .
  )))

# 2. Update combined_df to make NAs
combined_df <- combined_df %>% 
  mutate(across(Cal_Int:Brier_scaled, ~ case_when(
    Method == "Fully Observed" & (Missingness %in% c("50%", "75%")) ~ NA_real_,
    TRUE ~ as.numeric(.)  # This strips the 'auc' class and makes it a standard double
  )))

################################################################################
## Scale per Sample Size: N=100,000 for main body of text 
################################################################################
# Define parameter combinations
mechanisms <- c("MCAR", "MAR", "MNAR")
missing <- c("0.25","0.5","0.75")

# Define all measures and their corresponding column names
measures <- list(
  "AUC" = list(measure = "AUC", column = "AUC", title = "Discrimination"),
  "Brier" = list(measure = "Brier_scaled", column = "Brier_scaled", title = "Brier Score Scaled"),
  "Cal_Int" = list(measure = "Cal_Int", column = "Cal_Int", title = "Calibration in the Large"),
  "Cal_Slope" = list(measure = "Cal_Slope", column = "Cal_Slope", title = "Calibration Slope")
)

# Initialize empty list to store all plots
all_plots <- list()

# Define consistent color palette
color_palette <- c(
  "#984ea3",  # Purple
  "#ff7f00",  # Orange
  "#4daf4a",  # Green
  "#377eb8",  # Blue
  "black"   # Red
)

# Main loop
for (mech in mechanisms) {

    # Filter data once per combination
    filter_df_long <- summarised_df_long %>% 
      filter(Mechanism == mech & samplesize == "N=100,000")
    
    filtered_combined_df <- combined_df %>% 
      filter(Mechanism == mech & samplesize  == "N=100,000")
    
    # Create plots for each measure
    for (measure_name in names(measures)) {
      
      measure_info <- measures[[measure_name]]
      
      # Create base plot
      p <- ggplot(filter_df_long %>% filter(Measure == measure_info$measure),
                  aes(x = AVG, y = Method, colour = Method)) +
        geom_point(size = 3) +
        geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
        geom_point(data = filtered_combined_df, 
                   aes_string(x = measure_info$column, y = "Method"),
                   shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
        labs(
          #title = measure_info$title,
          x = measure_info$title,
          y = "Proportion of rows missing",
          colour = "Method (Mean, 95% CI)") +
        theme_minimal() +
        facet_grid(Missingness ~., scales = "fixed", switch = "both") +
        theme(legend.position = "right",
              strip.text = element_text(size = 14),
              strip.placement = "outside",
              strip.background = element_blank(),
              axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
              axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
              plot.title = element_text(size = 16, face = "bold"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        scale_colour_manual(values = color_palette)
      
      # Add measure-specific customizations
      p <- p + switch(measure_name,
                      "AUC" = scale_x_continuous(limits = c(0.78, 0.84), breaks = seq(0, 1, by=0.02)),
                      "Brier" = scale_x_continuous(limits = c(0.1, 0.2), breaks = seq(-0.05, 0.3, by=0.02)),
                      "Cal_Int" = list(
                        scale_x_continuous(limits = c(-0.35, 0.15), breaks = seq(-1, 1, by=0.1)),
                        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                      ),
                      "Cal_Slope" = list(
                        scale_x_continuous(limits = c(0.9, 1.1), breaks = seq(0.50, 2, by=0.05)),
                        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30")
                      )
      )
      
      # Create unique name and store plot - FIXED: added replacement parameter
      plot_name <- paste(mech, "100000", measure_name, sep = "_")
      all_plots[[plot_name]] <- p
      
      
      ## save 
      ggsave(
        filename = paste0(figures, "/", plot_name, ".svg"), 
        plot = p,
        width = 8, height = 6, dpi = 300
      )
      
    }
}

# Store in nested list structure 
plots_n100000 <- list()
for (mech in mechanisms) {
  plots_n100000[[mech]] <- list()
  for (measure_name in names(measures)) {
    plot_name <- paste(mech, "100000", measure_name, sep = "_")
    plots_n100000[[mech]][[measure_name]] <- all_plots[[plot_name]]
  }
}



### Create Patchworks
#################################################################################
## By Sample size comparing mechanism - N=100,000
###-----------------------------------------------
# 1. Flatten the plots into one list (12 plots total)
all_plots <- c(
  plots_n100000[["MCAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")],
  plots_n100000[["MAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")],
  plots_n100000[["MNAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")]
)

# 2. Assemble with Custom Tags
auc_brier_caln100000 <- wrap_plots(all_plots, ncol = 4) + 
  plot_layout(guides = "collect" , axes = "collect_y") +
  plot_annotation(
    title = "Model Performance Across Missingness Mechanisms (N=100,000)",
    # We provide 12 tags. Only the first of each row gets a label; others are empty ""
    tag_levels = list(c("", "", "MCAR", "", "", "","MAR", "", "", "", "MNAR", "")),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.tag = element_text(size = 20, face = "bold", angle = 90, vjust = 0.5), # Vertical labels
      plot.tag.position = "left" # Moves tags to the far left of the row
    )
  ) & 
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 12) # Individual plot titles (AUC, Brier, etc.)
  )

auc_brier_caln100000



ggsave(
  filename = paste0(figures,"AUCBrierCalibration_N100000_patchwork.pdf"),
  plot = auc_brier_caln100000,
  width = 14, height = 10)

ggsave(
  filename = paste0(figures,"AUCBrierCalibration_N100000_patchwork.svg"),
  plot = auc_brier_caln100000,
  width = 14, height = 10)


######################################################################################################
## N=10,000 
#######################################################################################################
# Initialize empty list to store all plots
all_n10000_plots <- list()

# Restrict to n10000
n10000_long <- summarised_df_long %>% filter(samplesize=="N=10,000")
n10000_combined <- combined_df %>% filter(samplesize=="N=10,000")

# Update scale for each 
AUCmin <- signif(min(n10000_combined$AUC), digits = 3) -0.02
AUCmax <- signif(max(n10000_combined$AUC), digits=2) + 0.02

CALINTmin <- signif(min(n10000_combined$Cal_Int), digits = 2) -0.02
CALINTmax <- signif(max(n10000_combined$Cal_Int), digits = 2) +0.02

CALSLOPEmin <- signif(min(n10000_combined$Cal_Slope), digits = 3) -0.02
CALSLOPEmax <- signif(max(n10000_combined$Cal_Slope), digits = 3) +0.02

BRIERSCmin <- signif(min(n10000_combined$Brier_scaled), digits = 2) -0.02
BRIERSCmax <- signif(max(n10000_combined$Brier_scaled), digits = 2) +0.02


# Main loop
for (mech in mechanisms) {
  
  # Filter data once per combination
  filter_df_long <- n10000_long %>% 
    filter(Mechanism == mech)
  
  filtered_combined_df <- n10000_combined %>% 
    filter(Mechanism == mech)
  
  
  # Create plots for each measure
  for (measure_name in names(measures)) {
    
    measure_info <- measures[[measure_name]]
    
    # Create base plot
    p <- ggplot(filter_df_long %>% filter(Measure == measure_info$measure),
                aes(x = AVG, y = Method, colour = Method)) +
      geom_point(size = 3) +
      geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
      geom_point(data = filtered_combined_df, 
                 aes_string(x = measure_info$column, y = "Method"),
                 shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
      labs(
        #title = measure_info$title,
        x = measure_info$title,
        y = "Proportion of rows missing",
        colour = "Method (Mean, 95% CI)") +
      theme_minimal() +
      facet_grid(Missingness ~., scales = "fixed", switch = "both") +
      theme(legend.position = "right",
            strip.text = element_text(size = 14),
            strip.placement = "outside",
            strip.background = element_blank(),
            axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
            axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
            plot.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_colour_manual(values = color_palette)
    
    # Add measure-specific customizations
    p <- p + switch(measure_name,
                    "AUC" = scale_x_continuous(limits = c(0.78, 0.84), breaks = seq(0, 1, by=0.02)),
                    "Brier" = scale_x_continuous(limits = c(0.1, 0.20), breaks = seq(0.1, 0.2, by=0.02)),
                    "Bias" = list(
                      scale_x_continuous(limits = c(BIASmin, BIASmax), breaks = seq(-0.06, 0.06, by=0.01)),
                      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                    ),
                    "Cal_Int" = list(
                      scale_x_continuous(limits = c(-0.4, 0.2), breaks = seq(-0.4, 0.2, by=0.1)),
                      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                    ),
                    "Cal_Slope" = list(
                      scale_x_continuous(limits = c(0.8, 1.2), breaks = seq(0.50, 2, by=0.1)),
                      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30")
                    ),
                    "RMSE" = scale_x_continuous(limits = c(RMSEmin, RMSEmax), breaks = seq(0.2, 0.325, by=0.01))
    )
    
    # Create unique name and store plot 
    plot_name <- paste(mech, "10000", measure_name, sep = "_")
    all_n10000_plots[[plot_name]] <- p
    
    
    ## save 
    ggsave(
      filename = paste0(figures, "/", plot_name, ".svg"),  # or ".pdf"
      plot = p,
      width = 8, height = 6, dpi = 300
    )
    
  }
}


# Store in nested list structure 
plots_n10000 <- list()

for (mech in mechanisms) {
  plots_n10000[[mech]] <- list()
  for (measure_name in names(measures)) {
    # Match the naming convention used in the loop above
    plot_name <- paste(mech, "10000", measure_name, sep = "_")
    plots_n10000[[mech]][[measure_name]] <- all_n10000_plots[[plot_name]]
  }
}


# 1. Flatten the plots into one list (12 plots total)
all_plotsn10000 <- c(
  plots_n10000[["MCAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")],
  plots_n10000[["MAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")],
  plots_n10000[["MNAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")]
)

# 2. Assemble with Custom Tags
auc_brier_caln10000 <- wrap_plots(all_plotsn10000, ncol = 4) + 
  plot_layout(guides = "collect" , axes = "collect_y") +
  #, axes = "collect") +
  plot_annotation(
    title = "Model Performance Across Missingness Mechanisms (N=10000)",
    # We provide 12 tags. Only the first of each row gets a label; others are empty ""
    tag_levels = list(c("", "", "MCAR", "", "", "","MAR", "", "", "", "MNAR", "")),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.tag = element_text(size = 18, face = "bold", angle = 90, vjust = 0.5), # Vertical labels
      plot.tag.position = "left" # Moves tags to the far left of the row
    )
  ) & 
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 12) # Individual plot titles (AUC, Brier, etc.)
  )

auc_brier_caln10000


ggsave(
  filename = paste0(figures,"AUCBrierCalibration_n10000_patchwork.pdf"),
  plot = auc_brier_caln10000,
  width = 14, height = 10)

ggsave(
  filename = paste0(figures,"AUCBrierCalibration_n10000_patchwork.svg"),
  plot = auc_brier_caln10000,
  width = 14, height = 10)



#############################################################################################
## N=500 
###############################################################################################
# Initialize empty list to store all plots
all_n500_plots <- list()

# Restrict to N500
n500_long <- summarised_df_long %>% filter(samplesize=="N=500")
n500_combined <- combined_df %>% filter(samplesize=="N=500")

# Update scale for each 
AUCmin <- signif(min(n500_combined$AUC), digits = 3) -0.02
AUCmax <- signif(max(n500_combined$AUC), digits=2) + 0.02

CALINTmin <- signif(min(n500_combined$Cal_Int), digits = 2) -0.02
CALINTmax <- signif(max(n500_combined$Cal_Int), digits = 2) +0.02

CALSLOPEmin <- signif(min(n500_combined$Cal_Slope), digits = 3) -0.02
CALSLOPEmax <- signif(max(n500_combined$Cal_Slope), digits = 3) +0.02

BRIERSCmin <- signif(min(n500_combined$Brier_scaled), digits = 2) -0.02
BRIERSCmax <- signif(max(n500_combined$Brier_scaled), digits = 2) +0.02


# Main loop
for (mech in mechanisms) {
  
  # Filter data once per combination
  filter_df_long <- n500_long %>% 
    filter(Mechanism == mech)
  
  filtered_combined_df <- n500_combined %>% 
    filter(Mechanism == mech)
  
  
  # Create plots for each measure
  for (measure_name in names(measures)) {
    
    measure_info <- measures[[measure_name]]
    
    # Create base plot
    p <- ggplot(filter_df_long %>% filter(Measure == measure_info$measure),
                aes(x = AVG, y = Method, colour = Method)) +
      geom_point(size = 3) +
      geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
      geom_point(data = filtered_combined_df, 
                 aes_string(x = measure_info$column, y = "Method"),
                 shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
      labs(
        #title = measure_info$title,
        x = measure_info$title,
        y = "Proportion of rows missing",
        colour = "Method (Mean, 95% CI)") +
      theme_minimal() +
      facet_grid(Missingness ~., scales = "fixed", switch = "both") +
      theme(legend.position = "right",
            strip.text = element_text(size = 14),
            strip.placement = "outside",
            strip.background = element_blank(),
            axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
            axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
            plot.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_colour_manual(values = color_palette)
    
    # Add measure-specific customizations
    p <- p + switch(measure_name,
                    "AUC" = scale_x_continuous(limits = c(AUCmin, AUCmax), breaks = seq(0, 1, by=0.04)),
                    "Brier" = scale_x_continuous(limits = c(BRIERSCmin, BRIERSCmax), breaks = seq(-0.05, 0.3, by=0.1)),
                    "Bias" = list(
                      scale_x_continuous(limits = c(BIASmin, BIASmax), breaks = seq(-0.06, 0.06, by=0.01)),
                      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                    ),
                    "Cal_Int" = list(
                      scale_x_continuous(limits = c(CALINTmin, CALINTmax), breaks = seq(-1, 1, by=0.4)),
                      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                    ),
                    "Cal_Slope" = list(
                      scale_x_continuous(limits = c(0.5, 1.5), breaks = seq(0.50, 2, by=0.2)),
                      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30")
                    ),
                    "RMSE" = scale_x_continuous(limits = c(RMSEmin, RMSEmax), breaks = seq(0.2, 0.325, by=0.01))
    )
    
    # Create unique name and store plot 
    plot_name <- paste(mech, "500", measure_name, sep = "_")
    all_n500_plots[[plot_name]] <- p
    
    
    ## save 
    ggsave(
      filename = paste0(figures, "/", plot_name, ".svg"),  # or ".pdf"
      plot = p,
      width = 8, height = 6, dpi = 300
    )
    
  }
}


# Store in nested list structure 
plots_n500 <- list()

for (mech in mechanisms) {
  plots_n500[[mech]] <- list()
  for (measure_name in names(measures)) {
    # Match the naming convention used in the loop above
    plot_name <- paste(mech, "500", measure_name, sep = "_")
    plots_n500[[mech]][[measure_name]] <- all_n500_plots[[plot_name]]
  }
}


# 1. Flatten the plots into one list (12 plots total)
all_plotsn500 <- c(
  plots_n500[["MCAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")],
  plots_n500[["MAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")],
  plots_n500[["MNAR"]][c("AUC", "Brier", "Cal_Slope", "Cal_Int")]
)

# 2. Assemble with Custom Tags
auc_brier_caln500 <- wrap_plots(all_plotsn500, ncol = 4) + 
  plot_layout(guides = "collect" , axes = "collect_y") +
  #, axes = "collect") +
  plot_annotation(
    title = "Model Performance Across Missingness Mechanisms (N=500)",
    # We provide 12 tags. Only the first of each row gets a label; others are empty ""
    tag_levels = list(c("", "", "MCAR", "", "", "","MAR", "", "", "", "MNAR", "")),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.tag = element_text(size = 18, face = "bold", angle = 90, vjust = 0.5), # Vertical labels
      plot.tag.position = "left" # Moves tags to the far left of the row
    )
  ) & 
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 12) # Individual plot titles (AUC, Brier, etc.)
  )

auc_brier_caln500


ggsave(
  filename = paste0(figures,"AUCBrierCalibration_N500_patchwork.pdf"),
  plot = auc_brier_caln500,
  width = 14, height = 10)

ggsave(
  filename = paste0(figures,"AUCBrierCalibration_N500_patchwork.svg"),
  plot = auc_brier_caln500,
  width = 14, height = 10)

