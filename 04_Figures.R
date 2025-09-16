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


### Need to do per sample size 

################################################################################
## Performance Metric with all the same scale 
################################################################################
# Define parameter combinations
mechanisms <- c("MCAR", "MAR", "MNAR")
samplesizes <- c("N=500","N=10,000", "N=100,000")
missing <- c("0.25","0.5","0.75")

# Define all measures and their corresponding column names
measures <- list(
  "AUC" = list(measure = "AUC", column = "AUC", title = "Discrimination"),
  "Brier" = list(measure = "Brier_scaled", column = "Brier_scaled", title = "Brier Score Scaled"),
  "Bias" = list(measure = "bias", column = "bias", title = "Bias of predictions vs true outcome"),
  "Cal_Int" = list(measure = "Cal_Int", column = "Cal_Int", title = "Calibration in the Large"),
  "Cal_Slope" = list(measure = "Cal_Slope", column = "Cal_Slope", title = "Calibration Slope"),
  "RMSE" = list(measure = "rmse", column = "rmse", title = "Root Mean Square Error")
)

# Initialize empty list to store all plots
all_plots <- list()

# Define consistent color palette
color_palette <- c(
  "#984ea3",  # Purple
  "#ff7f00",  # Orange
  "#4daf4a",  # Green
  "#377eb8",  # Blue
  "#e41a1c"   # Red
)

# Main loop
for (mech in mechanisms) {
  for (ss in samplesizes) {
    
    # Filter data once per combination
    filter_df_long <- summarised_df_long %>% 
      filter(Mechanism == mech & samplesize == ss)
    
    filtered_combined_df <- combined_df %>% 
      filter(Mechanism == mech & samplesize == ss)
    
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
             y = "Proportion of Rows Missing",
             colour = "Method\n(Mean, 95% CI)") +
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
                      "AUC" = scale_x_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, by=0.1)),
                      "Brier" = scale_x_continuous(limits = c(-0.05, 0.3), breaks = seq(-0.05, 0.3, by=0.05)),
                      "Bias" = list(
                        scale_x_continuous(limits = c(-0.05, 0.05), breaks = seq(-0.06, 0.06, by=0.01)),
                        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                      ),
                      "Cal_Int" = list(
                        scale_x_continuous(limits = c(-0.75, 0.5), breaks = seq(-1, 1, by=0.25)),
                        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                      ),
                      "Cal_Slope" = list(
                        scale_x_continuous(limits = c(0.50, 1.5), breaks = seq(0.50, 2, by=0.25)),
                        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30")
                      ),
                      "RMSE" = scale_x_continuous(limits = c(0.2, 0.325), breaks = seq(0.2, 0.325, by=0.025))
      )
      
      # Create unique name and store plot
      plot_name <- paste(mech, gsub("[^0-9]", "", ss), measure_name, sep = "_")
      all_plots[[plot_name]] <- p
      
      # Optional: print plot
      print(p)
    }
  }
}



### Store in nested list structure (recommended)
plots_nested <- list()
for (mech in mechanisms) {
  plots_nested[[mech]] <- list()
  for (ss in samplesizes) {
    plots_nested[[mech]][[ss]] <- list()
    # Store each measure plot here
  }
}




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
  "Bias" = list(measure = "bias", column = "bias", title = "Bias of predictions vs true outcome"),
  "Cal_Int" = list(measure = "Cal_Int", column = "Cal_Int", title = "Calibration in the Large"),
  "Cal_Slope" = list(measure = "Cal_Slope", column = "Cal_Slope", title = "Calibration Slope"),
  "RMSE" = list(measure = "rmse", column = "rmse", title = "Root Mean Square Error")
)

# Initialize empty list to store all plots
all_plots <- list()

# Define consistent color palette
color_palette <- c(
  "#984ea3",  # Purple
  "#ff7f00",  # Orange
  "#4daf4a",  # Green
  "#377eb8",  # Blue
  "#e41a1c"   # Red
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
                      "Bias" = list(
                        scale_x_continuous(limits = c(-0.025, 0.015), breaks = seq(-0.06, 0.06, by=0.01)),
                        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                      ),
                      "Cal_Int" = list(
                        scale_x_continuous(limits = c(-0.35, 0.15), breaks = seq(-1, 1, by=0.1)),
                        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30")
                      ),
                      "Cal_Slope" = list(
                        scale_x_continuous(limits = c(0.9, 1.1), breaks = seq(0.50, 2, by=0.05)),
                        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30")
                      ),
                      "RMSE" = scale_x_continuous(limits = c(0.26, 0.29), breaks = seq(0.2, 0.325, by=0.01))
      )
      
      # Create unique name and store plot - FIXED: added replacement parameter
      plot_name <- paste(mech, "100000", measure_name, sep = "_")
      all_plots[[plot_name]] <- p
      
      
      ## save 
      ggsave(
        filename = paste0(figures, "/", plot_name, ".svg"),  # or ".pdf"
        plot = p,
        width = 8, height = 6, dpi = 300
      )
      
    }
}

# Store in nested list structure - FIXED: corrected variable name
plots_n100000 <- list()
for (mech in mechanisms) {
  plots_n100000[[mech]] <- list()
  for (measure_name in names(measures)) {
    plot_name <- paste(mech, "100000", measure_name, sep = "_")
    plots_n100000[[mech]][[measure_name]] <- all_plots[[plot_name]]
  }
}





### Create Patchworks 

### Create Patchworks 
# Create patchwork plots for each mechanism
for (mech in mechanisms) {
  
  # Calibration plots (keep y-axis)
  cal_int_plot   <- plots_n100000[[mech]][["Cal_Int"]] + theme(legend.position = "none")
  cal_slope_plot <- plots_n100000[[mech]][["Cal_Slope"]] + theme(legend.position = "none")
  
  # Other plots (strip y-axis and legend)
  auc_plot   <- plots_n100000[[mech]][["AUC"]]   + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  brier_plot <- plots_n100000[[mech]][["Brier"]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  bias_plot  <- plots_n100000[[mech]][["Bias"]]  + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  rmse_plot  <- plots_n100000[[mech]][["RMSE"]]  + 
    theme(legend.position="bottom", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  # Combine plots: calibration left, others right
  combined_plot <- (cal_int_plot / cal_slope_plot | auc_plot + brier_plot + bias_plot + rmse_plot) +
    plot_layout(guides = "collect", widths = c(1, 2)) +
    plot_annotation(
      title = paste("Model Performance Metrics -", mech, "Mechanism (N=100,000)"),
      theme = theme(
        legend.position = "bottom",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
    )
  
  # Store, print, save
  patchwork_plots[[mech]] <- combined_plot
  print(combined_plot)
  
  ggsave(
    filename = paste0(figures, "/", mech, "_patchwork.pdf"),
    plot = combined_plot,
    width = 14, height = 10
  )
  
  
}

