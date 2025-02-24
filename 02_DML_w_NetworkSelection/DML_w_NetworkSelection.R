options(max.print = 1000)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(dplyr)
library(qgraph)
library(ggplot2)
library(mice)
library(VIM)
library(causalweight)

# Read in refined metadata
data <- read.csv("t1t2ImputedData.csv")


# Keep only the specified variables
data <- data[, c("subnum.x", "csascore.x", "uclalst.x", "faith_pd.x", "shannon.x", "faith_pd.y", 
                 "shannon.y", "moca_impaired.y", "mocatot.y", "GDS.y", "GDS_impaired.y", "mencomp.x", 
                 "ldr2.x", "lotrt.x", "mlq_ps.x", "cse_sff.x", "nefftot.x", "wsdm_cd.x", "sdw_sa.x", 
                 "sdw_psb.x", "phycomp.x", "prsd8a_ss.x", "prsi_ss.x", "prdsa_ss.x", "socposc.x", 
                 "nsictot.x")]

# Missingness summary (if needed)
summarize_missingness <- function(df) {
  total_missing <- sapply(df, function(x) sum(is.na(x)))
  percent_missing <- sapply(df, function(x) mean(is.na(x)) * 100)
  summary_df <- data.frame(
    TotalMissing = total_missing,
    PercentMissing = percent_missing
  )
  summary_df <- summary_df[order(-summary_df$PercentMissing), ]
  return(summary_df)
}
missingness_byCol <- summarize_missingness(data)
print(missingness_byCol)

# Selecting variables for the two data frames:
# df1 has moca_impaired.y as its outcome, df2 has mocatot.y as its outcome.
df_list <- list(
  df1 = subset(data, select = -c(subnum.x, shannon.x, shannon.y, mocatot.y, GDS.y, GDS_impaired.y)),
  df2 = subset(data, select = -c(subnum.x, shannon.x, shannon.y, moca_impaired.y, GDS.y, GDS_impaired.y))
)

# Map each data frame to its outcome column
outcome_columns <- list(
  df1 = "moca_impaired.y",
  df2 = "mocatot.y"
)

# Perform hot deck imputation for df2 on its outcome column (if needed)
hotdeck_impute <- function(df, outcome_column) {
  original_columns <- names(df)
  df_imputed <- hotdeck(df, variable = names(df))
  df_imputed <- df_imputed[, original_columns]
  df_imputed[[outcome_column]] <- as.numeric(df_imputed[[outcome_column]])
  return(df_imputed)
}
df_list$df2 <- hotdeck_impute(df_list$df2, "mocatot.y")

# Function to run medDML analysis
run_medDML_analysis <- function(df, expo, expo_threshold, k_value, cov_gamma, outcome_column) {
  # Create a binary treatment variable based on the specified exposure variable and threshold
  df$treatment <- ifelse(df[[expo]] >= expo_threshold, 1, 0)
  
  result <- medDML(
    df[[outcome_column]],  # Outcome variable
    df$treatment,          # Treatment variable (binary)
    df$faith_pd.y,         # Mediator variable
    df[, cov_gamma],       # Covariate
    k = k_value,
    multmed = TRUE,
    fewsplits = TRUE,
    normalized = TRUE
  )
  return(result)
}

# Covariate columns for analysis
cov_Loneliness_00 <- c("faith_pd.x", "mencomp.x", "ldr2.x", "lotrt.x", "mlq_ps.x", "cse_sff.x", "nefftot.x", 
                       "wsdm_cd.x", "sdw_sa.x", "sdw_psb.x", "phycomp.x", "prsd8a_ss.x", "prsi_ss.x", "prdsa_ss.x")
cov_Loneliness_01 <- c("faith_pd.x", "ldr2.x", "lotrt.x", "mlq_ps.x", "cse_sff.x", "nefftot.x", 
                       "wsdm_cd.x", "sdw_sa.x", "sdw_psb.x", "phycomp.x", "prsd8a_ss.x", "prsi_ss.x", "prdsa_ss.x")
cov_Stimulus_00   <- c("faith_pd.x", "socposc.x", "lotrt.x", "nefftot.x", "wsdm_cd.x", "nsictot.x")
cov_Stimulus_01   <- c("faith_pd.x", "lotrt.x", "wsdm_cd.x")

# Define exposure specifications along with associated covariate sets.
# For expo = "uclalst.x" (threshold = 35) use the Loneliness covariate sets.
# For expo = "csascore.x" (threshold = 4) use the Stimulus covariate sets.
expo_variants <- list(
  uclalst = list(
    expo = "uclalst.x",
    threshold = 35,
    covariates = list(
      Loneliness_00 = cov_Loneliness_00,
      Loneliness_01 = cov_Loneliness_01
    )
  ),
  csascore = list(
    expo = "csascore.x",
    threshold = 4,
    covariates = list(
      Stimulus_00 = cov_Stimulus_00,
      Stimulus_01 = cov_Stimulus_01
    )
  )
)

# List to store the results
results <- list()

# Loop over each data frame in df_list
for (df_name in names(df_list)) {
  df <- df_list[[df_name]]
  
  # Use the outcome specified for this data frame
  outcome <- outcome_columns[[df_name]]
  
  # Loop over each exposure variant (uclalst.x and csascore.x)
  for (variant_name in names(expo_variants)) {
    variant <- expo_variants[[variant_name]]
    
    # Loop over each covariate set for the current exposure variant
    for (cov_name in names(variant$covariates)) {
      cov_gamma <- variant$covariates[[cov_name]]
      
      # Create a unique key and title for each result
      result_key <- paste(df_name, variant_name, cov_name, outcome, sep = "_")
      title <- paste("Treatment:", variant$expo, 
                     "| Outcome:", outcome, 
                     "| Covariate Set:", cov_name)
      
      # Run the medDML analysis
      analysis_result <- run_medDML_analysis(
        df = df,
        expo = variant$expo,
        expo_threshold = variant$threshold,
        k_value = 3,
        cov_gamma = cov_gamma,
        outcome_column = outcome
      )
      
      # Store the result along with the title
      results[[result_key]] <- list(title = title, result = analysis_result)
    }
  }
}

# Print the results with their titles
for (key in names(results)) {
  cat("Result key:", key, "\n")
  cat("Title:", results[[key]]$title, "\n")
  print(results[[key]]$result)
  cat("\n====================================\n\n")
}
