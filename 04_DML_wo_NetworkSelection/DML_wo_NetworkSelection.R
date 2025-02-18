# ---------------------------
# Load libraries and data
# ---------------------------
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(mice)
library(causalweight)
library(VIM)

# Read in data
data <- read.csv("t1t2ImputedData.csv")

# Selecting variables - create two data frames (each missing one of the outcomes)
df_list <- list(
  df1 = subset(data, select = -c(subnum.x, shannon.x, shannon.y, mocatot.y, GDS.y, GDS_impaired.y)),
  df2 = subset(data, select = -c(subnum.x, shannon.x, shannon.y, moca_impaired.y, GDS.y, GDS_impaired.y))
)

# Perform hot deck imputation for df2 on its outcome if needed
hotdeck_impute <- function(df, outcome_column) {
  original_columns <- names(df)
  df_imputed <- hotdeck(df, variable = names(df))
  df_imputed <- df_imputed[, original_columns]
  df_imputed[[outcome_column]] <- as.numeric(df_imputed[[outcome_column]])
  return(df_imputed)
}
df_list$df2 <- hotdeck_impute(df_list$df2, "mocatot.y")

# ---------------------------
# Analysis settings
# ---------------------------

# Covariate columns for analysis (all covariates to be used)
column_names <- c("faith_pd.x", "agevisit.x", "gender.x", "livsit.x", "educ.x", "veteran.x", "socposc.x", 
                  "income_p2.x", "income_f2.x", "smoke_e.x", "alc_2.x", "bsiatot.x", "cesdt10.x", "phq9ss.x", 
                  "mencomp.x", "srsa.x", "ldr2.x", "psstot.x", "bmmrs_or.x", "cdrs10_t.x", "cesdhs.x", "lest.x", 
                  "lotrt.x", "mlq_ps.x", "mlq_ss.x", "ths_tot.x", "cei2_tot.x", "chs_tot.x", "cse_pfc.x", "cse_suet.x", 
                  "cse_sff.x", "nefftot.x", "pmst_new.x", "swlstot.x", "swfinan.x", "wsdm_cd.x", "wsdm_rd.x", "wsdm_ad.x", 
                  "sdw_sa.x", "sdw_d.x", "sdw_er.x", "sdw_i.x", "sdw_psb.x", "sdw_tdv.x", "sppb_sos.x", "tug_tt.x", 
                  "ffi_tfs.x", "ffi_ipf.x", "ffi_grip.x", "ffi_walk.x", "phycomp.x", "nsictot.x", "bqsa_risk.x", 
                  "psqitot.x", "sys.x", "dia.x", "bmi.x", "whip1cm.x", "cirs_mnce.x", "cirs_mtot.x", "prsd8a_ss.x", 
                  "prsi_ss.x", "prcmp_ss.x", "prdsa_ss.x", "ancoltot.x", "psytr_ms.x", "psytr_ad.x", "psytr_aa.x", 
                  "psytr_dm.x", "psytr_pk.x", "psytr_lith.x", "npsyt_ht.x", "npsyt_db.x", "npsyt_chol.x", "npsyt_horm.x", 
                  "AfricanAmerican", "Asian", "Caucasian", "Hispanic", "Divorced", "Single")

# Define exposure variants along with the covariate set to be used (all covariates here)
expo_variants <- list(
  uclalst = list(
    expo = "uclalst.x",
    threshold = 35,
    covariates = column_names
  ),
  csascore = list(
    expo = "csascore.x",
    threshold = 4,
    covariates = column_names
  )
)

# Define the outcomes of interest
outcomes <- c("moca_impaired.y", "mocatot.y")

# ---------------------------
# Modified run_medDML_analysis function (for given expo settings)
# ---------------------------
run_medDML_analysis <- function(df, expo, expo_threshold, k_value, covariates, outcome_column) {
  # Create a binary treatment variable based on the specified exposure variable and threshold
  df$treatment <- ifelse(df[[expo]] >= expo_threshold, 1, 0)
  
  result <- medDML(
    df[[outcome_column]],  # Outcome variable
    df$treatment,          # Treatment variable (binary)
    df$faith_pd.y,         # Mediator variable (assumed fixed)
    df[, covariates],      # Covariate matrix
    k = k_value,
    multmed = TRUE,
    fewsplits = TRUE,
    normalized = TRUE
  )
  return(result)
}

# ---------------------------
# Loop through all combinations
# ---------------------------
results <- list()

for (df_name in names(df_list)) {
  df <- df_list[[df_name]]
  
  # Loop over each exposure variant (uclalst.x and csascore.x)
  for (variant_name in names(expo_variants)) {
    variant <- expo_variants[[variant_name]]
    
    # Loop over each outcome variable
    for (outcome in outcomes) {
      # Check if the outcome exists in the current data frame
      if (!(outcome %in% names(df))) {
        cat("Skipping outcome", outcome, "in", df_name, "because it is not present.\n")
        next
      }
      
      # Create a unique key for the results list
      result_key <- paste(df_name, variant_name, outcome, sep = "_")
      
      # Create a title containing the treatment variable, outcome variable, and exposure settings
      title <- paste("Treatment:", variant$expo,
                     "| Outcome:", outcome,
                     "| Exposure Threshold:", variant$threshold,
                     "| Covariates: all")
      
      # Run the medDML analysis
      analysis_result <- run_medDML_analysis(
        df = df,
        expo = variant$expo,
        expo_threshold = variant$threshold,
        k_value = 3,
        covariates = variant$covariates,
        outcome_column = outcome
      )
      
      # Store the result along with its title
      results[[result_key]] <- list(title = title, result = analysis_result)
    }
  }
}

# ---------------------------
# Print the results with titles
# ---------------------------
for (key in names(results)) {
  cat("Result key:", key, "\n")
  cat("Title:", results[[key]]$title, "\n")
  print(results[[key]]$result)
  cat("\n====================================\n\n")
}
