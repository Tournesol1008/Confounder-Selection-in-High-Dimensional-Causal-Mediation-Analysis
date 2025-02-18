options(max.print = 1000)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(mice)

# Read in data
data<-read.csv("t1t2data.csv")

# Delete 2 useless outcome variables
data <- data[ , !(names(data) %in% c("cfq25_t.y", "excomp3.y"))]

# Recode "Missing:not collected" as NA
data[data == "Missing:not collected"] <- NA
data[data == "NA"] <- NA

# Check missing values
# Function to summarize missingness by columns
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

# Missingness by columns
missingness_byCol <- summarize_missingness(data)
print(missingness_byCol)

# Convert character data to numerical
data$moca_impaired.y <- ifelse(data$moca_impaired.y == "Impaired", 1, 0)
data$GDS_impaired.y <- ifelse(data$GDS_impaired.y == "Impaired", 1, 0)

# Perform multiple imputation
imputed_data <- mice(data, m =6, method = 'pmm', maxit = 10, seed = 500)
data <- complete(imputed_data, 1)
sum(data$moca_impaired.y)
sum(data$GDS_impaired.y)

# save for other comparative analysis
write.csv(data, file = "t1t2ImputedData.csv", row.names = FALSE)


