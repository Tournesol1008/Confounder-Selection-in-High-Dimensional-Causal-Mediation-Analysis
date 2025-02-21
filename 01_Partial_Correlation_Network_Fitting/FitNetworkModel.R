options(max.print = 1000)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(qgraph)
library(ggplot2)
library(bootnet)
library(mice)
library(corrplot)
library(clipr)

###################################################################
####################### Preprocess t1data  ########################
###################################################################

# Read in t1data (t1data is the data collected at baseline, t2 is collected 6 month after)
t1data<-read.csv("t1data.csv")

# Recode "Missing:not collected" as NA
t1data[t1data == "Missing:not collected"] <- NA
t1data[t1data == "NA"] <- NA

# Missingness
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

missingness_byCol <- summarize_missingness(t1data)
print(missingness_byCol)

# Remove npsyt_bc.x(currently taking birth control) and employed.x because not applied to elderly people
t1data <- subset(t1data, select = -c(smoke_c.x,npsyt_bc.x, employed.x))

# Perform multiple imputation
imputed_data <- mice(t1data, m = 10, method = 'pmm', maxit = 50, seed = 500)
t1data <- complete(imputed_data, 1)

# Identify character variables
# character_vars <- sapply(t1data, is.character)
# character_vars_names <- names(character_vars[character_vars])
# print(character_vars_names)

# check unique values for each character variables
# unique_values <- lapply(t1data[character_vars_names], unique)
# print(unique_values)

# Transform character to numercial data
# Remove highly-correlated character variables
t1data <- subset(t1data, select = -c(educ_cat.x, educ_sage.x,income_p.x, income_f.x, 
                                     marital3.x, hisp.x,race.x, race_lat2.x, educ2.x,
                                     ibm_group.x,lfq11f.x))
# Classify categorical variables
binary_var<-c("moca_impaired.x", "gender.x", "livsit.x","veteran.x","smoke_e.x",
              "ffi_frlty.x","ffi_ipf.x","bqsa_risk.x","psytr_lith.x","npsyt_ht.x",
              "npsyt_db.x","npsyt_chol.x","npsyt_horm.x")
ordinal_var<-c("income_p2.x","income_f2.x","alc_2.x")
categorial_var<-c("race_lat.x","imarital.x")

# Transform the binary vars
YN_binary <-c("veteran.x","smoke_e.x","ffi_frlty.x","ffi_ipf.x","psytr_lith.x","npsyt_ht.x",
       "npsyt_db.x","npsyt_chol.x","npsyt_horm.x")
t1data[YN_binary] <- lapply(t1data[YN_binary], function(x) ifelse(x == "Yes", 1, 0))
t1data$moca_impaired.x <- ifelse(t1data$moca_impaired.x == "Impaired", 1, 0)
t1data$gender.x <- ifelse(t1data$gender.x == "Male", 1, 0)  # Assuming 0 for Female
t1data$livsit.x <- ifelse(t1data$livsit.x == "Assisted Living", 1, 0)  # Assuming 0 for 13
t1data$bqsa_risk.x <- ifelse(t1data$bqsa_risk.x == "High Risk", 1, 0)  # Assuming 0 for Low Risk

# Transform categorical vars - one hot encoding
categorical_data <- t1data %>% dplyr::select(race_lat.x, imarital.x)
encoded_data <- model.matrix(~ race_lat.x + imarital.x - 1, data = categorical_data)
# Bind it with the original data
encoded_data_df <- as.data.frame(encoded_data)
t1data <- cbind(t1data, encoded_data_df)
# Remove the original categorical columns
t1data <- t1data %>% dplyr::select(-race_lat.x, -imarital.x)

# Transform ordinal vars
# Define the order
income_order <- c("<$35,000", "$35,000-$74,999", "$75,000+")
alc_order <- c("Lifetime Abstainer", "Former Drinker", "Current Infrequent Drinker", "Current Regular Drinker")
# Convert each ordinal variable to numeric
t1data$income_p2.x <- as.numeric(factor(t1data$income_p2.x, levels = income_order, ordered = TRUE))
t1data$income_f2.x <- as.numeric(factor(t1data$income_f2.x, levels = income_order, ordered = TRUE))
t1data$alc_2.x <- as.numeric(factor(t1data$alc_2.x, levels = alc_order, ordered = TRUE))

# if income_p2>income_f2 (personal income> family income), update income_f2 = income_p2
t1data$income_f2.x <- ifelse(t1data$income_p2.x > t1data$income_f2.x, t1data$income_p2.x, t1data$income_f2.x)

# Removing highly correlated variables
columns_to_remove <- c( "socposh.x","socposl.x",
                       "imarital.xMarried","imarital.xWidowed",
                       "npsytt.x","ths_ass.x","ths_pss.x",
                       "cei2_ss.x","cei2_es.x",
                       "bmmrs_ds.x","bmmrs_pp.x","ffi_frlty.x",
                       "sppb_csos.x","sppb_bos.x","sppb_gos.x",
                       "psytr_ot.x","wsdm_tot.x","sdw_tot.x",
                       "scbcstot.x")
t1data <- t1data[, !names(t1data) %in% columns_to_remove]

# Rename long fields name
new_names <- c( "AfricanAmerican"="race_lat.xAfrican American", "Asian"="race_lat.xAsian","Caucasian"='race_lat.xCaucasian',
                "Hispanic"='race_lat.xHispanic',"Divorced"='imarital.xDivorced', "Single"='imarital.xSingle')

names(t1data)[names(t1data) %in% new_names] <- names(new_names)[match(names(t1data), new_names, nomatch = 0)]


#write_clip(t1data)

# Check for multicollinearity by calculating the correlation matrix
# cor_matrix <- cor_auto(t1data)
# corrplot(cor_matrix, method = "circle")
# par(mfrow = c(1, 1), mar = c(3,3, 5, 3))
# corrplot(cor_matrix, method = "circle", tl.cex = 0.3)  

###################################################################
####################### Fit Network Model  ########################
###################################################################

# Assign groups to variables
group_labels <- c(
  rep("Loneliness", 2), 
  rep("Alpha Diversity", 1),  
  rep("Cognitive Functioning", 5),
  rep("Demographics", 10), 
  rep("Mental", 7),
  rep("Positive Psychological Traits",17),
  rep("Wisdom", 9),
  rep("Physical",20),
  rep("Medications",11),
  rep("Demographics",6)
)

# Fields description
items <- c("uclalst","csascore","faith_pd","moca_impaired","Cognitive Failures Questionnaire ",
           "UPSA Brief Total Score (Daily Functioning)",
           "Global Deficit Score on neuropsychological battery",
           "Executive Functioning 3-item Composite Score",
           "Age","Gender",
           "Living Situation",
           "Education-Total Years",
           "Veteran Status",
           "Hollingshead Index of Social Position-Current Status",
           "Personal Income","Family Income","Smoker Ever","Alcohol Use",
           "Brief Symptom Inventory Anxiety Scale Total Score",
           "Depression Scale 10-Item Total Score",
           "PHQ9 Severity Score",
           "SF-36 Mental Component Scale",
           "Self-Rated Successful Aging",
           "MacArthur Self-Perceived SES Ladder Scale",
           "Perceived Stress Scale Total Score",
           "Brief Multidimensional Measure of Religiousness - Overall Religiosity",
           "Connor Davidson Resilience Scale 10-item Total Score",
           "CESD Happiness Scale",
           "Life Events Scale Total Score","LOT-R Total Score (Optimism)",
           "Meaning of Life (MLQ) - Presence Subscale",
           "Meaning of Life (MLQ) - Search Subscale",
           "Trait Hope Scale - Total Score",
           "Curiosity and Exploration Inventory - Total Score",
           "Coping Humor Scale - Total Score",
           "Coping Self-Efficacy - Problem-Focused Coping Score",
           "Coping Self-Efficacy - Stop Unpleasant Emotions and Thoughts Score",
           "Coping Self-Efficacy - Get Support from Friends and Family Score",
           "Neff Self-Compassion Scale Total Score",
           "Personal Mastery Scale Total Score",
           "Satisfaction with Life (SWLS) Total Score",
           "Satisfaction with Finances",
           "3D Wisdom Scale - Cognitive dimension",
           "3D Wisdom Scale - Reflective dimension",
           "3D Wisdom Scale - Affective dimension",
           "SD-WISE - Social Advising",
           "SD-WISE - Decisiveness",
           "SD-WISE - Emotional Regulation",
           "SD-WISE - Insight",
           "SD-WISE - Pro-Scoial Behaviors",
           "SD-WISE - Tolerance for Divergent Values",
           "SPPB: Summary Ordinal Score",
           "Timed Get up and Go (TUG) Test Total Time",
           "Fried Frailty Index - Total Score",
           "Fried Frailty Index - Intermediate/Prefrail Criteria Met",
           "Fried Frailty Index - Grip Strength",
           "Fried Frailty Index - Walk Time",
           "SF-36 Physical Component Scale",
           "Nutrition Screening Checklist Total Score",
           "Berlin Sleep Apnea Risk Score",
           "PSQI Total Score",
           "Systolic Pressure",
           "Diastolic Pressure",
           "BMI",
           "Waist-to-Hip Ratio",
           "Cumulative Illness Rating Scale",
           "CIRS - Total Severity Score",
           "PROMIS Sleep Disturbances Scaled Score",
           "PROMIS Social Isolation Scaled Score",
           "PROMIS Companionship Scaled Score",
           "PROMIS Satisfaction with Discretionary Social Activities Scaled Score",
           "Total Current Anticholinergics",
           "Total Mood Stabilizers",
           "Total Anti-Depressents",
           "Total Anti-Anxiety",
           "Total Dementia",
           "Total Parkinson's",
            "Lithium",
            "Currently Taking Hypertension Medication",
            "Currently Taking Diabetes Medication",
            "Currently Taking Cholesterol Medication",
            "Currently Taking Hormone Medication",
           "Race-African American",
           "Race-Asian",
           "Race-Caucasian",
           "Race-Hispanic",
           "Marital-Divorced",
           "Marital-Single"
)

# Define a function to plot a network with your settings
plot_network <- function(net, filename) {
  png(filename, width = 6600, height = 4600, res = 1000)
  plot(net,
       layout = "spring",
       groups = group_labels,
       label.cex = 0.95,
       label.color = 'black',
       label.prop = 0.95,
       # Edges
       negDashed = TRUE,
       # Legend
       legend.cex = 0.1,
       legend.mode = 'style2',
       nodeNames = items,
       # Nodes
       vsize = 4,
       borders = TRUE,
       border.color = 'black',
       border.width = 0.2,
       shape = 'ellipse',
       vTrans = 100,
       font = 4)
  dev.off()
}

# Estimate the networks
network_0 <- estimateNetwork(
  t1data,
  default = "EBICglasso",
  corMethod = "spearman",
  tuning = 0
)
network_1 <- estimateNetwork(
  t1data,
  default = "EBICglasso",
  corMethod = "spearman",
  tuning = 0.1
)

# Plot and save both networks
plot_network(network_0, "qgraph_plot_0.png")
plot_network(network_1, "qgraph_plot_1.png")

# Check all variables connecting to the 2 exposure variables and the corresponding strength
# Helper function to extract connections for a given target variable
get_connections <- function(target, adj_matrix) {
  # Find the index of the target variable in the adjacency matrix
  target_index <- which(colnames(adj_matrix) == target)
  # Identify all variables with a non-zero connection (either in the target's row or column)
  connected_indices <- which(adj_matrix[target_index, ] != 0 | adj_matrix[, target_index] != 0)
  # Extract the variable names and remove the target itself from the list
  connected_variables <- colnames(adj_matrix)[connected_indices]
  connected_variables <- connected_variables[connected_variables != target]
  # Get the corresponding connection strengths and name them
  connection_strengths <- adj_matrix[target_index, connected_indices]
  names(connection_strengths) <- colnames(adj_matrix)[connected_indices]
  connection_strengths <- connection_strengths[names(connection_strengths) != target]
  # Return a list with both variables and their connection strengths
  return(list(variables = connected_variables, strengths = connection_strengths))
}

# List of networks and exposure variables
networks <- list(network_0 = network_0, network_1 = network_1)
targets <- c("csascore.x", "uclalst.x")

# Loop over each network and target variable, then print the results
for (net_name in names(networks)) {
  cat("Results for", net_name, ":\n")
  # Obtain the adjacency matrix for the network
  adj_matrix <- getWmat(networks[[net_name]])
  for (target in targets) {
    cat("\nConnected variables for", target, ":\n")
    connections <- get_connections(target, adj_matrix)
    print(connections$variables)
    cat("\nConnection strengths for", target, ":\n")
    print(connections$strengths)
    cat("\n------------------------------\n")
  }
  cat("\n=================================\n\n")
}


###############################################################################
####################### Calculate pairwise correlation ########################
###############################################################################

# Calculate pairwise correlations among confounders
confounder_cols <- names(t1data)[!names(t1data) %in% c("uclalst.x", "csascore.x",
                                                       "moca_impaired.x","cfq25_t.x",
                                                       "upsab_tot.x","excomp3.x","GDS.x")]
cor_matrix <- cor(t1data[, confounder_cols], use = "pairwise.complete.obs")

# Calculate the average correlation
avg_correlation <- mean(cor_matrix[upper.tri(cor_matrix)])
print(paste("Average correlation:", round(avg_correlation, 4)))

# Plot the figure
# Convert correlation matrix to a vector
cor_vector <- cor_matrix[upper.tri(cor_matrix)]

# Create a t1data frame for ggplot
plot_data <- data.frame(correlation = cor_vector)

# Create the histogram
ggplot(plot_data, aes(x = correlation)) +
  geom_histogram(binwidth = 0.05, fill = "white", color = "black") +
  geom_histogram(data = subset(plot_data, correlation >= 0.5), 
                 binwidth = 0.05, fill = "red", color = "black") +
  labs(x = "Correlations", y = "Frequency", 
       title = "Correlations between all probes in the subset") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("correlation_histogram.png", width = 8, height = 6, dpi = 300)

# Create a correlation heatmap for a more detailed view
png("correlation_heatmap.png", width = 3000, height = 3000, res = 500)

corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 90,  # Rotate text to vertical
         tl.cex = 0.4, # Reduce text size
         number.cex = 0.3, # Adjust size of correlation coefficients
         diag = FALSE, # Remove diagonal
         mar = c(0,0,2,0)) # Adjust margins

dev.off()

