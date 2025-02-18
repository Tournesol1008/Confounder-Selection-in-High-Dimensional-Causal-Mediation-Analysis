options(max.print = 1000)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(readxl)
library(mediation)
library(plyr)
library(dplyr)
library(mice)
library(VIM)
library(mgcv)

# Read in data
data <- read.csv("t1t2ImputedData.csv")

# Missingness
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
missingness_byCol <- summarize_missingness(data)
#print(missingness_byCol)

#Selecting variables - df1-df2. Change csascore.x to uclalst.x for comparison
df1 <- subset(data, select = -c(subnum.x,csascore.x, shannon.x, shannon.y, mocatot.y, GDS.y, GDS_impaired.y))
df2 <- subset(data, select = -c(subnum.x,csascore.x, shannon.x, shannon.y, moca_impaired.y, GDS.y, GDS_impaired.y))

################################################## DO NOT RUN AT ONCE #################################################################
## Loneliness - Binary MoCA(df1) vs Loneliness - Continous MoCA (df2) with Paritial Correlation Network Selection at gamma=0.1
########################################## For df1 ####################################################################################
# Exposure must be binary. 
df1$uclalst.x <- ifelse(df1$uclalst.x >= 35, 1, 0)
#df1$csascore.x <- ifelse(df1$csascore.x >= 4, 1, 0)

# Mediation analysis - uclalst.x - Binary MoCA with Network confounder Selection at Gamma=0.1
med.fit <- lm(faith_pd.y ~ uclalst.x +ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
              +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x, data = df1)
out.fit <- glm(moca_impaired.y ~ faith_pd.y + uclalst.x +ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
               +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,
               data = df1, family = binomial("probit"))

med.out <- mediate(med.fit, out.fit, treat = "uclalst.x", mediator = "faith_pd.y",boot=TRUE, robustSE = TRUE,
                   sims = 1000)
summary(med.out)
sensitivity <- medsens(med.out, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivity)

############################################ For df2 #################################
# Hot deck imputation using the hotdeck function
original_columns <- names(df2)
df2 <- hotdeck(df2, variable = names(df2))
df2 <- df2[, original_columns]
df2$mocatot.y <- as.numeric(df2$mocatot.y)

# Exposure must be binary.
df2$uclalst.x <- ifelse(df2$uclalst.x >= 35, 1, 0)

# Create control group indicator
df2$control <- ifelse(df2$uclalst.x == 0, 1, 0)

# Mediation analysis - uclalst.x - Continuous MoCA with Network confounder Selection at Gamma=0.1
med.fit <- lm(faith_pd.y ~ uclalst.x +ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
              +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x, 
              data = df2)
# Fit outcome model (using GAM with s() constructs)
out.fit <- gam(mocatot.y ~ s(uclalst.x, faith_pd.y,k=5, bs = "ad") +
                uclalst.x+faith_pd.y+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x+wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,
             family = gaussian(link = "identity"),
             method = "REML",
              data = df2)


# Perform mediation analysis
Med.out <- mediate(med.fit, out.fit, 
                   sims = 1000, 
                   treat = "uclalst.x", 
                   mediator = "faith_pd.y", 
                   control = "control",
                   boot = TRUE)
summary(Med.out)
sensitivity <- medsens(Med.out, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivity)

