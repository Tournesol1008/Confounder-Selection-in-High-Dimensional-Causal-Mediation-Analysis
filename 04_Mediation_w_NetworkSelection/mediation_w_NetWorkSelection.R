options(max.print = 1000)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(mediation)
library(dplyr)
library(mice)
library(VIM)
library(mgcv)

# Read in data
data <- read.csv("t1t2ImputedData.csv")


#####################################################################################################################################
## Loneliness - Binary MoCA(df1) vs Loneliness - Continous MoCA (df2) with Paritial Correlation Network Selection at gamma=0.1
########################################## For df1 ####################################################################################
#Selecting variables - df1-df2.
df1 <- subset(data, select = -c(subnum.x,csascore.x, shannon.x, shannon.y, mocatot.y, GDS.y, GDS_impaired.y))
df2 <- subset(data, select = -c(subnum.x,csascore.x, shannon.x, shannon.y, moca_impaired.y, GDS.y, GDS_impaired.y))

# Exposure must be binary. 
df1$uclalst.x <- ifelse(df1$uclalst.x >= 35, 1, 0)

# Mediation analysis - uclalst.x - Binary MoCA with Network confounder Selection at Gamma=0.1
med.fitL01B <- lm(faith_pd.y ~ uclalst.x +ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
              +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x, data = df1)
out.fitL01B <- glm(moca_impaired.y ~ faith_pd.y + uclalst.x +ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
               +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,
               data = df1, family = binomial("probit"))

med.outL01B <- mediate(med.fitL01B, out.fitL01B, treat = "uclalst.x", mediator = "faith_pd.y",boot=TRUE, robustSE = TRUE,
                   sims = 1000)
summary(med.outL01B)
sensitivityL01B <- medsens(med.outL01B, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityL01B)



# Mediation analysis - uclalst.x - Binary MoCA with Network confounder Selection at Gamma=0
med.fitL00B <- lm(faith_pd.y ~ uclalst.x +mencomp.x+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
                +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x, data = df1)
out.fitL00B <- glm(moca_impaired.y ~ faith_pd.y + uclalst.x +mencomp.x+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
                 +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,
                 data = df1, family = binomial("probit"))

med.outL00B <- mediate(med.fitL00B, out.fitL00B, treat = "uclalst.x", mediator = "faith_pd.y",boot=TRUE, robustSE = TRUE,
                     sims = 1000)
summary(med.outL00B)
sensitivityL00B <- medsens(med.outL00B, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityL00B)
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
med.fitL01C <- lm(faith_pd.y ~ uclalst.x +ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
              +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x, 
              data = df2)
# Fit outcome model (using GAM with s() constructs)
out.fitL01C <- gam(mocatot.y ~ s(uclalst.x, faith_pd.y,k=5, bs = "ad") +
                uclalst.x+faith_pd.y+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x+wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,
             family = gaussian(link = "identity"),
             method = "REML",
              data = df2)
# for sensitivity analysis
out.fitL01C_sens <- lm(mocatot.y ~ faith_pd.y+uclalst.x +uclalst.x:faith_pd.y+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x+wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,data=df2)

# Perform mediation analysis
Med.outL01C <- mediate(med.fitL01C, out.fitL01C, 
                   sims = 1000, 
                   treat = "uclalst.x", 
                   mediator = "faith_pd.y", 
                   control = "control",
                   boot = TRUE)

Med.outL01C_sens <- mediate(med.fitL01C, out.fitL01C_sens, 
                       sims = 1000, 
                       treat = "uclalst.x", 
                       mediator = "faith_pd.y", 
                       boot = TRUE)
summary(Med.outL01C)
sensitivityL01C <- medsens(Med.outL01C_sens, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityL01C)


# Mediation analysis - uclalst.x - Continuous MoCA with Network confounder Selection at Gamma=0
med.fitL00C <- lm(faith_pd.y ~ uclalst.x +ldr2.x+mencomp.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x
                  +wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x, 
                  data = df2)
# Fit outcome model (using GAM with s() constructs)
out.fitL00C <- gam(mocatot.y ~ s(uclalst.x, faith_pd.y,k=5, bs = "ad") +
                     uclalst.x+faith_pd.y+mencomp.x+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x+wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,
                   family = gaussian(link = "identity"),
                   method = "REML",
                   data = df2)
# for sensitivity analysis
out.fitL00C_sens <- lm(mocatot.y ~ faith_pd.y+uclalst.x +uclalst.x:faith_pd.y+ldr2.x+lotrt.x+mlq_ps.x+cse_sff.x+nefftot.x+wsdm_cd.x+sdw_sa.x+sdw_psb.x+phycomp.x+prsd8a_ss.x+prsi_ss.x+prdsa_ss.x+faith_pd.x,data=df2)

# Perform mediation analysis
Med.outL00C <- mediate(med.fitL00C, out.fitL00C, 
                       sims = 1000, 
                       treat = "uclalst.x", 
                       mediator = "faith_pd.y", 
                       control = "control",
                       boot = TRUE)
Med.outL00C_sens <- mediate(med.fitL00C, out.fitL00C_sens, 
                       sims = 1000, 
                       treat = "uclalst.x", 
                       mediator = "faith_pd.y", 
                       boot = TRUE)

summary(Med.outL00C)
sensitivityL00C <- medsens(Med.outL00C_sens, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityL00C)




#################################################################### #################################################################
## Stimulus - Binary MoCA(df1) vs Stimulus - Continous MoCA (df2) with Paritial Correlation Network Selection at gamma=0.1
########################################## For df1 ####################################################################################
df1 <- subset(data, select = -c(subnum.x,uclalst.x, shannon.x, shannon.y, mocatot.y, GDS.y, GDS_impaired.y))
df2 <- subset(data, select = -c(subnum.x,uclalst.x, shannon.x, shannon.y, moca_impaired.y, GDS.y, GDS_impaired.y))

# Exposure must be binary. 
df1$csascore.x <- ifelse(df1$csascore.x >= 4, 1, 0)

# Mediation analysis - csascore.x - Binary MoCA with Network confounder Selection at Gamma=0.1
med.fitC01B <- lm(faith_pd.y ~ csascore.x +lotrt.x+wsdm_cd.x+faith_pd.x, data = df1)
out.fitC01B <- glm(moca_impaired.y ~ faith_pd.y + csascore.x +lotrt.x+wsdm_cd.x+faith_pd.x,
                   data = df1, family = binomial("probit"))

med.outC01B <- mediate(med.fitC01B, out.fitC01B, treat = "csascore.x", mediator = "faith_pd.y",boot=TRUE,
                       sims = 1000)
summary(med.outC01B)
sensitivityC01B <- medsens(med.outC01B, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityC01B)



# Mediation analysis - csascore.x - Binary MoCA with Network confounder Selection at Gamma=0
med.fitC00B <- lm(faith_pd.y ~ csascore.x +socposc.x+lotrt.x+nefftot.x+wsdm_cd.x+nsictot.x+faith_pd.x, data = df1)
out.fitC00B <- glm(moca_impaired.y ~ faith_pd.y + csascore.x +socposc.x+lotrt.x+nefftot.x+wsdm_cd.x+nsictot.x+faith_pd.x,
                   data = df1, family = binomial("probit"))

med.outC00B <- mediate(med.fitC00B, out.fitC00B, treat = "csascore.x", mediator = "faith_pd.y",boot=TRUE, 
                       sims = 1000)
summary(med.outC00B)
sensitivityC00B <- medsens(med.outC00B, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityC00B)
############################################ For df2 #################################
# Hot deck imputation using the hotdeck function
original_columns <- names(df2)
df2 <- hotdeck(df2, variable = names(df2))
df2 <- df2[, original_columns]
df2$mocatot.y <- as.numeric(df2$mocatot.y)

# Exposure must be binary.
df2$csascore.x <- ifelse(df2$csascore.x >= 4, 1, 0)

# Create control group indicator
df2$control <- ifelse(df2$csascore.x == 0, 1, 0)

# Mediation analysis - csascore.x - Continuous MoCA with Network confounder Selection at Gamma=0.1
med.fitC01C <- lm(faith_pd.y ~ csascore.x +lotrt.x+wsdm_cd.x+faith_pd.x, 
                  data = df2)
# Fit outcome model (using GAM with s() constructs)
out.fitC01C <- gam(mocatot.y ~ s(csascore.x, faith_pd.y,k=5, bs = "ad") +
                     csascore.x+faith_pd.y+lotrt.x+wsdm_cd.x+faith_pd.x,
                   family = gaussian(link = "identity"),
                   method = "REML",
                   data = df2)
# for sensitivity analysis
out.fitC01C_sens <- lm(mocatot.y ~ faith_pd.y+csascore.x +csascore.x:faith_pd.y+lotrt.x+wsdm_cd.x+faith_pd.x,data=df2)

# Perform mediation analysis
Med.outC01C <- mediate(med.fitC01C, out.fitC01C, 
                       sims = 1000, 
                       treat = "csascore.x", 
                       mediator = "faith_pd.y", 
                       control = "control",
                       boot = TRUE)
Med.outC01C_sens <- mediate(med.fitC01C, out.fitC01C_sens, 
                       sims = 1000, 
                       treat = "csascore.x", 
                       mediator = "faith_pd.y", 
                       boot = TRUE)

summary(Med.outC01C)
sensitivityC01C <- medsens(Med.outC01C_sens, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityC01C)


# Mediation analysis - csascore.x - Continuous MoCA with Network confounder Selection at Gamma=0
med.fitC00C <- lm(faith_pd.y ~ csascore.x +socposc.x+lotrt.x+nefftot.x+wsdm_cd.x+nsictot.x+faith_pd.x, 
                  data = df2)
# Fit outcome model (using GAM with s() constructs)
out.fitC00C <- gam(mocatot.y ~ s(csascore.x, faith_pd.y,k=5, bs = "ad") +
                     csascore.x+faith_pd.y+socposc.x+lotrt.x+nefftot.x+wsdm_cd.x+nsictot.x+faith_pd.x,
                   family = gaussian(link = "identity"),
                   method = "REML",
                   data = df2)
# for sensitivity analysis
out.fitC00C_sens <- lm(mocatot.y ~ faith_pd.y+csascore.x +csascore.x:faith_pd.y+socposc.x+lotrt.x+nefftot.x+wsdm_cd.x+nsictot.x+faith_pd.x,data=df2)

# Perform mediation analysis
Med.outC00C <- mediate(med.fitC00C, out.fitC00C, 
                       sims = 1000, 
                       treat = "csascore.x", 
                       mediator = "faith_pd.y", 
                       control = "control",
                       boot = TRUE)
Med.outC00C_sens <- mediate(med.fitC00C, out.fitC00C_sens, 
                       sims = 1000, 
                       treat = "csascore.x", 
                       mediator = "faith_pd.y", 
                       boot = TRUE)

summary(Med.outC00C)
sensitivityC00C <- medsens(Med.outC00C_sens, rho.by = 0.1, sims = 1000,
                       eps = sqrt(.Machine$double.eps), effect.type = "indirect")
summary(sensitivityC00C)