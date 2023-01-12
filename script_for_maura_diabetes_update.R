# Script for the GLMM data with diabetes patients
# We are using the data from data.raw.
# Masking vibrations means that during those trials, high frequency vibrations were presented to the finger.
# Cluster foot represents the group that the participants were divided into based on vibration sensation of the foot (assessed by a tool called Biothesiometer).
# Ctr are healthy participants and the bio0 & bio1 are diabetic patients
# We analyzed the data twice, first altogether with and without masking vibrations (glmm_lower) and then we split by masking (glmm_lower_m0 and glmm_lower_m1)
# The plots are 2: non masking vibrations condition and masking vibrations condition across participants. 
# The red group is the control group, blue is bio0 which means there were no foot vibration sensation alterations
# green participants are part of the bio1 group which had shown alterations of vibration sensation to the foot


# Load libraries----------------------------------------------------------------

library(lme4)
library(tidyverse)

# First load the neurodiab_backup_2021-11-29 file 

load("neurodiab_backup_2021-11-29.RData")


# Glmm formula and data fit-----------------------------------------------------


glmm_lower <- glmer(formula = cbind(faster, slower) ~ speed.cms * cluster_foot * masking + (cluster_foot+ masking + speed.cms|id),
                    data =  data.raw,
                    family = binomial("probit"))
summary(glmm_lower)
xplode.raw <- xplode(glmm_lower, name.cont = "speed.cms", name.factor = "masking")
MixDelta(xplode.raw)


# formula and fit only when masking vibrations are NOT present during the tactile sensitivity test

speed.cms0=data.raw$speed.cms*(data.raw$cluster_foot=="ctr" )
speed.cms1=data.raw$speed.cms*(data.raw$cluster_foot=="bio0" )
speed.cms2=data.raw$speed.cms*(data.raw$cluster_foot=="bio1" )
glmm_m0 <- glmer(formula = cbind(faster, slower) ~  cluster_foot +speed.cms0[data.raw$masking==0]+speed.cms1[data.raw$masking==0]+speed.cms2[data.raw$masking==0] +(1 + speed.cms|id),
                       data =  dplyr::filter(data.raw, masking == 0),
                       family = binomial("probit"))
summary(glmm_m0)

xplode.raw <- xplode(glmm_lower_m0, name.cont = "speed.cms", name.factor = "cluster_foot")
MixDelta(xplode.raw)

#  formula and fit when masking vibrations ARE present during the tactile sensitivity test


speed.cms0=data.raw$speed.cms*(data.raw$cluster_foot=="ctr" )
speed.cms1=data.raw$speed.cms*(data.raw$cluster_foot=="bio0" )
speed.cms2=data.raw$speed.cms*(data.raw$cluster_foot=="bio1" )

glmm_m1 <- glmer(formula = cbind(faster, slower) ~  cluster_foot +speed.cms0[data.raw$masking==1]+speed.cms1[data.raw$masking==1]+speed.cms2[data.raw$masking==1] +(1 + speed.cms|id),
                 data =  dplyr::filter(data.raw, masking == 1),
                 family = binomial("probit"))
summary(glmm_m1)


