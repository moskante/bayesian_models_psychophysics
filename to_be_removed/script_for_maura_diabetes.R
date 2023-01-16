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

glmm_lower_m0 <- glmer(formula = cbind(faster, slower) ~ speed.cms * cluster_foot + (1 + speed.cms|id),
                       data =  dplyr::filter(data.raw, masking == 0),
                       family = binomial("probit"))
summary(glmm_lower_m0)

xplode.raw <- xplode(glmm_lower_m0, name.cont = "speed.cms", name.factor = "cluster_foot")
MixDelta(xplode.raw)

#  formula and fit when masking vibrations ARE present during the tactile sensitivity test

glmm_lower_m1 <- glmer(formula = cbind(faster, slower) ~ speed.cms * cluster_foot + (1 + speed.cms|id),
                       data =  dplyr::filter(data.raw, masking == 1),
                       family = binomial("probit"))
summary(glmm_lower_m1)



#Model predictions and plotting 

newdfr.list <- vector("list", 3)

for(i in 1:3){
  #select the cluster_foot level
  myclust <- levels(data.raw$cluster_foot)[[i]]
  
  # expland for cluster_foot level i
  newdfr.list[[i]] = expand_grid(
    speed.cms = seq(from = 0, to = 7, by = .1),
    cluster_foot = myclust,
    unique(data.raw[data.raw$cluster_foot == myclust, "id"])
  )
}


newdfr <- bind_rows(newdfr.list) %>%
  mutate(
    cluster_foot = as.factor(cluster_foot)
  )
newdfr0=newdfr
newdfr$masking=0

newdfr$predict.v0 = predict(glmm_lower_m0, type = "response", newdata = newdfr, allow.new.levels =T)
newdfr$predict.v1 = predict(glmm_lower_m1, type = "response", newdata = newdfr, allow.new.levels =T)

# user function to make plot of psychometric function --------------------------


plot_glmm_masking <- function(arg_masking){
  
  glmm.fig <-
    ggplot(data = dplyr::filter(data.raw, masking == arg_masking),
           aes(x = speed.cms, y = faster/(faster+slower), col = cluster_foot)) +
    ylab("Proportion of Faster Speed") +
    xlab("Speed [cm/s]") +
    geom_point() +
    geom_line(data = newdfr, aes(x = speed.cms, y = predict.v0)) +
    theme(legend.position= "none") +
    facet_wrap(~ cluster_foot + id, ncol = 6) +
    #scale_x_continuous(breaks = round(unique(data.raw$speed.cms), digits = 2)) +
    scale_x_continuous(breaks = c(0.63, 3.43, 6.24)) +
    scale_y_continuous(breaks = c(0.0, 0.5, 1.0))
  
  figure_name <- str_c("GLMM_V", arg_masking, ".pdf")
  
  ggsave(figure_name,
         glmm.fig, width = 140, height = 220, units = "mm",
         path ="Figures" )
  
  return(0)
}


glmm_fig_ll <- map(.x = c(0,1), .f = plot_glmm_masking)

