# Script for the GLMM data with diabetes patients
# We are using the data from data.raw.
# Masking vibrations means that during those trials, high frequency vibrations were presented to the finger.
# Cluster foot represents the group that the participants were divided into based on vibration sensation of the foot (assessed by a tool called Biothesiometer).
# Ctr are healthy participants and the bio0 & bio1 are diabetic patients
# We analyzed the data twice, first altogether with and without masking vibrations (glmm_lower) and then we split by masking (glmm_lower_m0 and glmm_lower_m1)
# The plots are 2: non masking vibrations condition and masking vibrations condition across participants. 
# The red group is the control group, blue is bio0 which means there were no foot vibration sensation alterations
# green participants are part of the bio1 group which had shown alterations of vibration sensation to the foot


# load libraries----------------------------------------------------------------

library(lme4)
library(tidyverse)
library(fastDummies)
library(rjags)

# load the data-----------------------------------------------------------------

#load("data.raw.RData")
load("esperimento2.RData")

attach(data.raw)

dummyid=dummy_cols(id)
dummycluster=dummy_cols(cluster_foot)

# cluster 1 = bio1/moderate
# cluster 3 = bio0/mild
# cluster 2 = ctr
cluster=1+dummycluster[,2]+2*dummycluster[,3]
maskingnew=(masking=="1")
soggetto=rep(0,600)
clusters=rep(0,60)
masks=rep(0,60)

# subjects in alphabetical order
for (i in 1:60)
{soggetto=soggetto+i*dummyid[,i+1]}

for (i in 1:60)
{clusters[i]=mean(cluster[soggetto==i])
masks[i]=mean(maskingnew[soggetto==i])

}

dummycluster=dummy_cols(cluster_foot)

speedmasking=speed.cms
speedmasking[masking==0]=0


input=list(noss=600,y=faster,n=faster+slower,x=speed.cms,nsubj=60,subject=soggetto,cluster=clusters,mask=maskingnew+1)

#Set the following line to TRUE if you would like to save all the figures
do_save <- FALSE

# run jags model at individual level--------------------------------------------

modello1 <- jags.model("modello_probit_pse_dati2.txt",data=input,n.chains=3)

update(modello1, 50000)

parameters=c("aa","bb")


snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)

param=matrix(0,12,3)
for (i in 1:12)
{param[i,1]=mean(y1[,i])
param[i,2:3]=quantile(y1[,i],c(.025,.975))
}

# figures 7 and 8: denisty plots for pse and slope-----------------------------------------------

diab_marginal <- list()

# cluster 1 = moderate
# cluster 3 = mild
# cluster 2 = controls

diab_marginal[["pse"]] <- as_tibble(y1[,1:6]) %>%
  rename(
    moderate_0 = `aa[1,1]`, # ie BIO1
    controls_0 = `aa[2,1]`, 
    mild_0 = `aa[3,1]`, # ie, BIO0
    moderate_1 = `aa[1,2]` ,
    controls_1 = `aa[2,2]`, 
    mild_1 = `aa[3,2]`
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = c("group", "masking"), names_sep = "_", values_to = "estimate") %>%
  mutate(
    sample = rep(1: 5000, 6),
    masking = ifelse(masking == 0, "no masking", "masking")
  )


diab_marginal[["slope"]] <- as_tibble(y1[,7:12]) %>%
  rename(
    moderate_0 = `bb[1,1]`,
    controls_0 = `bb[2,1]`, 
    mild_0 = `bb[3,1]`,
    moderate_1 = `bb[1,2]` ,
    controls_1 = `bb[2,2]`, 
    mild_1 = `bb[3,2]`
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = c("group", "masking"), names_sep = "_", values_to = "estimate") %>%
  mutate(
    sample = rep(1: 5000, 6),
    masking = ifelse(masking == 0, "no masking", "masking")
    )

do_plot_diab <- function(data, filename, do_save = FALSE){
  
  save_plot <- ggplot(data = data,
                      mapping = aes(x = estimate, color = group)) +
    geom_density()+
    facet_wrap(~ masking) +
    theme(text = element_text(size = 12))
  
  if(do_save == TRUE){
    ggsave(filename, save_plot, width = 85, height = 60, units = "mm")
  }
  
  return(save_plot)
  
}

# figures 7 and 8
# here we used the function map2 of the map family instead of the for loop
# set do_save to TRUE in line 123 to export the plots

plot_diabetes_marginal <-  map2(diab_marginal, list("diabetes_marginal_pse.pdf", "diabetes_marginal_slope.pdf"),  do_plot_diab )

# run jags model at population level--------------------------------------------

update(modello1, 50000)


parameters=c("alpha","beta")


snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)


# recover ids from the dummy matrix---------------------------------------------
id_names <- names(dummyid)[-1] %>%
  str_remove(".data_") 

# vector of names in form of id_masking
id_names_to <- list()

masking_condition <- c("_0", "_1")

for(i in 1:2){
  id_names_to[[i]] <- str_c(id_names, masking_condition[[i]])
}

id_names_vector <- c(id_names_to[[1]], id_names_to[[2]])




# PSE and Slope
# diabetes
diab_indiv <- list(pse = as_tibble(y1[,1:120]), slope = as_tibble(y1[,121:240]))

# cluster 1 = bio1/moderate
# cluster 3 = bio0/mild
# cluster 2 = ctr
names_by_group <- list()
names_cluster <- c("ctr", "bio0", "bio1")

for(j in 1:3){
  
  names_by_group[[j]] <- data.raw %>%
    filter(cluster_foot == names_cluster[[j]]) %>% # this can be updated
    select(id) %>%
    unique()
  
}


for(parameter in c("pse", "slope")){
  
  #This is for the individual PSE renames as subjects and masking 0,1
  # names(diab_indiv[[parameter]]) <- names_individuals_diab 
  names(diab_indiv[[parameter]]) <- id_names_vector
  
  # Now do pivot longer to move all the values into one column 
  diab_indiv[[parameter]]  <-  diab_indiv[[parameter]]%>%
    pivot_longer(cols = everything(), 
                 names_to = c("subject", "masking"), names_sep = "_", values_to = "estimate") %>%
    mutate(
      masking = ifelse(masking == 0, "no masking", "masking"),
      group = ifelse(subject %in% names_by_group[[1]]$id, "controls", 
                     ifelse(subject %in% names_by_group[[2]]$id, "mild", "moderate") )
    )
  
# figures 9 and 10: density plots for each cluster-------------------------------
  save_plot <- 
    ggplot(data = diab_indiv[[parameter]], 
                      mapping = aes(x = estimate, group = subject, color = group)) +
    geom_density()+
    facet_grid(group ~ masking)
  
  if(do_save == TRUE){
    filename <- str_c("diabetes_subjects_", parameter, ".pdf")
    ggsave(filename, save_plot)

  }
}

summary_data <- diab_indiv[["slope"]] %>%
  group_by(masking, group) %>%
  summarise(
    mean_by = mean(estimate)
  )



