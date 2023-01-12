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
library(fastDummies)
library(rjags)

# First load the neurodiab_backup_2021-11-29 file 

load("data.raw.RData")


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

# soggetti in ordine alfabetico
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
y2

param=matrix(0,12,3)
for (i in 1:12)
{param[i,1]=mean(y1[,i])
param[i,2:3]=quantile(y1[,i],c(.025,.975))
}


# figure 6 ------------------------------------------------------------------------
# PSE and Slope
# diabetes
diab_marginal <- list()

# cluster 1 = bio1/moderate
# cluster 3 = bio0/mild
# cluster 2 = ctr

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

do_plot_diab <- function(data, filename){
  
  save_plot <- ggplot(data = data,
                      mapping = aes(x = estimate, color = group)) +
    geom_density()+
    facet_wrap(~ masking) +
    theme(text = element_text(size = 12), legend.position = "none")
  
  ggsave(filename, save_plot, width = 85, height = 60, units = "mm")
  
  return(save_plot)
  
}

plot_diabetes_marginal <-  map2(diab_marginal, list("diabetes_marginal_pse.pdf", "diabetes_marginal_slope.pdf"),  do_plot_diab )

#-------------------------------------------------------------------------------------

update(modello1, 50000)


parameters=c("alpha","beta")


snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

# --------------------------------------------------------------------------------
# scatterplot

# data_id <- data.raw %>%
#   filter(masking == 0) %>%
#   select(id, cluster_foot) %>%
#   group_by(cluster_foot) %>%
#   group_split()
#   
# # not working - match with id
# # sort id in alfabetic order!
# y2_tibble <- tibble(
#   estimate = y2,
#   parameter = c(rep("pse", 120), rep("slope", 120)),
#   masking = rep(c(rep(0,60),rep(1,60)),2),
#   group = rep(c(rep("moderate", 20), rep("controls", 20), rep("mild", 20)),4),
#   participant = rep(str_c("S", 1:60), 4),
#   id = rep(c(unique(data_id[[3]]$id), unique(data_id[[1]]$id), unique(data_id[[2]]$id)),4) # alphabetic order by clusters - not working...
#   #id = rep(unique(data.raw$id), 4) # alphabetic order - not working
#   ) %>%
#   pivot_wider(
#     names_from = parameter,
#     values_from = estimate
#   )%>%
#   expand_grid(speed.cms = seq(from = 0, to = 7, by = .1)) %>%
#   mutate(
#     intercept = - (pse * slope),
#     predictions = pnorm(q = speed.cms, mean = pse, sd = slope)
#   )
# 
# 
# ggplot(dplyr::filter(data.raw, id %in% c("BeRu", "mavi0602", "FaPo")),
#        mapping = aes(x = speed.cms, y = proportion, col = as.factor(masking))) +
#   geom_point()+
#   geom_line(dplyr::filter(y2_tibble, id %in% c("BeRu", "mavi0602", "FaPo") ),
#             mapping = aes(x = speed.cms, y = predictions, col = as.factor(masking))) +
#   ylab("Proportion of Faster Speed") +
#   xlab("Speed [cm/s]") +
#   facet_wrap(~ id, ncol = 3) +
#   #geom_line(data = newdfr.ctr, aes(x = speed.cms, y = predict.glmm)) +
#   theme(legend.position= "none") +
#   scale_x_continuous(breaks = c(0.63, 3.43, 6.24)) +
#   scale_y_continuous(breaks = c(0.0, 0.5, 1.0)) +
#   scale_color_grey(start = 0.2, end = 0.8)
# 

# ------------------------------------------------------------------------------
# density plot Figure 7

# recover ids from the dummy matrix
id_names <- names(dummyid)[-1] %>%
  str_remove(".data_") 

# vector of names in form of id_masking
id_names_to <- list()

masking_condition <- c("_0", "_1")

for(i in 1:2){
  id_names_to[[i]] <- str_c(id_names, masking_condition[[i]])
}

id_names_vector <- c(id_names_to[[1]], id_names_to[[2]])

# runs loop across participants and for masking

# names_individuals_diab <- vector(mode = "character", length = 120)
# names_individuals_diab_b <- vector(mode = "character", length = 60)
# counter <- 0
# 
# for(i in 0:1){
#   for(k in 1:60){
#     counter <- counter + 1
#     names_individuals_diab[[counter]] <- str_c("s", k, "_", i)
#     
#     if(i == 0){
#       names_individuals_diab_b[[counter]] <- str_c("s", k)
#     }
#   }
# }


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
      # group = ifelse(subject %in% names_individuals_diab_b[1:20], "moderate", 
      #                ifelse(subject %in% names_individuals_diab_b[21:40], "controls", "mild") )
    )
  
  # density plot
  save_plot <- 
    ggplot(data = diab_indiv[[parameter]], 
                      mapping = aes(x = estimate, group = subject, color = group)) +
    geom_density()+
    facet_grid(group ~ masking) +
    theme(legend.position = "none")
  
  filename <- str_c("diabetes_subjects_", parameter, ".pdf")
  
  ggsave(filename, save_plot)
  
}

summary_data <- diab_indiv[["slope"]] %>%
  group_by(masking, group) %>%
  summarise(
    mean_by = mean(estimate)
  )


#-------------------------------------------------------------------------------------

write.csv(y1,"y1individual_pse_dati2.csv")


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
y2

param=matrix(0,12,3)
for (i in 1:12)
{param[i,1]=mean(y1[,i])
param[i,2:3]=quantile(y1[,i],c(.025,.975))
}