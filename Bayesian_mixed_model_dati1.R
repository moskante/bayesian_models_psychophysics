# load libraries ---------------------------------------------------------------
library(MixedPsy)
library(lme4)
library(rjags)
library(tidyverse)

# load data --------------------------------------------------------------------
# Data for the first experiment are included in the package MixedPsy. The next line is to load the data. 

data("vibro_exp3")

vibro_exp3 <- vibro_exp3 %>%
  mutate(
    speed.cms = speed,
    proportion = faster/(faster+slower),
    masking = ifelse(vibration == 0, "no masking", "masking")
  )

# maybe without attach data
attach(vibro_exp3)

# fitting glmm -----------------------------------------------------------------
# This is the general linear mixed model as the one in Dallmann et al., 2015. 

glmm.vibro <- glmer(cbind(faster, slower) ~ speed.cms + masking + speed.cms:masking +
                      (1+speed.cms| subject),
                    family = binomial(link = "probit"),
                    data = vibro_exp3)

# fitting model with jags-------------------------------------------------------

# Variables need to be passed to Jags as a list and this is why we created new 
# vectors for each variable below

vibr=0*speed
vibr[vibration=="32"]=1
interazione=speed*vibr

soggetto=0*speed
soggetto[subject=="AK"]=1
soggetto[subject=="AR"]=2
soggetto[subject=="DN"]=3
soggetto[subject=="FA"]=4
soggetto[subject=="MA"]=5
soggetto[subject=="MI"]=6
soggetto[subject=="NI"]=7
soggetto[subject=="NN"]=8
soggetto[subject=="RV"]=9

input=list(noss=126,y=faster,n=faster+slower,x=speed,vibration=vibr,nsubj=9,subject=soggetto)


modello1 <- jags.model("modello_pse_dati1.txt",data=input,n.chains=3)

update(modello1, 500000)


# estimating pse and slope for each participant --------------------------------

parameters=c("alpha","beta")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

# samples of the posterior distribution of pse (alpha) and slope (beta) for 
# participants 1:9 and  without masking (1) and with masking vibrations (2).
#  For example alpha[2,1] is the intercept in subject 2 without masking
y1=as.array(snew[[1]])

# mean of the posterior distribution by subject and masking
y2=apply(y1,2,mean)

# figures 3 and 4: density plot for all subjects--------------------------------

names_participants <- vector(mode = "character", length = 18)

#Set the following line to TRUE if you would like to save the figures
do_save <- FALSE

# names in alphabetic order
id_names <- vibro_exp3 %>%
  select(subject) %>%
  sort(x = .$subject, decreasing = FALSE) %>%
  unique()

# vector of names in form of id_masking
id_names_to <- list()
masking_condition <- c("_0", "_1")

for(i in 1:2){
  id_names_to[[i]] <- str_c(id_names, masking_condition[[i]])
}

id_names_vector <- c(id_names_to[[1]], id_names_to[[2]])


vibr_conditional <- list(pse = as_tibble(y1[,1:18]), slope = as_tibble(y1[,19:36]))

save_plot <- list()

for(parameter in c("pse", "slope")){
  
  #This is for the individual PSE renames as subjects and masking 0,1
  names(vibr_conditional[[parameter]]) <- id_names_vector
  
  # Now do pivot longer to move all the values into one column 
  vibr_conditional[[parameter]]  <-  vibr_conditional[[parameter]]%>%
    pivot_longer(cols = everything(), 
                 names_to = c("subject", "masking"), names_sep = "_", values_to = "estimate") %>%
    mutate(
      masking = ifelse(masking == 0, "no masking", "masking")
    )
  
  # density plot
  save_plot[[parameter]] <- 
    ggplot(data = vibr_conditional[[parameter]], 
           mapping = aes(x = estimate, group = subject, color = masking)) +
    geom_density()+
    facet_grid( ~ masking) +
    theme(text = element_text(size = 12), legend.position = "none")+
    # scale_x_continuous(breaks = ) +
    labs(x = "parameter")
  
  if(do_save == TRUE){
    filename <- str_c("vibration_conditional_", parameter, ".pdf")
    ggsave(filename, save_plot[[parameter]], width = 85, height = 70, units = "mm")
  }

  
}

# figures 5 and 6: psychometric functions across individuals--------------------

y2_tibble <- tibble(
  estimate = y2,
  parameter = c(rep("pse", 18), rep("slope", 18)),
  subject_masking = rep(id_names_vector, 2)
  ) %>%
  pivot_wider(
    names_from = parameter,
    values_from = estimate
  )%>%
  separate(subject_masking, c("subject", "masking")) %>%
  expand_grid(speed.cms = seq(from = 0, to = 16, by = .1)) %>%
  mutate(
    masking = ifelse(masking == 0, "no masking", "masking"),
    intercept = - (pse * slope),
    predictions_bayesian = pnorm(q = speed.cms, mean = pse, sd = 1/slope)
  )

y2_tibble[["predictions_glmm"]] <- predict(glmm.vibro, newdata = y2_tibble, type = "response")

scatterplot_list <- list()

for(condition in  c("masking", "no masking")){

  # do the plot
  scatterplot_list[[condition]] <- 
    ggplot(filter(vibro_exp3, masking == condition), mapping = aes(x = speed.cms, y= proportion)) +
    geom_point() +
    facet_wrap(~ subject, ncol = 3) +
    geom_line(filter(y2_tibble, masking == condition), mapping = aes(x = speed.cms, y = predictions_bayesian), col = "red") +
    geom_line(filter(y2_tibble, masking == condition), mapping = aes(x = speed.cms, y = predictions_glmm), col = "blue") +
    ylab("Proportion of Faster Speed") +
    xlab("Speed [cm/s]") +
    facet_wrap(~ subject, ncol = 3) +
    theme(legend.position= "none") +
    scale_x_continuous(breaks = c(1, 8.5, 16)) +
    scale_y_continuous(breaks = c(0.0, 0.5, 1.0)) 
  
  if(do_save == TRUE){
    if(condition == "masking"){
      ggsave("vibration_masking_scatterplot.pdf", width = 3.69, height = 4.5)
    }else{
      ggsave("vibration_no_masking_scatterplot.pdf", width = 3.69, height = 4.5)
    }
  }
  
}

# estimate across all participants ---------------------------------------------


parameters=c("aa","bb")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

# figures 1 and 2: density plots for experiment 1: pse and slope----------------


vibr_marginal <- list(pse = as_tibble(y1[,1:2]), slope = as_tibble(y1[,3:4]))


for(parameter in c("pse", "slope")){
  
  #This is for the individual PSE renames as subjects and masking 0,1
  names(vibr_marginal[[parameter]]) <- c("no masking", "masking")
  
  # Now do pivot longer to move all the values into one column 
  vibr_marginal[[parameter]]  <-  vibr_marginal[[parameter]]%>%
    pivot_longer(cols = everything(), 
                 names_to = c("masking"), values_to = "estimate")
  
  # density plot
  save_plot <- 
    ggplot(data = vibr_marginal[[parameter]], 
           mapping = aes(x = estimate, color = masking)) +
    geom_density()+
    theme(text = element_text(size = 12), legend.position = "none")+
    # scale_x_continuous(breaks = ) +
    labs(x = "parameter")
  
  filename <- str_c("vibration_marginal_", parameter, ".pdf")
  
  ggsave(filename, save_plot, height = 70, width = 85, units = "mm")
  
}


# run jags probit model on the data--------------------------------------------------


modello1 <- jags.model("modello_probit_dati1.txt",data=input,n.chains=3)
update(modello1, 500000)

parameters=c("alpha","beta")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2
write.csv(y1,"y1probit_par_individuale2807.csv")

parameters=c("aa","bb")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

# This line also not running: modify or delete?
errore=erroremodello(input,y2)

l1=quantile(y1[,1],c(.025 ,.975))[2]-quantile(y1[,1],c(.025 ,.975))[1]
l2=quantile(y1[,2],c(.025 ,.975))[2]-quantile(y1[,2],c(.025 ,.975))[1]

write.csv(y1,"y1probit_par_overall_2807.csv")
