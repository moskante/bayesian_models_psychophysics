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
library(beepr)

tidy_all <- TRUE

if(tidy_all){
  library(tidyverse)
}else{
  library(ggplot2)
  library(tidyr)
  library(magrittr)
  library(dbplyr)
  library(stringr)
  library(dplyr) 
}



# load data --------------------------------------------------------------------

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

usoggetto=unique(soggetto[cluster==2])
nsubj2=length(usoggetto)
soggetto2cluster=soggetto[cluster==2]
newsoggetto=0*soggetto2cluster
for (i in 1:nsubj2)
{newsoggetto[soggetto2cluster==usoggetto[i]]=i
}

input=list(noss=200,y=faster[cluster==2],n=faster[cluster==2]+slower[cluster==2],x=speed.cms[cluster==2],nsubj=20,subject=newsoggetto,mask=maskingnew[cluster==2]+1)



modello1 <- jags.model("modello_pse_uninformative.txt",data=input,n.chains=3)

update(modello1, 50000)
update(modello1, 100000)
parameters=c("aa","bb")


snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 10000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"y1overall_1111.cvs")


# ------------------------------------------------------------------------------
# prior elicitation

load("esperimento2.RData")
load("esperimento1.RData")



input1=input
input2=list(noss2=200,y2=input1$y,n2=input1$n,x2=input1$x,nsubj2=20,subject2=input1$subject,mask2=input1$mask,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto)

modello1 <- jags.model("modello_joint_a.txt",data=input2,n.chains=3)

update(modello1, 100000)
#update(modello1, 50000)

parameters=c("aa2","bb2")


snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 10000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2


errorio2=erroremodello2(input,y2,1,1)

write.csv(y1,"y1joint_1111.cvs")


zeros1=rep(0,126)
zeros2=rep(0,200)

a0 = c(0, 0.7, 1)
y1 <- pse_marginal <- slope_marginal <- list(non_informative = NA, power_prior = NA, informative = NA)
y2 <- list(non_informative = NA, power_prior = NA, informative = NA)

for(w in 1:3){
  
  input = list(noss2=200,y2=input1$y,n2=input1$n,x2=input1$x,nsubj2=20,subject2=input1$subject, # Maura è corretto x2=input$x oppure è x2=input1$x ?
             mask2=input1$mask,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,
             x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto,zeros1=zeros1,zeros2=zeros2,
             a0=a0[[w]] )
  
  
  modello1 <- jags.model("modelloa0.txt",data=input,n.chains=3)
  
  #update(modello1, 100000)
  update(modello1, 50000)
  
  parameters=c("aa2","bb2")
  
  
  snew = coda.samples(
    model =modello1,
    variable.names = parameters,
    thin = 1,
    n.iter = 10000 )
  
  y1[[w]] = as.array(snew[[1]])
  y2[[w]] = apply(y1[[w]],2,mean)
  
  # figure marginal --------------------------------------------------------------
  
  pse_marginal[[w]] <- as_tibble(y1[[w]][,1:2])
  slope_marginal[[w]] <- as_tibble(y1[[w]][,3:4])
  
  names(pse_marginal[[w]]) <- names(slope_marginal[[w]]) <- c("no masking", "masking")
  
  # Now do pivot longer to move all the values into one column 
  pse_marginal[[w]]  <-  pse_marginal[[w]]%>%
    pivot_longer(cols = everything(), 
                 names_to = c("masking"), values_to = "estimate")
  
  slope_marginal[[w]]  <-  slope_marginal[[w]]%>%
    pivot_longer(cols = everything(), 
                 names_to = c("masking"), values_to = "estimate")
  
}

#y2
#write.csv(y1,"y1a0_07.csv")
ctr_marginal <- list(
  pse = bind_rows(pse_marginal, .id = "prior"),
  slope = bind_rows(slope_marginal, .id = "prior")
)

for(parameter in c("pse", "slope")){
  
  #This is for the individual PSE renames as subjects and masking 0,1
  # density plot
  save_plot <- 
    ggplot(data = ctr_marginal[[parameter]], 
           mapping = aes(x = estimate, color = prior)) +
    geom_density()+
    facet_wrap(~ masking) +
    theme(legend.position = "none")
  
  filename <- str_c("prior_elicitation_", parameter, ".pdf")
  
  ggsave(filename, save_plot)
  
}

