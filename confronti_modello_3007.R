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

load("data.raw.RData")

attach(data.raw)
dati=data.raw[data.raw$cluster_foot=="ctr",]



library(fastDummies)
library(rjags)
maskingnew=(dati$masking=="1")
soggetto=rep(0,200)
masks=rep(0,200)
dummyid=dummy_cols(dati$id)

for (i in 1:20)
{soggetto=soggetto+i*dummyid[,i+1]}



dummycluster=dummy_cols(cluster_foot)

speedmasking=speed.cms
speedmasking[masking==0]=0



input=list(noss=200,y=dati$faster,n=dati$faster+dati$slower,x=dati$speed.cms,nsubj=20,subject=soggetto,mask=maskingnew+1)



modello1 <- jags.model("modello_probit_pse_3107_ctr.txt",data=input,n.chains=3)

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
y2o=y2


e1o=erroremodello(input,y2,1,1)

l1o=matrix(0,2,1)
for (i in 1:2)
{
  l1o[i,1]=quantile(y1[,i],c(.025 ,.975))[2]-quantile(y1[,i],c(.025 ,.975))[1]
  }

parameters=c("alpha","beta")


snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2


e1i=erroremodello(input,y2,0,1)

l1i=matrix(0,40,1)
for (i in 1:40)
{
  l1i[i,1]=quantile(y1[,i],c(.025 ,.975))[2]-quantile(y1[,i],c(.025 ,.975))[1]
}


input1=input
load("C:/Users/maura/Dropbox/Moscatelli/Programmi R/GLMM_diabetes/Bayesian_project/esperimento1.RData")

input=list(noss2=200,y2=input1$y,n2=input1$n,x2=input1$x,nsubj2=20,subject2=input1$subject,mask2=input1$mask,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto)




modello1 <- jags.model("Two_experiments_ctr_pse_2907.txt",data=input,n.chains=3)


update(modello1, 100000)


parameters=c("aa2","bb2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

e1o=erroremodello(input1,y2,1,1)

l1o=matrix(0,6,1)
for (i in 1:6)
{
  l1o[i,1]=quantile(y1[,i],c(.025 ,.975))[2]-quantile(y1[,i],c(.025 ,.975))[1]
}



parameters=c("alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2



e1i=erroremodello(input1,y2,0,1)

l1i=matrix(0,40,1)
for (i in 1:40)
{
  l1i[i,1]=quantile(y1[,i],c(.025 ,.975))[2]-quantile(y1[,i],c(.025 ,.975))[1]
}


