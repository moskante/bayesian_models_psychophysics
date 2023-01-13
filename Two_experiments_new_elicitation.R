

load("esperimento1.RData")
load("esperimento2.RData")

library(rjags)
library(tidyverse)

zeros1=rep(0,126)
zeros2=rep(0,600)


# elicitation for a0 = 0.75 ----------------------------------------------------

#mettere valore a0
a0=0.75
input=list(noss2=600,y2=data.raw$faster,n2=data.raw$faster+data.raw$slower,x2=data.raw$speed.cms,nsubj2=60,subject2=subject,cluster2=clusters,mask2=maskingnew+1,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto,a0=a0,zeros1=zeros1,zeros2=zeros2)



modello1 <- jags.model("Two_experiments_new_elicitation.txt",data=input,n.chains=3)

update(modello1, 50000)
# update(modello1, 50000)

parameters=c("aa2","bb2","aa1","bb1")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2


# figure marginal --------------------------------------------------------------

ctr_marginal <- list(pse = as_tibble(y1[,1:8]), slope = as_tibble(y1[,9:16]))


for(parameter in c("pse", "slope")){
  
  #This is for the individual PSE renames as subjects and masking 0,1
  names(ctr_marginal[[parameter]]) <- c("no masking", "masking")
  
  # Now do pivot longer to move all the values into one column 
  ctr_marginal[[parameter]]  <-  ctr_marginal[[parameter]]%>%
    pivot_longer(cols = everything(), 
                 names_to = c("masking"), values_to = "estimate")
  
  # density plot
  save_plot <- 
    ggplot(data = ctr_marginal[[parameter]], 
           mapping = aes(x = estimate, color = masking)) +
    geom_density()+
    theme(legend.position = "none")
  
  filename <- str_c("vibration_marginal_", parameter, ".pdf")
  
  ggsave(filename, save_plot)
  
}

write.csv(y1,"two_experiments_elicitation_75_1607.csv")


parameters=c("alpha1","beta1","alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_individual_0.csv")



parameters=c("taua1","taub1","taua2","taub2","s1","s2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_precision_0.csv")




# elicitation for other values of a0  -------------------------------------------


a0=0.2
input=list(noss2=600,y2=data.raw$faster,n2=data.raw$faster+data.raw$slower,x2=data.raw$speed.cms,
           nsubj2=60,subject2=subject,cluster2=clusters,mask2=maskingnew+1,noss1=126,
           y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,
           nsubj1=9,subject1=soggetto,a0=a0,zeros1=zeros1,zeros2=zeros2)



modello1 <- jags.model("Two_experiments_elicitation_1.txt",data=input,n.chains=3)

update(modello1, 50000)
update(modello1, 50000)

parameters=c("aa2","bb2","aa1","bb1")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_2.csv")


parameters=c("alpha1","beta1","alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_individual_2.csv")



parameters=c("taua1","taub1","taua2","taub2","s1","s2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_precision_2.csv")




a0=0.4
input=list(noss2=600,y2=data.raw$faster,n2=data.raw$faster+data.raw$slower,x2=data.raw$speed.cms,nsubj2=60,subject2=subject,cluster2=clusters,mask2=maskingnew+1,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto,a0=a0,zeros1=zeros1,zeros2=zeros2)



modello1 <- jags.model("Two_experiments_elicitation_1.txt",data=input,n.chains=3)

update(modello1, 50000)
update(modello1, 50000)

parameters=c("aa2","bb2","aa1","bb1")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_4.csv")


parameters=c("alpha1","beta1","alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_individual_4.csv")



parameters=c("taua1","taub1","taua2","taub2","s1","s2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_precision_4.csv")





a0=0.6
input=list(noss2=600,y2=data.raw$faster,n2=data.raw$faster+data.raw$slower,
           x2=data.raw$speed.cms,nsubj2=60,subject2=subject,cluster2=clusters,mask2=maskingnew+1,
           noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,
           x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto,a0=a0,zeros1=zeros1,zeros2=zeros2)



modello1 <- jags.model("Two_experiments_elicitation_1.txt",data=input,n.chains=3)

update(modello1, 50000)
update(modello1, 50000)

parameters=c("aa2","bb2","aa1","bb1")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_6.csv")


parameters=c("alpha1","beta1","alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_individual_6.csv")



parameters=c("taua1","taub1","taua2","taub2","s1","s2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_precision_6.csv")






a0=0.8
input=list(noss2=600,y2=data.raw$faster,n2=data.raw$faster+data.raw$slower,x2=data.raw$speed.cms,nsubj2=60,subject2=subject,cluster2=clusters,mask2=maskingnew+1,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto,a0=a0,zeros1=zeros1,zeros2=zeros2)



modello1 <- jags.model("Two_experiments_elicitation_1.txt",data=input,n.chains=3)

update(modello1, 50000)
update(modello1, 50000)

parameters=c("aa2","bb2","aa1","bb1")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_8.csv")


parameters=c("alpha1","beta1","alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_individual_8.csv")



parameters=c("taua1","taub1","taua2","taub2","s1","s2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_precision_8.csv")





a0=1
input=list(noss2=600,y2=data.raw$faster,n2=data.raw$faster+data.raw$slower,x2=data.raw$speed.cms,nsubj2=60,subject2=subject,cluster2=clusters,mask2=maskingnew+1,noss1=126,y1=vibro_exp3$faster,n1=vibro_exp3$faster+vibro_exp3$slower,x1=vibro_exp3$speed,vibration1=vibr,nsubj1=9,subject1=soggetto,a0=a0,zeros1=zeros1,zeros2=zeros2)



modello1 <- jags.model("Two_experiments_elicitation_1.txt",data=input,n.chains=3)

update(modello1, 50000)
update(modello1, 50000)

parameters=c("aa2","bb2","aa1","bb1")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_1.csv")


parameters=c("alpha1","beta1","alpha2","beta2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_individual_1.csv")



parameters=c("taua1","taub1","taua2","taub2","s1","s2")

snew = coda.samples(
  model =modello1,
  variable.names = parameters,
  thin = 1,
  n.iter = 5000 )

y1=as.array(snew[[1]])
y2=apply(y1,2,mean)
y2

write.csv(y1,"two_experiments_elicitation_precision_1.csv")
