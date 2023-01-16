
erroremodello<-function(input,y2,overall,pse)
{  
  successo=input$y/input$n
  
  if (overall==1)
  {pse1=y2[1:2]
  
  bb1=y2[3:4]
  ppse=matrix(t(replicate(input$nsubj,pse1)),nrow=input$nsubj)
  bb=matrix(t(replicate(input$nsubj,bb1)),nrow=input$nsubj)
  ppse=t(ppse)
  bb=t(bb)
  if (pse==1){
    aa=-ppse*bb}
  if (pse==0){
    aa1=y2[1:2]
    aa=matrix(t(replicate(input$nsubj,aa1)),nrow=input$nsubj)
    aa=t(aa)
  }
  
  }
  
  
  
  if (overall==0)
  {
    n1=2*input$nsubj
    n2=n1+1
    n3=length(y2)
    pse1=y2[c(1:n1)]
  
  bb1=y2[c(n2:n3)]
  ppse=matrix(pse1,nrow=input$nsubj)
  bb=matrix(bb1,nrow=input$nsubj)
  ppse=t(ppse)
  bb=t(bb)
  if (pse==1){
    aa=-ppse*bb}
  if (pse==0){
    aa1=y2[c(1:n1)]
    aa=matrix(aa1,nrow=input$nsubj)
    aa=t(aa)
  }
  
  }
  
  
  
  
  predetti=rep(0,input$noss)

    for (i in 1:input$noss)
    {
    linear=aa[input$vibration[i]+1,input$subject[i]]+input$x[i]*bb[input$vibration[i]+1,input$subject[i]]
    predetti[i]=pnorm(linear)
    }
  errore=sum((successo-predetti)^2)
  errore
}
