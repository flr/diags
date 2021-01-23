maseSS<-function(obs,hat,step=1,na.rm=TRUE){
  t=length(hat)
  
  sum(abs(obs[-seq(step)]-hat[-seq(step)]),na.rm=na.rm)/
    sum(abs(obs[-seq(step)]-obs[-(length(obs)-seq(step)+1)]),na.rm=na.rm)}
