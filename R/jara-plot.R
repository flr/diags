jaraFits<-function(object){
  trj=merge(object$trj,object$fits[,c("assessment","scenario","name","hat")])
  trj[,7:11]=trj[,7:11]/trj[,12]
  names(trj)[4]="year"
 
  u   =transform(object$fits,obs.uci=exp(log(obs)+1.96*obs.err)/hat,obs.lci=exp(log(obs)-1.96*obs.err)/hat)
  fits=merge(u[,  c("name","year","obs","hat","obs.lci","obs.uci")],
             trj[,c("name","year","lci","uci","lpp","upp","mu")],all.y=T)
  fits}

if (FALSE){
  ggplot(jaraFits(jas[[1]]))+
    geom_ribbon(aes(year,ymin=lpp,ymax=upp),fill="grey75")+
    geom_ribbon(aes(year,ymin=lci,ymax=uci),fill="grey25")+
    geom_errorbar(aes(year,ymin=obs.lci,ymax=obs.uci))+
    geom_line(aes(year,mu))+
    geom_point( aes(year,obs/hat),col="white")+
    facet_wrap(~name,scale="free_y",ncol=2)+
    xlab("Year")+ylab("Index")
}  