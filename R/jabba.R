getProd<-function(x){
  
  object=SS_output(dirname(x), verbose=FALSE,printstats=FALSE,covar=FALSE,forecast=FALSE)
  
  ts=getTs(object)
  pf=getPellaT(object)
  u =object$cpue[,c("Fleet","Fleet_name","Yr","Obs","Exp","SE")]
  u =data.table::setnames(u,c("Fleet","Fleet_name","Yr","Obs","Exp","SE"),c("id","name","year","obs","hat","se"))
  
  list(timeseries=ts,prodfn=pf,indices=u)}

setBiodyn<-function(x){
  object=getProd(x)

  bd=biodyn(object=as.FLQuant(transmute(object[["timeseries"]],year=year,data=catch)),
          params=rbind(as(object[["prodfn"]],"FLPar")[-4],FLPar(b0=1)),
          indices=FLQuants(dlply(object[["indices"]], .(name), with, as.FLQuant(data.frame(year=year,data=obs)))))

  setControl(bd)=bd@indices
  
  bd}
