
# readCpue2box utils::globalVariables(c("variable"))
# readCpueAspic utils::globalVariables(c("id."))
# readCpueSS utils::globalVariables(c("SS_output"))
# writeCpueVPASuite utils::globalVariables(c("name"))

 utils::globalVariables(c("value"))
 utils::globalVariables(c("index"))
 utils::globalVariables(c("residual"))
 utils::globalVariables(c("diagsFn"))


.diagUaspic=function(object){
  #object="http://gbyp-sam.googlecode.com/svn/trunk/data/ASPIC/albs/2011/run2/aspic.prn"
  
  res=read.table(object,header=TRUE)
  res=res[,seq(dim(res)[2]-2)]
  
  obs=melt(res[,seq((dim(res)[2]-1)/2+1)],id.var="year")
  est=melt(res[,c(1,((dim(res)[2]-1)/2+2):dim(res)[2])],id.var="year")
  
  res=data.frame(transform(obs,obs  =value,
                               index=gsub(".obs","",obs$variable))[,c("year","index","obs")],
                               hat  =est$value)
  
  res$residual=log(res$obs/res$hat)
  
  res=subset(ddply(res,.(index),diagsFn),!is.na(residual))
  
  names(res)[2:3]=c("name","obs")
  
  return(res)}
