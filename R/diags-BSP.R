utils::globalVariables(c("name","age"))

.diagUbsp=function(x){

  res       =read.csv(x)
  
  names(res)=c("name","year","obs","hat","residual")
  
  res=ddply(res,.(name),  diagsFn)
  
  names(res)[seq(length(names(res)))[names(res)=="obs"]]="index"
  
  res}

  

