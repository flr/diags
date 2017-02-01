utils::globalVariables(c("name","age"))
utils::globalVariables(c("read.csv"))
utils::globalVariables(c("ddply"))
utils::globalVariables(c("."))
utils::globalVariables(c("diagsFn"))

.diagUbsp=function(x){

  res       =read.csv(x)
  
  names(res)=c("name","year","obs","hat","residual")
  
  res=ddply(res,.(name),  diagsFn)
  
  #names(res)[seq(length(names(res)))[names(res)=="obs"]]="index"
  
  res}

  

