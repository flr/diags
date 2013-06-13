read.cas=function(x,nrows=NULL){
  require(LaF)
  require(reshape)
  
  nms=names(read.csv(x,nrows=1))
  fld=nms[1:18]
  ln =nms[-(1:18)]

  types=c(rep("factor",2),"integer",rep("factor",5),"integer",rep("factor",2),rep("integer",3),rep("factor",2),rep("integer",2),rep("numeric",length(nms)-18))
  dat  =laf_open_csv(filename=x,column_types=types,column_names=nms,skip=1)
  if (is.null(nrows))  res=subset(melt(dat[,],id.vars=fld),value>0)
  else                 res=subset(melt(dat[1:nrows,],id.vars=fld),value>0)   
  
  res  =transform(res, n  =value, 
                       len=as.numeric(substr(as.character(variable),2,nchar(as.character(variable)))),
                       yr =as.numeric(as.character(YearC))-(as.numeric(ac(YearC)) %/% 10)*10,
                       dec=(as.numeric(as.character(YearC)) %/% 10)*10)[,-(19:20)]
  
  names(res)[seq(length(names(res)))[names(res)=="YearC"]]="year"
  
  res}

read.caa=function(x,nrows=NULL){
  require(LaF)
  require(reshape)
  
  nms=names(read.csv(x,nrows=1))
  fld=nms[1:22]
  ag =nms[-(1:22)]
  
  types=c(rep("factor",2),"integer",rep("factor",5),"numeric",rep("factor",2),rep("integer",3),rep("factor",2),rep("integer",2),rep("factor",4),rep("numeric",length(nms)-22))
  dat  =laf_open_csv(filename=x,column_types=types,column_names=nms,skip=1)
  if (is.null(nrows))  res=subset(melt(dat[,],id.vars=fld),value>0)
  else                 res=subset(melt(dat[1:nrows,],id.vars=fld),value>0) 
  
  res  =transform(res, n  =value, 
                       age=as.numeric(substr(as.character(variable),4,nchar(as.character(variable)))),
                       yr =as.numeric(as.character(YearC))-(as.numeric(as.character(YearC)) %/% 10)*10,
                       dec=(as.numeric(as.character(YearC)) %/% 10)*10)  
  
  res}


month<-function(x){
  x <-pmax(0,x)
  
  fn<-function(x) switch(ac(x),
                         "13"= 2,
                         "14"= 5, 
                         "15"= 8, 
                         "16"=11,
                         "17"= 6, 
                         "18"= 3, 
                         "19"= 9,
                         x)
  
  sapply(x,fn)}