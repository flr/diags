utils::globalVariables(c("av"))

setGeneric('aav', function(object, ...)
  standardGeneric('aav'))

setGeneric('av', function(object, ...)
  standardGeneric('av'))

setGeneric('antiCurve', function(object, ...)
  standardGeneric('antiCurve'))

avFn<-function(object){
  o1=object[-length(object)]
  o2=object[-1]
  
  return(abs((o2-o1)/o1))}

setMethod('aav', signature(object='FLQuant'),
          function(object){
          
  dmns=dimnames(object)
  dmns[[2]]=dmns[[2]][-length(dmns[[2]])]
  dmns=dmns[c(TRUE,(laply(dmns[-1],length)>1))]
            
  res=aaply(object,c(1,3:6), avFn)
  res=FLQuant(c(res),dimnames=dmns)
          
  res=aaply(res,c(1,3:6), mean)
  res=FLQuant(c(res),dimnames=dmns[-2])
            
  return(res)})

setMethod('av', signature(object='FLQuant'),
          function(object){
            
  dmns=dimnames(object)
  dmns[[2]]=dmns[[2]][-length(dmns[[2]])]
  dmns=dmns[c(TRUE,(laply(dmns[-1],length)>1))]
            
  res=aaply(object,c(1,3:6), avFn)
  res=FLQuant(c(res),dimnames=dmns)
            
  return(res)})

setMethod('aav', signature(object='data.frame'),
 function(object){
          
  yrs   =object$year
  
  object=object[order(yrs),]

  res  =apply(object[,dimnames(object)[[2]]!="year"], 2, avFn)
  res  =apply(res, 2, mean)
  
  return(res)})

setMethod('aav', signature(object='numeric'),
          function(object){
            
            return(mean(avFn(object)))})

setMethod('antiCurve', signature(object='data.frame'),
 function(object,tol=0.01){
          
  yrs   =object$year
  object=object[order(yrs),]
  
  anCr=function(x) {
      n  =length(x)
      if (!is.numeric(x[-(1:2)]) || !is.numeric(x[-(n-0:1)])) return(NULL)

      M  =(x[-(1:2)]+x[-(n-0:1)])/2
      o1 =x[-c(1,n)]

      res=mean(abs((o1-M)/pmax(M,tol)), na.rm=TRUE)
 
      return(res)}
 
  res  =apply(object, 2, anCr)
  res  =res[!(names(res) %in% "year")]

  return(res)})

