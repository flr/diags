utils::globalVariables(c("av"))

setGeneric('aav', function(object, ...)
  standardGeneric('aav'))

setGeneric('antiCurve', function(object, ...)
  standardGeneric('antiCurve'))

avFn<-function(object){
  
  o1 =object[-1]
  o2 =object[-length(object)]
  
  return(abs(o1-o2)/o1)}

setMethod('aav', signature(object='data.frame'),
 function(object){
          
  yrs   =object$year
  object=object[order(yrs),]
  
  res  =apply(object, 2, avFn)
  res  =res[!(names(res) %in% "year")]

  return(res)})
  
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

