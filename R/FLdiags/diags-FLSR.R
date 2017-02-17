 utils::globalVariables(c("value"))
 utils::globalVariables(c("index"))
 utils::globalVariables(c("residual"))

setMethod('diags',  signature(object='FLSR',method="missing"), 
          function(object){
  
  res=model.frame(FLQuants(object,"ssb"=ssb,
                           "rec"=rec,
                           "predict"=predict,
                           "residual"=residuals))
  res=diagsFn(res)
  
  return(res)})
   
 
 setMethod('diags',  signature(object='FLSRs',method="missing"), 
           function(object){
             
             ldply(object,diags)})
 