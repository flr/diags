.writeU2bsp=function(x,file,...){
   
  ## 2box
  if (all(c("name","year","index","cv") %in% names(x)))
    x=transform(x, name=as.integer(name))[,c("name","year","index","cv")]
  
  
  write.table(x,file=file)
  
  return(TRUE)}
