setJitterFrac<-function(dir,frac=0.1){
  res=scan(file.path(dir,"/starter.ss"),
           what=as.character(),sep="\n")
  res=paste(str_trim(res)," ")
  res=res[!substr(res,1,1)=="#"]
  
  res2=maply(res[-c(1:2,length(res))],function(x) substr(x,1,gregexpr("\\s",x)[[1]][1]-1))
  res2[13]=frac
  
  cat(c(res[1:2],res2,"999"),sep="\n",file=file.path(dir,"starter.ss"))}

jitterSS<-function(dir,frac=0.1){
  
  dirNow=getwd()
  
  #create temp dir
  dirTmp=mkTmp()
  
  #copy from target to temp
  system(paste("cp",file.path(dir,"*.*"),dirTmp))
  
  setwd(dirTmp)
  
  #set jitter
  setJitterFrac(dirTmp,frac=frac)
  
  #run
  system2("./ss3_3.24z",stdout=NULL)
  
  #get results
  res=try(SS_output(dirTmp,verbose=FALSE,printstats=FALSE))
  
  #clean up
  setwd(dirNow)
  
  if (is(res)=="try-error")
    NULL 
  else{
    rmTmp(dirTmp)
    res}
}
