rval<-function(x,tail,dir=mkTmp(),starter="starter.ss",forecast="forecast.ss",bin="ss3_3.24z",...){
  files=list(...)
  
  ## copy files
  tmp=scan(file.path(x,starter),sep="\n",what=as.character())
  tmp=tmp[substr(tmp,1,1)!="#"]
  tmp=tmp[nchar(tmp)!=0]
  
  #dat=paste(strsplit(tmp[1],".dat")[[1]][1],"dat",sep=".")
  #ctl=paste(strsplit(tmp[2],".ctl")[[1]][1],"ctl",sep=".")
  dat=strsplit(tmp[1],".dat")[[1]][1]
  ctl=strsplit(tmp[2],".ctl")[[1]][1]
  
  dirTmp=mkTmp()
  
  file.copy(file.path(x,c(starter,forecast,dat,ctl,bin,files)),dirTmp,recursive=TRUE)
  
  ## get data
  dfl=str_trim(scan(file.path(x,dat),sep="\n",what=as.character()))
  dfl=dfl[substr(dfl,1,1)!="#"]
  dfl=dfl[nchar(dfl)!=0]
  
  # function to get count number of rows
  rw<-function(x) as.numeric(strsplit(x,"\\s+")[[1]][1])
  
  # number of fleets and surveys
  nFlt=rw(dfl[6])
  nSry=rw(dfl[7])
  
  # rows with data
  rCtc=seq(rw(dfl[17]))+17
  rU  =max(rCtc)+1+nFlt+nSry+seq(rw(dfl[max(rCtc)+1]))
  nDsc=rw(dfl[max(rU)+1])
  rDsc=max(rU)+nDsc+2+seq(rw(dfl[max(rU)+nDsc+2]))
  nLnc=rw(dfl[max(rDsc)+9])
  #rLnc=(max(rDsc)+12):(length(dfl)-12)
  rLnc=(max(rDsc)+11):(length(dfl)-13)
  
  u  =mdply(rU,   function(i) as.numeric(strsplit(dfl[i],"\\s+")[[1]])[1:5])[,-1]
  lnc=try(mdply(rLnc, function(i) as.numeric(strsplit(dfl[i],"\\s+")[[1]]))[,-1])
  if (is(lnc)=="try-error") lnc=mdply(rLnc-1, function(i) as.numeric(strsplit(dfl[i],"\\s+")[[1]]))[,-1]
  
  ## "remove" index for year
  u[u[,1]>=tail,3]= (-u[u[,1]>=tail,3])
  u       =u[, !adply(u,2,function(x) all(is.na(x)))[,2]]
  dfl[rU] =apply(u,1,function(x) paste(x,"\t",collapse=""))
  
  ## "remove" length for year
  lnc[lnc[,1]>=tail,3]= (-lnc[lnc[,1]>=tail,3])
  lnc          =lnc[, !adply(lnc,2,function(x) all(is.na(x)))[,2]]
  dfl[rLnc]    =apply(lnc,1,function(x) paste(x,"\t",collapse=""))
  
  ## save & run
  cat(dfl, file=file.path(dirTmp,dat),sep="\n")
  
  where =getwd()
  setwd(dirTmp)
  #system2(paste("./",bin,sep=""),args="-nohess",stdout=NULL)
  system2(paste(bin,sep=""),args="-nohess",stdout=NULL)
  
  ss=SS_output(dirTmp, forecast  =FALSE,
               covar     =FALSE,
               checkcor  =FALSE,
               verbose   =FALSE,
               printstats=FALSE,
               hidewarn  =TRUE,
               NoCompOK  =TRUE,
               ncols     =250)
  
  rtn=cbind(tail=tail,cbind(getTs(ss),getRf(ss)[1,]))

  setwd(where)
  
  rtn}

if (FALSE){
  library(doParallel)
  library(foreach)
  
  cl <-makeCluster(6)
  registerDoParallel(cl)
  
  rtr=foreach(tail=2009:2014,
          .combine=rbind,
          .export      =ls(globalenv()),
          .packages="r4ss") %dopar%{
          source('~/Desktop/flr/diags/R/ss-prd.R')
          source('~/Desktop/flr/xvl/R/xval.R')
          x="/home/laurence-kell/Desktop/papers/albio/inputs/ss/1-M0202_sigmaR0.4_steepness0.7_cpuecv0.2_ess20_llq1.0000_llselLog_number1"
          rval(x,tail)}
          
  save(rtr,file="/home/laurence-kell/Desktop/papers/albio/paper1/results/rtr.RData")}