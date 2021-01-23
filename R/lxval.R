setGeneric('lxvl', function(object, tail, ...) standardGeneric('lxvl'))
setGeneric('lbi',  function(object, linf, ...) standardGeneric('lbi'))

setMethod('lxvl', signature(object='character','numeric'),
          function(object,tail,dir=mkTmp(),starter="starter.ss",forecast="forecast.ss",bin="ss3_3.24z",...){
            
  files=list(...)
  
  ## copy files
  tmp=scan(file.path(object,starter),sep="\n",what=as.character())
  tmp=tmp[substr(tmp,1,1)!="#"]
  tmp=tmp[nchar(tmp)!=0]
  
  dat=strsplit(tmp[1],"\\s+")[[1]][1]
  ctl=strsplit(tmp[2],"\\s+")[[1]][1]
  
  dirTmp=mkTmp()
  
  file.copy(file.path(object,c(starter,forecast,dat,ctl,bin,files)),dirTmp,recursive=TRUE)
  
  ## get data
  dfl=str_trim(scan(file.path(object,dat),sep="\n",what=as.character()))
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
  rLnc=(max(rDsc)+11):(length(dfl)-13)
  
  lnc=try(mdply(rLnc, function(i) as.numeric(strsplit(dfl[i],"\\s+")[[1]]))[,-1])
  if (is(lnc)=="try-error") lnc=mdply(rLnc-1, function(i) as.numeric(strsplit(dfl[i],"\\s+")[[1]]))[,-1]
  
  ## "remove" length for year
  lnc[lnc[,1]>=tail,3]= (-lnc[lnc[,1]>=tail,3])
  lnc          =lnc[, !adply(lnc,2,function(x) all(is.na(x)))[,2]]
  dfl[rLnc]    =apply(lnc,1,function(x) paste(x,"\t",collapse=""))
  
  ## save & run
  cat(dfl, file=file.path(dirTmp,dat),sep="\n")
  
  where =getwd()
  setwd(dirTmp)
  system2("./ss3_3.24z",args="-nohess",stdout=NULL)
  
  lb=SS_output(dirTmp, forecast  =FALSE,
               covar     =FALSE,
               checkcor  =FALSE,
               verbose   =FALSE,
               printstats=FALSE,
               hidewarn  =TRUE,
               NoCompOK  =TRUE,
               ncols     =250)
  
  lb=rbind(lb$lendbase,lb$ghostlendbase)
  
  setwd(where)
  
  lb})

lbiFn<-function(x,linf,what="exp"){
  
  names(x)=tolower(names(x))
  lf       =x[,c("yr","fleet","sex","seas","bin",what)]
  names(lf)=c("year","name","sex","seas","len","n")
  lf=ddply(lf,.(name,year,sex,seas,len), with, data.frame(n=sum(n)))
  #lf=merge(lf,transmute(growth, sex=Gender,linf=Linf))
  lf$linf=linf
  
  ind=ddply(lf, .(name,sex,year,seas), with, lenInd(len,n,lopt=linf*2/3))
  
  return(ind)}

setMethod('lbi', signature(object='list','missing'),
      function(object,linf,...){

      if (!all(c("lendbase","Growth_Parameters"))%in%names(object)) stop("Is object an SS_output?")

      linf  =mean(ss[[1]]$Growth_Parameters[,"Linf"])
      object=object$lendbase
        
      lbi(object,linf)})



setMethod('lbi', signature(object='data.frame','numeric'),
          function(object,linf,...){
            
  hat=melt(lbiFn(object,linf),      id=c("name","sex","year","seas"))
  obs=melt(lbiFn(object,linf,"obs"),id=c("name","sex","year","seas"))
  rsd=cbind(obs,hat=hat[,"value"])
  names(rsd)[c(4:6)]=c("season","indicator","obs")
  transform(rsd,residual=log(obs/hat))})


