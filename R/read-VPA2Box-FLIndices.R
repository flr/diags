# library(FLCore)
# file="/home/laurie/Desktop/kobe/inputs/bfte/2014/vpa/reported/med/bfte2014.d1"

skip.hash<-function(i,x) {
  i<-i+1
  while (substr(scan(x,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
    i<-i+1
  return(i)}

skip.until.minus.1<-function(i,x) {
  i<-i+1
  while (scan(x,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
    i<-i+1
  return(i)}

readVP2BoxIndices<-function(x,na.strings="NA") {

  #range for stock
  rngStk=c("min"=NA,"max"=NA,"plusgroup"=NA,"minyear"=NA,"maxyear"=NA)
  
  i =skip.hash(0,x)
  rngStk[c("minyear","maxyear")]=scan(x,skip=i, nlines=1, nmax=2, quiet=TRUE, na.strings=na.strings)
  
  i =skip.hash(i,x)
  rngStk[c("min", "max","plusgroup")]=scan(x,skip=i, nlines=1, nmax=3, quiet=TRUE,na.strings=na.strings)
  
  i =skip.hash(i+1,x)
  nIdx =scan(x,skip=i,nlines=1,nmax=1,quiet=TRUE)
  i =skip.until.minus.1(i,x)
  i =skip.hash(i,x)
  
  #range by index
  rng =data.frame(array(0,dim=c(nIdx,12)))
  names(rng)=c("i","pdf","units","type","timing","min","max","plusgroup","minyear","maxyear","startf","endf")

  j <- numeric(1)
  for (i in i:(i+nIdx-1)) {
    j<-j+1
    rng[j,1:7]<-scan(x,skip=i,nlines=1,nmax=7,quiet=TRUE,na.strings=na.strings)}
  rng[,"plusgroup"]=NA
  rng[rng[,"max"]==max(rng[,7]),"plusgroup"]=max(rng[,7])
  rng[,"startf"]=(rng[,"timing"]-1)/12
  rng[,"endf"]  =(rng[,"timing"])/12
  rng[rng[,"timing"]<0,c("startf","endf")]=-1
  
  #Index
  i<-skip.hash(i,x)
  i<-skip.hash(i,x)
  idx<-array(0,dim=c(skip.until.minus.1(i,x)-i, 4))
  for (j in i:(skip.until.minus.1(i,x)-1))
    idx[j-i+1,]<-scan(x,skip=j,nlines=1,nmax=4,quiet=T,na.strings=na.strings)
  idx=data.frame(idx)
  names(idx)=c("index","year","data","se")
  rng[,"minyear"]=tapply(idx[,2],idx[,1],min)
  rng[,"maxyear"]=tapply(idx[,2],idx[,1],max)
  
  i<-skip.until.minus.1(i,x)+1	   	
  i<-skip.hash(i,x)
  pcaa=read.table(x,skip=i,fill=T,nrows=(skip.until.minus.1(i,x)-i),colClasses="numeric",na.strings=na.strings)
  pca =data.frame(index=rep(pcaa[,1],(rngStk[2]-rngStk[1]+1)),
                  year =rep(pcaa[,2],(rngStk[2]-rngStk[1]+1)),
                  age  =rep(rngStk["min"]:rngStk["max"],each=dim(pcaa)[1]),
                  data =unlist(c(pcaa[,-(1:2)])))

  i<-skip.until.minus.1(i,x)+1	   	
  i<-skip.hash(i,x)
  pwaa=read.table(x,skip=i,fill=T,nrows=(skip.until.minus.1(i,x)-i),colClasses="numeric",na.strings=na.strings)
  pwa =data.frame(index=rep(pwaa[,1],(rngStk[2]-rngStk[1]+1)),
                  year =rep(pwaa[,2],(rngStk[2]-rngStk[1]+1)),
                  age  =rep(rngStk["min"]:rngStk["max"],each=dim(pwaa)[1]),
                  data =unlist(c(pwaa[,-(1:2)])))
  
  rtn=FLIndices()   
  
  c("fixed","frac","part","butt")

  for (i in seq(dim(rng)[1])){
    range       =unlist(rng[i,6:12])
    names(range)[4:5]=c("minyear","maxyear")
    dmns         =list(age =as.character(range["min"]:range["max"]),
                       year=as.character(range["minyear"]:range["maxyear"]))  
    name         =""
    desc        =paste("Read in from",x)
    type        =c("fixed","frac","part","butt")[rng[i,"type"]]
    distribution=c("lognormal","normal")[rng[i,"pdf"]]
    index       =as.FLQuant(subset(idx,index==i)[,c("year","data")],units=c("number","biomass")[rng[i,"units"]])
    index.var   =as.FLQuant(transform(subset(idx,index==i),data=se)[,c("year","data")])
    if (i%in%pca[,"index"]){
      catch.n     =as.FLQuant(subset(pca,index==i)[,c("age","year","data")])
      catch.n     =catch.n[as.character(rng[i,"min"]:rng[i,"max"]),as.character(rng[i,"minyear"]:rng[i,"maxyear"])]
    }else{
      catch.n     =FLQuant(NA,dimnames=dmns)}
    if (i%in%pwa[,"index"]){
      catch.wt    =as.FLQuant(subset(pwa,index==i)[,c("age","year","data")])
      catch.wt    =catch.wt[as.character(rng[i,"min"]:rng[i,"max"]),as.character(rng[i,"minyear"]:rng[i,"maxyear"])]
    }else{
      catch.wt    =FLQuant(NA,dimnames=dmns)}
    effort      =FLQuant(NA,dimnames=dimnames(index))     
    if (i%in%pca[,"index"]){
      sel.pattern =if(rng[i,"type"]!=1) FLQuant(NA,dimnames=dimnames(catch.n)) else catch.n
      catch.n     =if(rng[i,"type"]==1) FLQuant(NA,dimnames=dimnames(catch.n)) else catch.n
    }else{
      sel.pattern =FLQuant(NA,dimnames=dmns)}
    index.q     =effort

    rtn[[i]]=FLIndex(index=index,range=range,type=type,
                     name=name,desc=desc,distribution=distribution,
                     index=index,index.var=index.var,
                     catch.n=catch.n,catch.wt=catch.wt,
                     effort=effort,index.q=index.q,
                     sel.pattern=sel.pattern)
    }
  
  rtn}