utils::globalVariables(c("stock.wt"))
utils::globalVariables(c("stock.n"))
utils::globalVariables(c("stock"))
utils::globalVariables(c("ssb0.1"))
utils::globalVariables(c("harvest"))
utils::globalVariables(c("f0.1"))
utils::globalVariables(c("read.table"))
utils::globalVariables(c("melt"))
utils::globalVariables(c("ddply"))
utils::globalVariables(c("."))
utils::globalVariables(c("year"))
utils::globalVariables(c("quantile"))
utils::globalVariables(c("readVPA2Box"))
utils::globalVariables(c("m"))
utils::globalVariables(c("dims"))
utils::globalVariables(c("read.table"))
utils::globalVariables(c("stock<-"))
utils::globalVariables(c("computeStock"))
utils::globalVariables(c("propagate"))
utils::globalVariables(c("setPlusGroup"))
utils::globalVariables(c("window"))
utils::globalVariables(c("as.FLQuant"))
utils::globalVariables(c("harvest"))
utils::globalVariables(c("harvest<-"))
utils::globalVariables(c("V2"))
utils::globalVariables(c("melt"))
utils::globalVariables(c("stock.wt"))
utils::globalVariables(c("stock.wt<-"))
utils::globalVariables(c("discards.wt<-"))
utils::globalVariables(c("catch<-"))
utils::globalVariables(c("computeCatch"))
utils::globalVariables(c("catch.wt<-"))
utils::globalVariables(c("ssb"))
utils::globalVariables(c("stock.n"))
utils::globalVariables(c("stock.n<-"))
utils::globalVariables(c("m<-"))
utils::globalVariables(c("m.spwn"))
utils::globalVariables(c("m.spwn<-"))
utils::globalVariables(c("harvest.spwn"))
utils::globalVariables(c("harvest.spwn<-"))
utils::globalVariables(c("mat"))
utils::globalVariables(c("mat<-"))
utils::globalVariables(c("catch.wt"))
utils::globalVariables(c("discards.wt"))

utils::globalVariables(c("FLQuant"))
utils::globalVariables(c("FLQuants"))
utils::globalVariables(c("llply"))
utils::globalVariables(c("harvest"))

utils::globalVariables(c("harvest"))
utils::globalVariables(c("FLQuant"))
utils::globalVariables(c("dmns"))
utils::globalVariables(c("quantile"))

utils::globalVariables(c("dlply"))
utils::globalVariables(c("."))
utils::globalVariables(c("series"))
utils::globalVariables(c("as.FLQuant"))
utils::globalVariables(c("read.table"))
utils::globalVariables(c("melt"))
utils::globalVariables(c("FLIndices"))
utils::globalVariables(c("dims"))
utils::globalVariables(c("FLIndex"))
utils::globalVariables(c("trim"))
utils::globalVariables(c("FLQuant"))

utils::globalVariables(c("type"))
utils::globalVariables(c("stock.n"))
utils::globalVariables(c("stock.wt"))
utils::globalVariables(c("yrw"))
utils::globalVariables(c("kobe"))

getPro2TS<-function(dir,minyear){
  
  kb=kobe(dir,"2box")
  kb=transform(kb,SSB=stock*ssb0.1,Harvest=harvest*f0.1)[,c("year","iter","SSB","Harvest")]
  
  rec=read.table(paste(dir,"RECRT-1.OUT",sep="/"),header=F,skip=0)
  names(rec)=c("tac","iter",seq(dim(rec)[2]-2))
  rec=melt(rec,id=c("tac","iter"))[,-1]
  names(rec)[c(2,3)]=c("year","Rec")
  
  yld=read.table(paste(dir,"YIELD-1.OUT",sep="/"),header=F,skip=0)
  names(yld)=c("tac","iter",seq(dim(yld)[2]-2))
  yld=melt(yld,id=c("tac","iter"))[,-1]
  names(yld)[c(2,3)]=c("year","Catch")
  yld=cbind(yld,Rec=rec[,3])
  
  kb=merge(kb,yld)
  
  kb=ddply(melt(kb,id=c("iter","year")),.(year,variable), with, 
           quantile(value,c(0.1,0.25,0.5,0.75,0.9)))
  
  kb$year=as.numeric(ac(kb$year))+minyear-1
  names(kb)[2]="qname"
  
  kb}

readIndices2box<-function(file,na.strings="NA") {
  skip.hash<-function(i) {
    i=i+1
    while (substr(scan(file,skip=i,nlines=1,
                       what =("character"),
                       quiet=TRUE)[1],1,1)=="#")
      i=i+1
    return(i)}
  
  skip.until.minus.1<-function(i) {
    i=i+1
    while (scan(file,skip=i,nlines=1,
                what =("character"),
                quiet=TRUE)[1]!="-1")
      i=i+1
    return(i)}
  
  # Range
  range=c("min"=as.numeric(NA),"max"=as.numeric(NA),"plusgroup"=as.numeric(NA),
          "minyear"=as.numeric(NA),"maxyear"=as.numeric(NA))
  
  i<-skip.hash(0)
  range[c("minyear","maxyear")]<-scan(file,skip=i, nlines=1, 
                                      nmax=2, quiet =TRUE,
                                      na.strings=na.strings)
  i<-skip.hash(i)
  range[c("min","max","plusgroup")]<-scan(file,skip  =i, 
                                          nlines=1, 
                                          nmax  =3, 
                                          quiet =TRUE,
                                          na.strings=na.strings)
  
  ## Number of Indices
  i<-skip.hash(i+1)
  nIdx<-scan(file,skip=i,nlines=1,nmax=1,quiet=TRUE)
  
  ## Summary of PDFs etc  
  i<-skip.until.minus.1(i)
  i<-skip.hash(i)
  
  smry<-array(0,dim=c(nIdx,7))
  
  ## Indices of abundance
  j <- numeric(1)
  for (i in i:(i+nIdx-1)) {
    j<-j+1
    smry[j,]<-scan(file,skip =i,nlines=nIdx,
                   nmax =7,
                   quiet=TRUE,na.strings=na.strings)}
  
  i<-skip.hash(i)
  i<-skip.hash(i)
  index<-as.data.frame(array(0,dim=c(skip.until.minus.1(i)-i, 4)))
  
  for (j in i:(skip.until.minus.1(i)-1))
    index[j-i+1,]<-scan(file,skip  =j,
                        nlines=1,
                        nmax  =4,quiet=T,na.strings=na.strings)
  names(index)=c("series","year","data","data")
  cv =dlply(index[,-3],.(series), function(x) as.FLQuant(x[,-1],quant="age"))
  idx=dlply(index[,-4],.(series), function(x) as.FLQuant(x[,-1],quant="age"))
  
  ## Partial catches etc 
  i<-skip.until.minus.1(i)+1       
  p<-read.table(file,skip=i,fill=T,
                nrows=(skip.until.minus.1(i)-i-3),
                colClasses="numeric",
                na.strings=na.strings)
  p=melt(p,id=c("V1","V2"))
  names(p)=c("series","year","age","data")
  p$age=as.numeric(p$age)+range["min"]-1
  p=dlply(p[p$series>0,],.(series),function(x) as.FLQuant(x[,c("age","year","data")]))
  
  ## weights-at-age 
  i<-skip.until.minus.1(i)+1     	
  w<-read.table(file,skip=i,fill=T,
                nrows=(skip.until.minus.1(i)-i-3),
                colClasses="numeric",
                na.strings=na.strings)
  w=melt(w,id=c("V1","V2"))
  names(w)=c("series","year","age","data")
  w$age=as.numeric(w$age)+range["min"]-1
  w=dlply(w[w$series>0,],.(series),function(x) as.FLQuant(x[,-1]))
  
  res<-FLIndices()
  for (j in seq(nIdx)){
    range=c(unlist(dims(idx[[j]]))[c("minyear","maxyear")],
            min=smry[j,6],max=smry[j,7])
    res[[j]]<-FLIndex(index      =idx[[j]],
                      index.var  =cv[[j]])
    
    if (ac(j) %in% names(w))
      res[[j]]@catch.wt=trim(w[[ac(j)]],age =range["min"    ]:range["max"],
                             year=range["minyear"]:range["maxyear"])
    else
      res[[j]]@catch.wt=FLQuant(NA,dimnames=list(age =range["min"    ]:range["max"],
                                                 year=range["minyear"]:range["maxyear"]))
    if (ac(j) %in% names(p))
      res[[j]]@catch.n=trim(p[[ac(j)]],age =range["min"    ]:range["max"],
                            year=range["minyear"]:range["maxyear"])
    else
      res[[j]]@catch.n=FLQuant(NA,dimnames=list(age =range["min"    ]:range["max"],
                                                year=range["minyear"]:range["maxyear"]))
    
    range[5]=NA
    
    # TIMING (-1 = AVERAGE DURING YEAR, POSITIVE INTEGER = NUMBER OF MONTHS ELAPSED)
    if (smry[j,5]==-1) 
      range[6:7]<-c(-1,-1)
    else   
      range[6:7]<-c(smry[j,5]/12,smry[j,5]/12)
    names(range)[5:7]=c("plusgroup","startf","endf")
    
    # PDF         (0= do not use,1=lognormal, 2=normal)
    if (smry[j,2]==2) {
      res[[j]]@distribution<-"normal"
    } else {
      res[[j]]@distribution<-"lognormal"
    }
    
    # UNITS       (1 = numbers, 2 = biomass)
    if (smry[j,3]==2) {
      res[[j]]@type<-"biomass" 
    } else {
      res[[j]]@type<-"numbers"
    }
    
    # SELECTIVITY (1 = fixed, 2 = fractional catches, 
    # 3 = Powers and Restrepo partial catches,4=Butterworth and Geromont eq 4)
    if (smry[j,4]==1) 
      res[[j]]@type<-c(res[[j]]@type,"sel") 
    else if (smry[j,4]==2) 
      res[[j]]@type<-c(res[[j]]@type,"catches")
    else if (smry[j,4]==3) 
      res[[j]]@type<-c(res[[j]]@type,"Powers")
    else if (smry[j,4]==4) 
      res[[j]]@type<-c(res[[j]]@type,"Butterworth")  
    names(res[[j]]@type)<-c("type")
    
    res[[j]]@range=range
  }
  
  return(res)}

partialF<-function(idx,object,years){
  if ("integer"%in%class(years)) years=as.character(years)
  paa=FLQuants(llply(idx,catch.n))
  caa=catch.n(object)
  faa=harvest(object)
  
  res=FLQuants(llply(paa,function(p){
    dmns=dimnames(p)
    pf  =faa[dmns$age,dmns$year]%*%p%/%caa[dmns$age,dmns$year]
    
    if (dim(p)[1]==1)
      FLQuant(1,dimnames=list(year=1,age=dmns$age))
    else{
      res=apply(pf[,years],1,function(x) sum(x))
      res=res/max(res)
      res
    }}))
  
  res}

partialFn<-function(object,stk){
  age=ac(range(object)["min"]:range(object)["max"])
  yrs=ac(range(object)["minyear"]:range(object)["maxyear"])
  
  paa=catch.n(object)
  caa=catch.n(stk)[age,yrs]
  faa=harvest(stk)[age,yrs]
  
  pf  =faa%*%paa%/%caa
  
  if (dim(paa)[1]==1)
    res=FLQuant(1,dimnames=list(year=1,age=age))
  else{
    res=apply(pf[,yrs],1,function(x) sum(x))
    res=res/max(res)
    res=FLQuant(res,dimnames=list(age=age,iter=dimnames(object@index)$iter))}
  
  res}

cpueHat<-function(object,pf,vpa){
  hat=apply(stock.wt(vpa)[dimnames(object@catch.n)$age, dimnames(object@index)$year]*
            stock.n( vpa)[dimnames(object@catch.n)$age, dimnames(object@index)$year]%*%
              pf,c(2,6),sum)
  
  sweep(hat,6,apply(hat,6,mean),"/")}

uHat<-function(object,stk,units="n"){
  
  age=ac(range(object)["min"    ]:range(object)["max"])
  yrs=ac(range(object)["minyear"]:range(object)["maxyear"])
  
  if (tolower(substr(type(object),1,1))=="b"){
    pf =partialFn(object,stk)
    
    if (tolower(substr(units(index(object)),1,1))=="b")
      hat=apply(stock.n( stk)[age, yrs]*
                stock.wt(stk)[age, yrs]%*%pf,c(2,6),sum)
    else
      hat=apply(stock.n(stk)[age, yrs]%*%pf,c(2,6),sum)
    }
  
  if (tolower(substr(type(object),1,1))=="f"){
    
    if (tolower(substr(type(object)[1],1,1))=="w")
      hat=apply(stock.n( stk)[age, yrs]*
                  stock.wt(stk)[age, yrw],c(2,6),sum)
    else
      hat=apply(stock.n(stk)[age, yrs],c(2,6),sum)
  }
  
  hat}

hindcast<-function(file,dirPrj,maxyear="missing",plusgroup=40){
  
  vpa        =readVPA2Box(file)
  vpaOriginal=vpa
  ages       =dimnames(m(vpa))[[1]]
  nits       =dims(vpa)$iter
  yrs        =read.table(paste(dirPrj,"SSmsy.STA",sep="/"),header=FALSE,skip=1)[,2]  
  if (!missing(maxyear))  
    yrs=yrs[yrs<=maxyear]
  
  vpaYrs=as.character(as.numeric(unlist(dims(vpa))["minyear"]):
                      as.numeric(unlist(dims(vpa))["maxyear"]))
  prjYrs=as.character((as.numeric(max(vpaYrs))+1):max(yrs))
  
  stock(vpa)=computeStock(vpa)
  vpa       =propagate(vpa,nits)
  vpa       =setPlusGroup(vpa,plusgroup)
  vpa       =window(vpa,end=maxyear)
  
  sel       =paste(dirPrj,"SELECTIVITY.OUT",sep="/")
  sel       =read.table(sel,skip=1)
  sel       =sel[seq(1,dim(sel)[1]-1,2),-(seq(3))]
  sel       =cbind(expand.grid(iter=0:nits,age=ages),data=unlist(c(sel)))
  sel       =as.FLQuant(sel)
  
  harvest(vpa)[,ac(prjYrs)]=sel[,,,,,-1]
  
  waa  =paste(dirPrj,"waa.txt",sep="/")
  waa  =subset(read.table(waa),V2==0)
  waa  =melt(waa[,-(1:2)],id=c("V3"))
  names(waa)=c("year","age","data")
  waa$age  =as.numeric(waa$age)
  waa  =as.FLQuant(waa)
  yrs. =dimnames(waa)$year[dimnames(waa)$year%in%dimnames(stock.wt(vpa))$year]
  stock.wt(vpa)[,yrs.]=waa[,yrs.]
  discards.wt(vpa)=stock.wt(vpa)
  catch(vpa)=computeCatch(vpa, slot="all")
  stock(vpa)=computeStock(vpa)
  catch.wt(vpa)=stock.wt(vpa)
  
  ## SOP Wts
  stock.wt(vpa)[,vpaYrs]=stock.wt(vpa)[,vpaYrs]%*%(ssb(vpaOriginal)/ssb(vpa)[,vpaYrs])
  
  rec  =paste(dirPrj,"RECRT.OUT",sep="/")
  rec  =read.table(rec)
  rec  =melt(rec,id=c("V1","V2"))[,-1]
  names(rec)=c("iter","year","data")
  rec$year  =as.numeric(yrs)[rec$year]
  stock.n(vpa)[1]=as.FLQuant(rec)[,,,,,-1]
  
  m(vpa)[,           prjYrs]=m(vpa)[,           max(vpaYrs)]
  m.spwn(vpa)[,      prjYrs]=m.spwn(vpa)[,      max(vpaYrs)]
  harvest.spwn(vpa)[,prjYrs]=harvest.spwn(vpa)[,max(vpaYrs)]
  mat(vpa)[,         prjYrs]=mat(vpa)[,         max(vpaYrs)]
  catch.wt(vpa)[,    prjYrs]=catch.wt(vpa)[,    max(vpaYrs)]
  stock.wt(vpa)[,    prjYrs]=stock.wt(vpa)[,    max(vpaYrs)]
  discards.wt(vpa)[, prjYrs]=discards.wt(vpa)[, max(vpaYrs)]
  
  vpa=fwd(vpa,catch=)
  
  return(vpa)} 

#' @title read.vpa2box
#' 
#' @description 
#' Reads in the results from VPA2Box into an \code{FLStock} object. This has slots for the estimates 
#' of numbers and fishing mortality-at-age and the assummed values of natural mortality, weight and 
#' maturity-at.age.
#' 
#' @param file the control file
#' @param m \code{vector} for natural mortality 
#' @param minage  the age of the first age in the data, 1 by default, 
#' @param retros read in retrospectives, TRUE by default, 
#' @param printFiles print the names of the individual file read in, FALSE by default
#' 
#' @return an \code{FLStock} object
#' 
#' @export
#' @rdname read.vpa2box
#' 
#' @examples
#' \dontrun{
#' 
#' } 
#' 
setGeneric('read.vpa2box',   function(file,...) standardGeneric('read.vpa2box'))
setMethod( 'read.vpa2box',   signature(file='character'), 
      function(file, m=NULL, minage=1, retros=TRUE, printFiles=FALSE, ...)
              readVPA2Box(file,m,minage,retros,printFiles)) 
  