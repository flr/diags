#file="/home/laurie/Desktop/ICCAT/SCRS/2012/bfte/VPA/2012/K2SM/2012/Reported/low/bfte2012.d1"

utils::globalVariables(c("variable","id."))

iU2box = function(file,na.strings="NA") {
  skip.hash=function(i) {
    i=i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
      i=i+1
    return(i)}
  
  skip.until.minus.1=function(i) {
    i=i+1
    while (scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
      i=i+1
    return(i)}
  
  # range
  range=numeric(5)
  names(range)=c("min","max","plusgroup","minyear","maxyear")
  i = skip.hash(0)
  range[c("minyear", "maxyear")] = scan(file,skip = i, nlines = 1, nmax = 2, quiet = TRUE,na.strings=na.strings)
  i = skip.hash(i)
  range[c("min", "max", "plusgroup")] = scan(file,skip = i, nlines = 1, nmax = 3, quiet = TRUE,na.strings=na.strings)
  i = skip.hash(i+1)
  NIndex = scan(file,skip=i,nlines=1,nmax=1,quiet=TRUE)
  i = skip.until.minus.1(i)
  i = skip.hash(i)
  smry. = array("",dim=c(NIndex,8))
  
  j = numeric(1)
  for (i in i:(i+NIndex-1)) {
    j=j+1
    smry.[j,]=scan(file,skip=i,nlines=1,nmax=8,quiet=TRUE,na.strings=na.strings,what=as.character())
  }
  
  i=skip.hash(i)
  i=skip.hash(i)
  index.=array(0,dim=c(skip.until.minus.1(i)-i, 4))
  for (j in i:(skip.until.minus.1(i)-1))
    index.[j-i+1,]=scan(file,skip=j,nlines=1,nmax=4,quiet=T,na.strings=na.strings)
  i=skip.until.minus.1(i)+1	   	
  if ((skip.until.minus.1(i)-i-3)>0)
    p.=read.table(file,skip=i,fill=T,nrows=(skip.until.minus.1(i)-i-3),colClasses="numeric",na.strings=na.strings)
  else p.=NULL
  
  smry=as.data.frame(smry.[,-1])
  dimnames(smry)=list(seq(NIndex),c("pdf","units","sel","timing","minage","maxage","index"))

  smry[,"pdf"]=factor(c("do not use","lognormal","normal")[as.numeric(smry[,"pdf"])+1],
               levels=c("do not use","lognormal","normal"))
  
  smry[,"units"]=factor(c("numbers","biomass")[smry[,"units"]],
                 levels=c("numbers","biomass"))
  
  smry[,"minage"]=as.numeric(as.character(smry[,"minage"]))
  smry[,"maxage"]=as.numeric(as.character(smry[,"maxage"]))
  
  smry[,"sel"]=factor(c("fixed","fractional","Powers and Restrepo","Butterworth and Geromont")[smry[,"sel"]],
               levels=c("fixed","fractional","Powers and Restrepo","Butterworth and Geromont"))
  
  # TIMING (-1 = AVERAGE DURING YEAR, POSITIVE INTEGER = NUMBER OF MONTHS ELAPSED)
  smry[,"timing"]=as.numeric(as.character(smry[,"timing"]))
  
  dimnames(index.)=list(NULL,c("index","year","cpue","cv"))
  index.=as.data.frame(index.)

  if (!is.null(p.)){
     dimnames(p.)[[2]][1:2]=c("index","year")
     p.=melt(p.,id=c("index","year"))
     p.=transform(p.,age=(range[1]:range[2])[as.numeric(variable)])[,c("index","year","age","value")]
     names(p.)[4]=c("fraction")
     p.          =p.[p.$index>=1,]
     p.$index    =smry[p.$index,    "index"]
     }
  
  
  index.$index=smry[index.$index,"index"]
  
  attributes(index.)$smry=smry
  
  index.$cv[  index.$cv  <0]=NA
  index.$cpue[index.$cpue<0]=NA
  
  names(index.)[1:4]=c("name","year","index","cv")
  return(index.)}
  #return(list(smry=smry,index=index.,fraction=p.))}
 
