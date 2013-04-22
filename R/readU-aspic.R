cpueCode=c("CE", "Fishing effort rate, catch (weight)",                  "Effort rate: annual average,Catch: annual total",
           "CC", "CPUE (weight-based), catch (weight)",                  "CPUE: annual average,Catch: annual total",
           "B0", "Estimate of biomass Effort rate: annual average",      "Start of year",
           "B1", "Estimate of biomass Catch: annual total",              "Annual average",
           "B2", "Estimate of biomass CPUE: annual average",             "End of year",
           "I0", "Index of biomass Catch: annual total",                 "Start of year",
           "I1", "Index of biomass Start of year",                       "Annual average", 
           "I2", "Index of biomass Annual average",                      "End of year")

cpueCode=t(array(cpueCode, dim=c(3,8),dimnames=list(c("code","desc","timing"),NULL)))
dimnames(cpueCode)[[1]]=cpueCode[,1]
cpueCode=transform(cpueCode[,-1],startf=c(0,0,0,0,1,0,0,1),
                   endf  =c(1,1,0,1,1,0,1,1),
                   ncol  =c(3,3,2,2,2,2,2,2),
                   col2  =c("effort","index","biomass","biomass","biomass","index","index","index"),
                   col3  =c("catch", "catch","",       "",       "",       "",     "",     ""))

ac=as.character

iUAspic=function(x){
    
  ## Years
  uYrs=scan(x,sep="\n",what=character())[22]
  uYrs=substr(uYrs,1,regexpr("#",uYrs)[1]-1)  
  uYrs=as.numeric(strsplit(uYrs," ")[[1]])[1]
  
  ## Serie lengths
  uN=scan(x,sep="\n",what=character())[12]
  uN=substr(uN,1,regexpr("#",uN)[1]-1)  
  uN=as.numeric(strsplit(uN," ")[[1]])[1]
  uN=rep(uYrs,uN)
  
  #uN=gsub("(\\s+)", " ", uN, perl=TRUE)
  #uN=strsplit(uN," ")[[1]]
  #uN=as.numeric(uN)
  
  uDat=str_trim(scan(x,sep="\n",what=character())[-(1:22)])
  uDat=maply(uDat,function(x) gsub("\"","",x))
  names(uDat)=NULL
  
  uDat=maply(uDat,function(x) gsub("d0","e+",x))
  names(uDat)=NULL
  uDat=maply(uDat,function(x) gsub("d-","e-",x))
  
  
  uPos=cumsum(c(rbind(1,1,uN)))
  nms=uDat[uPos[seq(1,length(uPos),3)]]
  cde=uDat[uPos[seq(2,length(uPos),3)]]
  cde=gsub('(\\s+)', '', cde, perl=TRUE)
  cde=substr(cde,1,2)
  
  flts=uDat[uPos[seq(1,length(uPos),3)]]
  flts=gsub('"',"", flts)
  
  uPos=mlply(data.frame(from=uPos[seq(2,length(uPos),3)]+1,
                        to  =uPos[seq(3,length(uPos),3)]),seq)
  
  cpue=llply(uPos,function(x) uDat[x])
  
  cpue=ldply(cpue, function(dat) mdply(data.frame(x=dat), function(x)  as.numeric(strsplit(gsub("\\s+"," ", x, perl=T)," ")[[1]]))[,-1])
  names(cpue)[1:2]=c("id.","year")
    
  cpue[cpue<0]=NA
  
  cpue=transform(cpue,code=cde[as.numeric(id.)])
  
  cpue=adply(cpue,1,function(x)
    with(x,switch(as.character(code),
        CE=data.frame(index  =V3/V2,effort=V2, catch=V3),
        CC=data.frame(index  =V2,   catch =V3),
        B0=data.frame(biomass=V2),
        B1=data.frame(biomass=V2),
        B2=data.frame(biomass=V2),
        I0=data.frame(index  =V2),
        I1=data.frame(index  =V2),
        I2=data.frame(index  =V2))))[,-(3:4)]

  smry=data.frame(unlist(dlply(cpue,.(id.), function(x) unique(x$code))),
                         ddply(cpue,.(id.), function(x) range(x$year)))
  names(smry)=c("code","name","minyr","maxyr")
  smry=smry[,c(2,3,4,1)]
  smry[,1]=flts[as.numeric(smry[,"name"])]
  
  names(cpue)[1]="name"
  cpue[,1]=flts[as.numeric(cpue[,1])]
  
  attributes(cpue)$smry=smry
  
  return(cpue)}


