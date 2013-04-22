
utils::globalVariables(c("read.frq","catch","effort","qtr","week","fishery","read.rep"))

utils::globalVariables(c("value.x"))
utils::globalVariables(c("value.y"))
utils::globalVariables(c("index"))
utils::globalVariables(c("residual"))


scanText<-function(string, what = character(0), ...){
  ## Like scan() but reading from a vector of character strings
  tc <- textConnection(string)
  result <- scan(tc, what = what, quiet = TRUE, ...)
  close(tc)
  return(result)}

getplotdat1<-function (h = "", plotrepfile, skip = 1) {
  dat <- readLines(plotrepfile)
  recnum <- grep(h, dat)
  scanText(dat[recnum + skip], what = 0)}


getnfish <-
  function(plotrepfile="plot.rep"){
    ##==============================================================
    ## Number of fisheries
    ##==============================================================
    getplotdat1(plotrepfile,h="# Number of fisheries")
  }

getnreal <-
  function(plotrepfile="plot.rep"){
    ###==============================================================
    ### Number of realizations per fishery
    ###==============================================================
    getplotdat1(plotrepfile,h="# Number of realizations per fishery")
  }

getplotdat4 <- function(h="",plotrepfile) {
  ##=================================================
  ## Start listing after header h.  Quit if encounter
  ##  "^#"
  ##=================================================
  dat <- readLines(plotrepfile)
  rec1 <- grep(h, dat)
  if(length(rec1) <= 0)
    stop(paste('"',h,'"',"not found in",plotrepfile," Die yuppie scum!"))
  recnum <- rec1+1
  tt <- numeric(0)
  for(i in recnum:length(dat)) {
    if (regexpr("^#", dat[i]) != -1) break
    tt <- c(tt, scanText(dat[i], what = 0))
  }
  tt
}


getrtimes <-
  function(plotrepfile="plot.rep"){
    ##==============================================================
    ## Time of each realization by fishery (down)
    ##==============================================================
    dat <- getplotdat4("# Time of each realization by fishery",plotrepfile)
    nreal <- getnreal(plotrepfile)
    nfish <- getnfish(plotrepfile)
    splitter <- rep(seq(nfish), nreal)
    split(dat, splitter)
  }

getCPUEplist=
  function(plotrepfile="plot.rep"){
    ##==============================================================
    ## Predicted CPUE by fishery (down) and time (across)
    ##==============================================================
    dat <- getplotdat4("# Predicted CPUE by fishery",plotrepfile)
    nreal <- getnreal(plotrepfile)
    nfish <- getnfish(plotrepfile)
    splitter <- rep(seq(nfish), nreal)
    split(dat, splitter)
  }

getCplist=
  function(plotrepfile="plot.rep"){
    ###==============================================================
    ### Predicted catch by fishery (down) and time (across)
    ###   Returns list w/ 1 element (vector) per fishery
    ###==============================================================
    dat <- getplotdat4("# Predicted catch by fishery",plotrepfile)
    nreal <- getnreal(plotrepfile)
    nfish <- getnfish(plotrepfile)
    splitter <- rep(seq(nfish), nreal)
    split(dat, splitter)
  }

getCPUEolist=
  function(plotrepfile="plot.rep"){
    ##==============================================================
    ## Observed CPUE by fishery (down) and time (across)
    ##==============================================================
    dat <- getplotdat4("# Observed CPUE by fishery",plotrepfile)
    nreal <- getnreal(plotrepfile)
    nfish <- getnfish(plotrepfile)
    splitter <- rep(seq(nfish), nreal)
    split(dat, splitter)
  }


getCPUEp=
  function(plotrepfile="plot.rep"){
    #==============================================================
    # 
    #==============================================================
    rtimes <- getrtimes(plotrepfile)
    qq <- data.frame(time=sort(unique(unlist(rtimes))))
    q <- getCPUEplist(plotrepfile)
    for(i in 1:length(q)) qq[[paste("flt",i,sep="")]] <-
      q[[i]][match(qq$time,rtimes[[i]])]
    qq
  }

getCPUEo= function(plotrepfile="plot.rep"){
    #==============================================================
    # 
    #==============================================================
    rtimes <- getrtimes(plotrepfile)
    qq <- data.frame(time=sort(unique(unlist(rtimes))))
    q <- getCPUEolist(plotrepfile)
    
    for(i in 1:length(q)) qq[[paste("flt",i,sep="")]] <-
      q[[i]][match(qq$time,rtimes[[i]])]
    qq
  }

.diagUmfcl=function(x){
  #x="/home/laurie/Desktop/Dropbox/ICCAT/SCRS/ALB/2010/inputs/MFCL scenarios/2009_4B/plot-09.par.rep"

  obs=getCPUEo(x)
  hat=getCPUEp(x)

  obs=melt(obs,id.var="time")
  hat=melt(hat,id.var="time")
  
  res=subset(merge(obs,hat,by=c("variable","time")), !is.na(value.x) & !is.na(value.y))
  names(res)[1:4]=c("name","time","obs","hat")
  
  res$residual=log(res$obs/res$hat)
  res$year    =floor(res$time)
  res$month   =res$time-res$year 

  res=res[do.call("order",res[,c("name","year")]),]  
  
  res=ddply(res,.(name),diagsFn)
  
  devs=read.rep(x)$qEdevAtAge
  devs=melt(t(devs))[,3]
  devs=devs[!is.na(devs)]
  res=data.frame(res,effDev=devs)
  
  res}



