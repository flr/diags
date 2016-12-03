iUMFCL=function(plotFrqFile){
            
        mm=as.data.frame(read.frq(plotFrqFile)$mat[-1,1:6])
        mm$effort[mm$effort<0]=NA
        mm=transform(mm,index=catch/effort,
                         month=qtr+week/4,
                         name =fishery)[,c("name","year","month","qtr","week","index","catch","effort")]
        
        attributes(mm)$smry=mm$fish
        return(mm)}


.iUMFCL <-
  function(plotrepfile="plot.rep"){
    
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


scanText<-function(string, what = character(0), ...){
  ## Like scan() but reading from a vector of character strings
  tc <- textConnection(string)
  result <- scan(tc, what = what, quiet = TRUE, ...)
  close(tc)
  return(result)}

getplotdat1 <- function(h="",plotrepfile,skip=1) {
  ##=================================================
  ## List single line after header h. 
  ##=================================================
  dat <- readLines(plotrepfile)
  recnum <- grep(h, dat)
  scanText(dat[recnum + skip],what=0)
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


"getnfish" <-
  function(plotrepfile="plot.rep"){
    ##==============================================================
    ## Number of fisheries
    ##==============================================================
    getplotdat1(plotrepfile,h="# Number of fisheries")
  }

"getnreal" <-
  function(plotrepfile="plot.rep"){
    ###==============================================================
    ### Number of realizations per fishery
    ###==============================================================
    getplotdat1(plotrepfile,h="# Number of realizations per fishery")
  }

"getrtimes" <-
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

"getCPUEolist" <-
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

