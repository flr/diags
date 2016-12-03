utils::globalVariables(c("readFLIndices","catch.n"))

.writeUss=function(x,file,...){
  
  #install.packages("FLAssess",repos="http://R-Forge.R-project.org")
  # 4/Sept/2012, Start of writing a conversion script for FLR data to SS
  #FLRdir <- "C:\\Dropbox\\SScourse\\Sweden\\Examples\\SwedenData SS3\\Eastern Baltic cod"
  
  VPA2SS_tuning <- function(fn,minAge,maxAge,season,fleet,gender,partition,ageerr,sampSize,outFileName=NULL) {
    xx  <- readFLIndices(fn)
    nInd <- length(xx)
    out <- vector(mode="list")
    if(length(season)==1) season <- rep(season,nInd)
    if(length(fleet)==1) fleet <- rep(fleet,nInd)
    if(length(gender)==1) gender <- rep(gender,nInd)
    if(length(partition)==1) partition <- rep(partition,nInd)
    if(length(ageerr)==1) ageerr <- rep(ageerr,nInd)
    if(length(sampSize)==1) sampSize <- rep(sampSize,nInd)
    if(length(sampSize)==nInd) sampSize <- as.list(sampSize)
    
    for(i in 1:nInd) {
      rng <- range(xx[[i]])
      cn <- t(matrix(catch.n(xx[[i]]),nrow=rng["max"]-rng["min"]+1))
      out[[i]] <- cbind(rng["minyear"]:rng["maxyear"],season[i],fleet[i],gender[i],partition[i],ageerr[i],-1,-1,sampSize[[i]],matrix(0,nrow=nrow(cn),ncol=maxAge-minAge+1))
      dimnames(out[[i]])[[2]] <- c("#Year","Season","Fleet","Gender","Part","AgeErr","LbinLo","LbinHi","sampSize",as.character(minAge:maxAge))
      ###should do some checks to let user know if data ages are outside of set ages
      if(rng["min"]<minAge) {cat("minimum age in data is less than input minimum age.\n")}
      if(rng["max"]>maxAge) {cat("maximum age in data is greater than input maximum age.\n")}
      out[[i]][,as.character(rng["min"]:rng["max"])] <- cn 
    }
    names(out) <- names(xx)
    if(is.null(outFileName)) {
      return(out)
    }
    if(!is.null(outFileName)) {
      cat("#",desc(xx[[1]]),"\n\n",file=outFileName)
      cat(dimnames(out[[1]])[[2]],"\n",file=outFileName,append=T)
      for(i in 1:nInd) {
        cat("#",names(out)[i],"\n",file=outFileName,append=T)
        write.table(out[[i]],file=outFileName,row.names=F,col.names=F,append=T)
        cat("\n",file=outFileName,append=T)
      }
    }
  }
  
  #look at results
  #VPA2SS_tuning(paste(FLRdir, "c2532Tun1.txt", sep="/"),minAge=2,maxAge=8,season=1,fleet=c(NA,NA,3,2),gender=0,partition=0,ageerr=1,sampSize=300)
  
  #write out tables to a file for pasting into SS data file
  #VPA2SS_tuning(paste(FLRdir, "c2532Tun1.txt", sep="/"),minAge=2,maxAge=8,season=1,fleet=c(NA,NA,3,2),gender=0,partition=0,ageerr=1,sampSize=300,
  #                        outFileName="C:\\Dropbox\\SScourse\\Sweden\\Examples\\SwedenData SS3\\EBcod_SS\\ssIndices.txt")
  
  
  return(TRUE)}
