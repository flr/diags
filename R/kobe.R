
ss_trjs<-function(data=out,pars=c('SSB','Bratio','F')){
  out=data
  d. =out$derived_quants
  
  logCIs = function(mu,se){
    log.sd = sqrt(log(1+(se/mu)^2))
    logCIs =data.frame(exp(log(mu)+(1.96*log.sd)%o%c(-1,1)))
    colnames(logCIs) = c("lci","uci")
    return(logCIs)  
    }
    
  ssb =  d.[d.$Label %in% paste0(paste0(pars[1],"_"),yrs),2:3]
  stock = d.[d.$Label %in% paste0(paste0(pars[2],"_"),yrs),2:3] # old version Label not LABEL
  harvest = d.[d.$Label %in% paste0(paste0(pars[3],"_"),yrs),2:3]

  ssb = data.frame(year=as.numeric(substr(dimnames(ssb)[[1]],
                 regexpr("_",dimnames(ssb)[[1]])+1,nchar(dimnames(ssb)[[1]]))), 
                    ssb,logCIs(ssb[,1],ssb[,2]))
  stock = data.frame(year=as.numeric(substr(dimnames(stock)[[1]],
                 regexpr("_",dimnames(stock)[[1]])+1,nchar(dimnames(stock)[[1]]))),
                 stock,logCIs(stock[,1],stock[,2]))
  harvest = data.frame(year=as.numeric(substr(dimnames(harvest)[[1]],
                 regexpr("_",dimnames(harvest)[[1]])+1,nchar(dimnames(harvest)[[1]]))),
                      harvest,logCIs(harvest[,1],harvest[,2]))
  
  return(data.frame(ssb=ssb,stock=stock,harvest=harvest))
  }  
  
