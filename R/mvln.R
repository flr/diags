require("mvtnorm") 
require("r4ss")  

#-----------------------------------------------------------------------------------------------------------------------
# ss_deltaMVLN Function to generate Kobe posteriors for Stock Synthesis Models
# from mles, vars and cov of log (F/Fmsy) and log(SSB/SSBmsy)
# Written by Henning Winker, Cape Town 2019 - henning.winker@gmail.com
# www.github.com/henning-winker/kobeMVLN [still to finish]
#-----------------------------------------------------------------------------------------------------------------------

ss_deltaMVLN = function(data,pars=c('Bratio','F'),year=c("last","all"),mc=1000,run="MVLN"){
  cv = data$CoVar
  # Get years
  yrs = unique(as.numeric(gsub(paste0(pars[1],"_"),"",cv$label.i[grep(paste(pars[1]), cv$label.i)])))
  if(year[1]=="last"){yrs=max(yrs)}
  if(year[1]!="last" & year[1]!="all"){yrs = as.numeric(year[1])}
  cv <- cv[cv$label.i %in% paste0(pars,"_",yrs),]
  cv$label.j[cv$label.j=="_"] <- cv$label.i[cv$label.j=="_"]
  hat = data$derived_quants
  if(is.null(hat$Label)){ylabel = hat$LABEL} else {ylabel=hat$Label}
  kb=mle = NULL
  for(yi in 1:length(yrs)){
    year= yrs[yi]
    x <- cv[cv$label.j %in% paste0(pars,"_",year),]
    y = hat[ylabel %in% paste0(pars,"_",year),] # old version Label not LABEL
    varF = log(1+(y$StdDev[1]/y$Value[1])^2) # variance log(F/Fmsy)  
    varB = log(1+(y$StdDev[2]/y$Value[2])^2) # variance log(SSB/SSBmsy)  
    cov = log(1+x$corr[3]*sqrt(varF*varB)) # covxy
    # MVN means of SSB/SBBmsy and F/Fsmy
    mvnmu = log(c(y$Value[2],y$Value[1])) # Assume order F_ then Bratio_
    # Create MVN-cov-matrix
    mvncov = matrix(c(varB,rep(cov,2),varF),ncol=2,nrow=2)
    kb.temp = data.frame(year=year,exp(rmvnorm(mc ,mean = mvnmu,sigma = mvncov)),run=run) # random  MVN generator
    colnames(kb.temp) = c("year","stock","harvest","run")
    kb = rbind(kb,kb.temp)
    mle = rbind(mle,data.frame(year,stock=y$Value[2],harvest=y$Value[1],run=run))
  }
  return(list(kb=kb,mle=mle))}


ss_covar=function(x){
  x

  # Get years
  yrs = unique(as.numeric(gsub(paste0(pars[1],"_"),"",cv$label.i[grep(paste(pars[1]), cv$label.i)])))
  if(year[1]=="last"){yrs=max(yrs)}
  if(year[1]!="last" & year[1]!="all"){yrs = as.numeric(year[1])}
  cv <- cv[cv$label.i %in% paste0(pars,"_",yrs),]
  cv$label.j[cv$label.j=="_"] <- cv$label.i[cv$label.j=="_"]
  hat = data$derived_quants
  if(is.null(hat$Label)){ylabel = hat$LABEL} else {ylabel=hat$Label}
  kb=mle = NULL
  for(yi in 1:length(yrs)){
    year= yrs[yi]
    x <- cv[cv$label.j %in% paste0(pars,"_",year),]
    y = hat[ylabel %in% paste0(pars,"_",year),] # old version Label not LABEL
    varF = log(1+(y$StdDev[1]/y$Value[1])^2) # variance log(F/Fmsy)  
    varB = log(1+(y$StdDev[2]/y$Value[2])^2) # variance log(SSB/SSBmsy)  
    cov = log(1+x$corr[3]*sqrt(varF*varB)) # covxy
    # MVN means of SSB/SBBmsy and F/Fsmy
    mvnmu = log(c(y$Value[2],y$Value[1])) # Assume order F_ then Bratio_
    # Create MVN-cov-matrix
    mvncov = matrix(c(varB,rep(cov,2),varF),ncol=2,nrow=2)
    kb.temp = data.frame(year=year,exp(rmvnorm(mc ,mean = mvnmu,sigma = mvncov)),run=run) # random  MVN generator
    colnames(kb.temp) = c("year","stock","harvest","run")
    kb = rbind(kb,kb.temp)
    mle = rbind(mle,data.frame(year,stock=y$Value[2],harvest=y$Value[1],run=run))
  }
  return(list(kb=kb,mle=mle))}
