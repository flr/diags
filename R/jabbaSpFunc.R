spFuncJabba<-function(x){
  dat=x$inputseries$catch
  names(dat)[1:2]=c("year","catch")
  dat$biomass=x$timeseries[,"mu","B"][as.character(dat$year)]
  dat$sp=c(dat$biomass[-1],NA)-dat$biomass+dat$catch
  dat}