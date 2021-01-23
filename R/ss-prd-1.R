rfs=c("ssb0",            "b0",               "b0.",              "r0",         "ssb1", "virgin",
      "ssbtar",          "sprtar",           "ftar",             "biomasstar", 
      "ssbsprtar",       "fsprtar",          "catchsprtar",      "ssbmsy", 
      "sprmsy",          "fmsy",             "msy",              "retainmsy",
      "msy") 
names(rfs)=tolower(
    c("SSB_Unfished",    "TotBio_Unfished",  "SmryBio_Unfished", "Recr_Unfished", "SSB_Initial","SSB_Virgin",
      "SSB_Btgt",        "SPR_Btgt",         "Fstd_Btgt",        "TotYield_Btgt", 
      "SSB_SPRtgt",      "Fstd_SPRtgt",      "TotYield_SPRtgt",  "SSB_MSY",  
      "SPR_MSY",         "Fstd_MSY",         "TotYield_MSY",     "RetYield_MSY",
      "Dead_Catch_MSY")) 

getTs<-function(x) {
  ts=merge(x$timeseries[,c(1:5,7:8)],ddply(x$catch,.(Yr), with,sum(Obs)))
  if (length(names(ts))==8)
    names(ts)=c("year","area","era","season","biomass","ssb","rec","catch")
  else
    names(ts)=c("year","era","season","biomass","ssb","rec","catch")
  
  ts}

getRf<-function(x){
  names(x$derived_quants)=tolower(names(x$derived_quants))

  x$derived_quants$label=tolower(x$derived_quants$label)
  
  rf=subset(x$derived_quants,
            label%in%tolower(c("SSB_Unfished","TotBio_Unfished","SSB_Initial","SSB_MSY","TotYield_MSY","Dead_Catch_MSY","Fstd_MSY")))[,1:5]
  rf[,1]=rfs[rf[,1]]
  dimnames(rf)[[1]]=rf[,1]
  
  cbind("quant"=c("hat","var"),as.data.frame(t(rf[,2:3])))}

smrySS<-function(x,covar=TRUE,forecast=TRUE,ncols=320){
  
  ss=llply(x, function(x) SS_output(x, verbose=FALSE,printstats=FALSE,covar=covar,forecast=forecast,ncols=ncols))
  ts=ldply(ss, getTs) 
  pf=ldply(ss, getPellaT)
  rf=ldply(ss, getRf)

  if (!is.null(attributes(x)$split_labels)){
    ts=cbind(attributes(x)$split_labels[ts$.id,],ts)
    pf=cbind(attributes(x)$split_labels[pf$.id,],pf)
    rf=cbind(attributes(x)$split_labels[rf$.id,],rf)
    }
    
  return(list(timeseries=ts,refpts=rf,pfunc=transform(pf,m=1+p)))}

getRefpts<-function(object,value=TRUE){
  names(object$derived_quants)=tolower(names(object$derived_quants))
  rf=subset(object$derived_quants,label%in%rfs[c(1,12,15)])[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","bmsy","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  rf[ifelse(value,1,2),]}

getPellaT<-function(object){
  rfs=c("SSB_Unfished",    "TotBio_Unfished",  "SmryBio_Unfished", "Recr_Unfished",    
        "SSB_Btgt",        "SPR_Btgt",         "Fstd_Btgt",        "TotYield_Btgt",   
        "SSB_SPRtgt",      "Fstd_SPRtgt",      "TotYield_SPRtgt",  "SSB_MSY",         
        "SPR_MSY",         "Fstd_MSY",         "TotYield_MSY",     "RetYield_MSY",
        "SSB_unfished","Dead_Catch_MSY") 
  
  names(object$derived_quants)=tolower(names(object$derived_quants))
  rf=subset(object$derived_quants,label%in%rfs[c(1,17,12,15,18)])[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","bmsy","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  pt=transmute(rf["value",],shape=bmsy/k,
               k    =k,
               p    =optimise(function(x,y) (y-(1/(1+x))^(1/x))^2,
                              c(-0.9999,10),y=shape)$minimum,
               r    =(1+p)*(msy/bmsy))
  
  pt[,c("k","r","p","shape")]}

productionFn<-function(b,r,k,p){
  t1=b*r/p
  t3=(b/k)^p
  t1*(1-t3)}

hat<-function(ssb,r,k,p) productionFn(ssb,r,k,p)
pe <-function(year,ssb,catch,hat){
        data.frame(year=year[-length(year)],
                 pe=log((ssb[-1]+catch[-length(ssb)]-hat[-length(ssb)])/ssb[-length(ssb)]))}

phase<-function(bmsy,msy,maxyield){
  
    fn1<-function(bmsy,msy)
            data.frame(x=c(0,  0, bmsy,bmsy,0),
                       y=c(0, Inf, Inf,   0,0))
    fn2<-function(bmsy,msy)
            data.frame(x=c(Inf, Inf,bmsy,bmsy,Inf),
                       y=c(Inf, Inf, Inf,   0,  0))
    fn3<-function(bmsy,msy)
            data.frame(x=c(0, bmsy,bmsy,0),
                       y=c(0,  msy, 0,  0))
    fn4<-function(bmsy,msy,maxyield)
            data.frame(x=c(bmsy,    bmsy, bmsy/msy*maxyield,bmsy),
                       y=c( msy,maxyield,          maxyield, msy))
  
    rbind(cbind(what=1,fn1(bmsy,msy)),
                 cbind(what=2,fn2(bmsy,msy)),
                 cbind(what=3,fn3(bmsy,msy)),
                 cbind(what=4,fn4(bmsy,msy,maxyield)))}


getPellaTBiomass<-function(object){
  rf=subset(object$derived_quants,label%in%c("TotBio_Unfished","TotYield_MSY"))[,2:3]
  names(rf)=c("value","var")
  dimnames(rf)[[1]]=c("k","msy")
  rf=t(as.matrix(rf))
  rf["var",]=rf["var",]^2
  rf=as.data.frame(rf)
  
  pt=transmute(rf["value",],
               shape=bmsy/k,
               k    =k,
               p    =optimise(function(x,y) (y-(1/(1+x))^(1/x))^2,
                              c(-0.9999,10),y=shape)$minimum,
               r    =(1+p)*(msy/bmsy))
  
  pt[,c("k","r","p","shape")]}

productionFn<-function(b,r,k,p){
  t1=b*r/p
  t3=(b/k)^p
  t1*(1-t3)}


spFunc<-function(x,add=FALSE,col2="blue",labels=c("","","biomass","Surplus Production"),plotIt=FALSE){
  # function to calculate and plot surplus production
  
  # timeseries excluding equilibrium conditions and forecasts
  ts <- x$timeseries[!x$timeseries$Era %in% c("VIRG","FORE"),]
  
  # get total dead catch
  stringB <- "dead(B)"
  catchmat <- as.matrix(ts[, substr(names(ts),1,nchar(stringB))==stringB])
  # aggregate catch across fleets
  catch <- rowSums(catchmat)
  
  # aggregate catch and biomass across seasons and areas
  catch_agg <- aggregate(x=catch, by=list(ts$Yr), FUN=sum)$x
  Bio_agg <- aggregate(x=ts$Bio_all, by=list(ts$Yr), FUN=sum)$x
  
  # number of years to consider
  Nyrs <- length(Bio_agg)
  sprod <- rep(NA, Nyrs)
  
  # calculate surplus production as difference in biomass adjusted for catch
  sprod[1:(Nyrs-1)] <- Bio_agg[2:Nyrs] - Bio_agg[1:(Nyrs-1)] + catch_agg[1:(Nyrs-1)]
  sprodgood <- !is.na(sprod)
  Bio_agg_good <- Bio_agg[sprodgood]
  sprod_good <- sprod[sprodgood]
  xlim <- c(0, max(Bio_agg_good, na.rm=TRUE))
  ylim <- c(min(0, sprod_good, na.rm=TRUE), max(sprod_good, na.rm=TRUE))
  
  if (plotIt){
    # make empty plot
    if(!add){
      plot(0, ylim=ylim, xlim=xlim, xlab=labels[3], ylab=labels[4], type="n")
    }
    # add lines
    lines(Bio_agg_good, sprod_good, col=col2)
    # make arrows
    old_warn <- options()$warn      # previous setting
    options(warn=-1)                # turn off "zero-length arrow" warning
    s <- seq(length(sprod_good)-1)
    arrows(Bio_agg_good[s], sprod_good[s], Bio_agg_good[s+1], sprod_good[s+1],
           length=0.06, angle=20, col=col2, lwd=1.2)
    options(warn=old_warn)  #returning to old value
    
    # add lines at 0 and 0
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    # add blue point at start
    points(Bio_agg_good[1], sprod_good[1], col=col2, bg="white", pch=21)}
  
  invisible(data.frame(year=seq(length(Bio_agg)),biomass=Bio_agg, sp=sprod, catch=catch_agg,ssb=ddply(ts,.(Yr),with, sum(SpawnBio,na.rm=TRUE))[,2]))}

