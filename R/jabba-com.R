# Calculate moving average
ma <- function(x){
  x.1 <- stats::filter(x,rep(1/3,3),sides=1)
  x.1[1] <- x[1]
  x.1[2] <- (x[1]+x[2])/2
  return(x.1)}

# Set end saturation prior CMSY2017 from datalimited2
dFinalPrior<-function(ct.raw, endb.low=NA, endb.hi=NA){
  
  nyr=length(ct.raw)
  ct =ma(ct.raw)
  
  # final biomass range from input file
  if(is.na(endb.low)==F & is.na(endb.hi)==F){
    endbio <- c(endb.low,endb.hi)
  }else{
    # else use mean final catch/max catch to estimate final biomass
    rawct.ratio=ct.raw[nyr]/max(ct)
    endbio <- if(ct[nyr]/max(ct) > 0.8) {c(0.4,0.8)} else if(rawct.ratio < 0.5) {c(0.01,0.4)} else {c(0.2,0.6)}
    
    # if default endbio is low (0.01-0.4), check whether the upper bound should be lower than 0.4 for depleted stocks
    if(endbio[2]==0.4){
      if(rawct.ratio< 0.05) {endbio[2] <- 0.1} else
        if(rawct.ratio< 0.15) {endbio[2] <- 0.2} else
          if(rawct.ratio< 0.35) {endbio[2] <- 0.3} else {endbio[2] <- 0.4}}
  }
  
  return(mean(endbio))}

dInitialPrior<-function(start.yr,stb.low=NA, stb.hi=NA){
  # use initial biomass range from input file if stated
  if(is.na(stb.low)==F & is.na(stb.hi)==F){
    startbio <- c(stb.low,stb.hi)
  }else{
    # if start year < 1960 assume high biomass
    if(start.yr < 1960){
      startbio <- c(0.5,0.9)
    }else{
      # else use medium prior biomass range
      startbio <- c(0.2,0.6)}}
  
  return(mean(startbio))}

jabbaComSetup<-function(ts,model="fox",dFinal="heuristic",dInitial="heuristic",r=0.4){

  #sink(file=tempfile())
  
  if (tolower(substr(model[1],1,1))=="f")                
    sa=list(assessment="com",
            scenario  ="CatchOnly",
            model.type="Fox",
            BmsyK     =0.2)
  else
    sa=list(assessment="com",
            scenario  ="CatchOnly",
            model.type="Pella",
            BmsyK     =0.37) 
  
  sa$catch=transmute(ts,Yr=year,Total=catch)
 
  if (is.na(dFinal[1])|tolower(substr(dFinal[1],1,1))=="h")
    sa$b.prior  =c(dFinalPrior(sa$catch$Total),0.2,max(sa$catch$Yr),"bbmsy")
  else if (is.numeric(dFinal))
    sa$b.prior=c(dFinal[1],0.2,max(sa$catch$Yr),"bbmsy")
  else
    sa$b.prior  =c(sa$ts$stock[length(sa$ts$stock)],0.2,max(sa$catch$Yr),"bbmsy")
  
  if (is.na(dInitial[1])|tolower(substr(dInitial[1],1,1))=="h")
    sa$psi.prior=c(0.9/sa$BmsyK,0.3) #c(dInitialPrior(min(sa$catch$Yr)),0.3)
  else if (is.numeric(dInitial))
    sa$psi.prior=c(dInitial[1],0.3)
  else
    sa$psi.prior=c(sa$ts$stock[1],0.3)
  
  sa$psi.prior[1] = min(sa$psi.prior[1],1)
  
  sa$sigma.proc  =0.1 #jb$estimates["sigma.proc","mu"]
  sa$proc.dev.all=T
  sa$sigma.est   =TRUE # estimate additional variance
  sa$fixed.obsE  =0.2  # minimum plausible
  
  sa$r.prior=c(r,0.3)
  if (model=="Fox") sa$r.prior[1]=sa$r.prior[1]*0.5
  
  #sink()
  
  return(sa)}


jabbaCom<-function(ts,model="fox",dFinal="heuristic",dInitial="heuristic",r=0.4,save.trj=FALSE){
  #sink(file=tempfile())
  
  sa=jabbaComSetup(ts,model,dFinal,dInitial,r)

  if ("try-error"%in%is(sa)) {warning("sa fail"); return(NULL)}
  
  jb=do.call("build_jabba",sa)
  jb=try(fit_jabba(jb,
                   save.trj=save.trj,
                   ni =5500,
                   nt =1,
                   nb =500,
                   nc =2))
  #sink()
  
  if ("try-error"%in%is(jb)) return(NULL)

  bd=try(jabba2biodyn(jb)) 
  
  if ("try-error"%in%is(bd)) return(NULL)
  
  if (TRUE) return(bd)
  
  model.frame(mcf(FLQuants(bd,
                           stock  =function(x) stock(  x)/refpts(x)["bmsy"],
                           harvest=function(x) harvest(x)/refpts(x)["fmsy"],
                           catch  =function(x) catch(  x)/refpts(x)["msy"])),drop=TRUE)}

jabbaComXvl<-function(ts,model="fox",dFinal="heuristic",dInitial="heuristic",r=0.4,nyr=10){
  
  #sink(file=tempfile())
  
  res=mlply(data.frame(tail=max(ts$year)-0:nyr), function(tail)
    jabbaCom(subset(ts,year<=tail),model,dFinal,dInitial,r))
  
  rs=res 
  rs[seq(nyr)+1]=mlply(data.frame(i=seq(nyr)),function(i){
       f  =harvest(res[[1]])[,ac(dims(res[[1]])$maxyear-(i:0))]
       f[]=c(f[,1])
       fwd(res[[i+1]],harvest=f)})
  
  #sink()
  
  rs}      

smryFn<-function(bd)
  model.frame(mcf(FLQuants(bd,
                         stock  =function(x) stock(  x)/refpts(x)["bmsy"],
                         harvest=function(x) harvest(x)/refpts(x)["fmsy"],
                         catch  =function(x) catch(  x)/refpts(x)["msy"])),drop=TRUE)

#jbinput = build_jabba(catch=iccat$bet$catch,cpue=iccat$bet$cpue
#                      ,se=iccat$bet$se,assessment="BET",
#                      ,model.type = "Pella_m",
#                      shape.CV = 0.3,
#                      sigma.est = FALSE,fixed.obsE = 0.01,)
