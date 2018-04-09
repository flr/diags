ssSRR<-function(obj){
  
  #r=4hR0S/(S0(1-h)+S(5h-1))
  rhat<-function(par,ssb)
    (ssb%*%(4*par["s"]*par["r0"]))%/%(par["spr0"]%*%(1-par["s"])%+%ssb%*%(5*par["s"]-1))
  
  srr=seq(8)
  names(srr)=c("","ricker","bevholt","scaa","segreg", "bevholtFlat","survival","shepard")
  model=srr[obj$SRRtype]
  
  par  =obj$parameters[c("SR_LN(R0)","SR_BH_steep",
                         "SR_sigmaR","SR_envlink",
                         "SR_R1_offset", "SR_autocorr"),"Value"]
  
  names(par)=c("r0","s","sigmar","env","offset","ar")
  par["r0"]=exp(par["r0"])
  
  par=FLPar(c("v"=obj$SBzero,par,"spr0"=obj$SBzero/par["r0"]))
  dimnames(par)$params[8]="spr0"
  
  params=ab(par[c("v","s","spr0")],"bevholt")
  
  ssb   =as.FLQuant(transmute(base$recruit,year=Yr,data=SpawnBio))
  rec   =as.FLQuant(transmute(base$recruit,year=Yr,data=pred_recr))
  recHat=as.FLQuant(transmute(base$recruit,year=Yr,data=exp_recr))
  
  res=FLSR(ssb   =ssb,
           rec   =rec,
           model =names(model),
           params=params)
  
  res}
