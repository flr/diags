require(ss3diags)
require(r4ss)
require(tseries)
require(reshape2)

sss2jabba<-function(x){
  tmp=llply(ss,ss2jabba)
  d1 =ldply(tmp,function(x) x[[1]])
  d2 =ldply(tmp,function(x) x[[2]])
  d3 =ldply(tmp,function(x) x[[3]])
  d4 =ldply(tmp,function(x) x[[4]])
  
  names(d1)[1]="run"
  names(d2)[1]="run"
  names(d3)[1]="run"
  names(d4)[1]="run"
  
  rtn=list(d1,d2,d3,d4)
  names(rtn)=c(names(tmp[[1]])[1],names(tmp[[1]])[2],names(tmp[[1]])[3],names(tmp[[1]])[4])
  rtn}

ss2jabba<-function(x){
  
  if ("list"%in%is(x)) 
    {if (all(c("Data_File","Control_File")%in%names(x))) ss3rep=x else 
      stop("x needs to be a SS report object or an SS dir")}
  else if ("character"%in%is(x)){ 
    if (dir.exists(x)) ss3rep=SS_output(x, verbose=FALSE, printstats=FALSE, hidewarn=FALSE) else
      stop("x needs to be a SS report object or an SS dir")}
  else
    stop("x needs to be a SS report object or an SS dir")
  
  if ("character"%in%is(x)) setwd(x)
  else                      setwd(tempdir())
  
  File      =getwd()
  ssmodel   =paste(ss[[1]]$Data_File,ss[[1]]$Control_File)
  assessment="ss2jabba"
  dir.create(file.path(File,assessment))
  
  #ss3rep = ss3sma # (from pck diags)
  #----------------------------
  # Extract/Plot catch matrix
  #---------------------------
  Par = list(mfrow=c(1,1),mai=c(0.5,0.5,0.1,.1),omi = c(0.1,0.1,0.1,0.1) + 0.1,mgp=c(2,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0(File,"/",assessment,"/CatchFleet_",ssmodel,".png"), width = 6, height =4, 
      res = 200, units = "in")
  par(Par)
  catches = SSplotCatch(ss3rep,subplots = 2)$totcatchmat
  catches = catches[,c(ncol(catches),1:(ncol(catches)-1))]
  dev.off()
  
  par.save = par
  # Extract residual for by scenario and index 
  d =ss3rep$cpue
  d$scenario = factor("BaseCase_S3")
  names(d)
  scenarios = (levels(d$scenario))
  
  #--------------------------------
  # Extract Vulnarable biomass (fit)
  #--------------------------------
  fits = aggregate(Exp~Yr+Fleet_name,d,mean)
  idd = paste0(fits[,2])
  id = levels(factor(idd))
  tot.catch = catches
  fityr = catches[-1,1]
  
  # Reference biomass EU-LL (highest F, highst Vuln.Bio(SELEX))
  refid = id[5]
  ref.yr = fits[fits$Fleet_name == refid,1]
  ref.mu = mean(fits[fits$Fleet_name == refid,3])
  fit.mat = fit.raw = matrix(NA,nrow=length(fityr),ncol=length(id))
  fit.dat = NULL
  for(i in 1:length(id)){
    fit.temp = fits[fits$Fleet_name==id[i],]
    fit.mat[fityr%in%fit.temp[,1],i] = fit.temp[,3]/mean(fit.temp[fit.temp[,1]%in%ref.yr,3])*ref.mu
    fit.raw[fityr%in%fit.temp[,1],i] = fit.temp[,3]
    fit.dat = rbind(fit.dat,data.frame(fit.temp[,1:2],Vuln_bio=fit.temp[,3]/mean(fit.temp[fit.temp[,1]%in%ref.yr,3])*ref.mu)) 
  }
  cols = rainbow(length(id))
  
  Par = list(mfrow=c(1,1),mar = c(3, 3, 1, 1), mgp =c(2,0.8,0), tck = -0.02,cex=0.8)
  png(file = paste0(File,"/",assessment,"/CPUEfit_",ssmodel,".png"), width = 5, height = 4, 
      res = 200, units = "in")
  par(Par)
  plot(fityr,fit.mat[,1],ylim=c(0,max(fit.mat,na.rm=TRUE)),type="n",xlim=range(c(fits[,1],2017)),ylab="Alligned CPUE fits (N)",xlab="Year")  
  for(i in 1:length(id)) points(fityr,fit.mat[,i],bg=cols[i],pch=21)  
  lines(unique(fit.dat[,1]),apply(fit.mat[fityr%in%unique(fits[,1]),],1,mean,na.rm=T),lwd=2)
  lines(c(rep(2010,2)),c(0,max(fit.mat,na.rm=TRUE)*0.85),lty=2)
  lines(c(rep(2015,2)),c(0,max(fit.mat,na.rm=TRUE)*0.85),lty=2)
  text(2010,max(fit.mat,na.rm=TRUE)*0.89,"ICCAT 2012",cex=0.75)
  text(2015,max(fit.mat,na.rm=TRUE)*0.89,"ICCAT 2017",cex=0.75)
  
  legend("bottomleft",c(paste(id),paste("Mean")),pt.bg = c(cols,-1,-1),pch=c(rep(21,length(id)),-1,-1),lwd=c(rep(-1,length(id)),2,2),col=c(rep(1,length(id)),1,2),bty="n",cex=0.8)
  dev.off()
  
  
  #SSplotSelex(ss3rep,subplot = 2)
  #-----------------------------------------------
  # Get Vulnarable Biomass as function of N_at_age & w_at_age
  #-------------------------------------------------------
  Na = ss3rep$natage
  yr.vb =unique(Na[Na$`Beg/Mid`=="B" & Na$Era=="TIME","Yr"]) 
  NaF = Na[Na$`Beg/Mid`=="B" & Na$Era=="TIME" & Na$Morph==1,12:(ncol(Na))]
  NaM = Na[Na$`Beg/Mid`=="B" & Na$Era=="TIME" & Na$Morph==2,12:(ncol(Na))]
  wtaF = ss3rep$mean_body_wt[4,4:(ncol(ss3rep$mean_body_wt))]
  wtaM = ss3rep$mean_body_wt[2,4:(ncol(ss3rep$mean_body_wt))]
  #VBa = as.matrix(NaF[,-1])%*%diag(wtaF)+as.matrix(NaM[,-1])%*%diag(wtaM)
  VBa = as.matrix(NaF[,])%*%diag(wtaF)+as.matrix(NaM[,])%*%diag(wtaM)
  
  tmat = 18
  #ssb = apply(as.matrix(NaF[,-1])%*%diag(wtaF)[,tmat:30],1,sum)
  ssb = apply(as.matrix(NaF)%*%diag(wtaF)[,tmat:30],1,sum)
  
  minA = 3
  maxA = rev(seq(9,30,3))
  Par = list(mfrow=c(1,2),mar = c(3, 3, 1, 1), mgp =c(2,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0(File,"/",assessment,"/ssBio_",ssmodel,".png"), width = 7, height = 3.5, 
      res = 200, units = "in")
  par(Par)
  i=1
  ssVB = data.frame(Yr=yr.vb,tb = ss3rep$timeseries$Bio_all[ss3rep$timeseries$Era=="TIME"],ssb)
  cols= rainbow(length(maxA))
  plot(ss3rep$timeseries$Yr,ss3rep$timeseries$Bio_all,ylim=c(0,max(ss3rep$timeseries$Bio_all)*1.1),ylab="Biomass",xlab="Year",lwd=2,col=1,type="l")
  lines(yr.vb,ssb,lwd=2,col=2,type="l")
  lines(ss3rep$timeseries$Yr,ss3rep$timeseries$SpawnBio,lwd=2,lty=5,col=2,type="l")
  
  for(i in 1:length(maxA)){
    lines(yr.vb,apply(VBa[,minA:maxA[i]],1,sum),col=cols[i])
    ssVB = cbind(ssVB,apply(VBa[,minA:maxA[i]],1,sum))
  }
  colnames(ssVB) = c(names(ssVB[,1:3]),paste0("VB.age",minA,"-",maxA))
  plot(ss3rep$timeseries$Yr,ss3rep$timeseries$Bio_all/max(ss3rep$timeseries$Bio_all),ylim=c(0,1),ylab="Biomass/B0",xlab="Year",lwd=2,col=1,type="l")
  for(i in 1:length(maxA)){
    lines(yr.vb,apply(VBa[,minA:maxA[i]],1,sum)/max(apply(VBa[,minA:maxA[i]],1,sum)),col=cols[i])
  }
  lines(yr.vb,ssb/max(ssb),lwd=2,col=2,type="l")
  lines(ss3rep$timeseries$Yr,ss3rep$timeseries$SpawnBio/max(ss3rep$timeseries$SpawnBio),lwd=2,lty=5,col=2,type="l")
  abline(h=ssb[length(ssb)]/max(ssb),col=2,lty=2)
  legend("bottomleft",c("Biomass (age-1+)","SSB (age-18+)","SSF (Fecundity)",paste0("VB.Age",minA,"-",maxA)),col=c(1,2,2,cols),lwd=c(2,2,2,rep(1,20)),lty=c(1,1,2,rep(1,20)),cex=0.8,bty="n")
  dev.off()
  
  #------------------------------------------------------------------------------------
  # Plot Selectivity at Age
  #------------------------------------------------------------------------------------
  
  Par = list(mfrow=c(1,1),mar = c(3, 3, 1, 1), mgp =c(2.,0.8,0), tck = -0.02,cex=0.8)
  png(file = paste0(File,"/",assessment,"/ssSELEX_",ssmodel,".png"), width = 7, height = 7, 
      res = 200, units = "in")
  par(Par)
  SSplotSelex(ss3rep,subplot = 2)
  rect(3,0,9,100,col=grey(0.5,0.3))
  rect(18,0,100,100,col=rgb(1,0,0,0.1))
  dev.off()
  
  
  #--------------------------------------------------------------------
  # Extract CPUE
  #--------------------------------------------------------------------
  
  # Get CPUE
  CPUE.agg = aggregate(Obs~Yr+Fleet_name+Fleet, ss3rep$cpue,mean)
  fl = unique(CPUE.agg$Fleet)
  flname = unique(CPUE.agg$Fleet_name)
  nI = length(fl)
  # Add early catch years
  CPUE.agg = rbind(CPUE.agg,data.frame(Yr=catches[,1],Fleet_name="Z",Fleet=1000,Obs=NA)) 
  cpues = dcast(CPUE.agg,Yr~Fleet_name,value.var = "Obs")[-1,]
  # Reorganize
  cpues=cpues[,c("Yr",flname)]
  cpues = cpues[,which(names(cpues)%in%c("Yr",flname))]
  
  Par = list(mfrow=c(round(nI/2+0.01,0),ifelse(nI==1,1,2)),mai=c(0.35,0.15,0,.15),omi = c(0.2,0.25,0.2,0) + 0.1,mgp=c(2,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0(File,"/",assessment,"/ssIndices_",ssmodel,".png"), width = 7, height = ifelse(nI==1,5,ifelse(nI==2,3.,2.5))*round(nI/2+0.01,0), 
      res = 200, units = "in")
  par(Par)
  for(i in 1:nI){
    plot(Obs~Yr,data=CPUE.agg[CPUE.agg$Fleet==fl[i],],type="l",ylim=c(0,max(CPUE.agg[CPUE.agg$Fleet==fl[i],4])*1.1),col=4,lwd=2)  
    points(Obs~Yr,data=CPUE.agg[CPUE.agg$Fleet==fl[i],],pch=21,bg="white",cex=1.2)  
    legend("top",paste0(flname[i]),bty="n",cex=0.9,y.intersp = -0.2)
  }
  mtext(paste("Year"), side=1, outer=TRUE, at=0.5,line=1,cex=1)
  mtext(paste("CPUE"), side=2, outer=TRUE, at=0.5,line=1,cex=1)
  dev.off()
  # add cpue.se
  se.agg = aggregate(SE~Yr+Fleet_name+Fleet, ss3rep$cpue,mean)
  se.agg = rbind(se.agg,data.frame(Yr=cpues[,1],Fleet_name="Z",Fleet=1000,SE=NA)) 
  cpue.se = dcast(se.agg,Yr~Fleet_name,value.var = "SE")
  cpue.se = cpue.se[,names(cpues)]
  
  # write catch,cpue,se files
  jbinput  = list()
  jbinput$catch= data.frame(Yr=catches[,1], Total=apply(catches[,-1],1,sum))[-1,] 
  jbinput$cpue = cpues
  jbinput$se = cpue.se
  jbinput$bio.index = ssVB
  
  names(jbinput[[1]])=c("year","catch")
  names(jbinput[[2]])[1]=c("year")
  names(jbinput[[3]])[1]=c("year")
  names(jbinput[[4]])[1]=c("year")
  
  jbinput}



