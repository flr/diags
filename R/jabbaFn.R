#-------------------------------------------------
# Function kobeJabbaProj for projections with FLR
#-------------------------------------------------
kobeJabbaProj<-function(x,minyear=1,tac=NULL){
  
  out=cbind(melt(x[,,,2]),c(x[,,,3]))
  names(out)=c("iter","year","tac","stock","harvest")
  out$year=out$year+minyear-1
  
  out}

#------------------------------------
# Function kobeJabba for FLR
#------------------------------------
kobeJabba<-function(x,minyear=1){
  
  out=cbind(melt(x[,,2]),c(x[,,3]))
  names(out)=c("iter","year","stock","harvest")
  out$year=out$year+minyear-1
  out}

#--------------------------------------------------
# Function to get lognormal prior parameters
#--------------------------------------------------
plot_lnorm<-function(mu,CV,Prior="x"){
  sdev= sqrt(log(CV^2+1))
  rand.pr = rlnorm(1000,log(mu)-0.5*sdev^2,sdev)
  x = seq(min(rand.pr),quantile(rand.pr,0.995),max(rand.pr/500))  
  pdf = dlnorm(x,log(mu),sdev)  
  plot(x,pdf,type="l",xlim=range(x),xlab=paste(Prior),ylab="",yaxt="n")
  polygon(c(x,rev(x)),c(rep(0,length(x)),rev(ifelse(pdf==Inf,100000,pdf))),col="grey")
  
  return(c(mu,sdev))
  }

#-----------
# FUNCTIONS
#-----------
cat(paste0("\n","><> Prepare JABBA prior inputs <><","\n"))

#--------------------------------------------------
# Function to get beta prior parameters
#--------------------------------------------------
get_beta<-function(mu,CV,Min=0,Prior="x"){
  a = seq(0.0001,1000,0.001)
  b= (a-mu*a)/mu
  s2 = a*b/((a+b)^2*(a+b+1))
  sdev = sqrt(s2)
  # find beta )parameter a
  CV.check = (sdev/mu-CV)^2
  a = a[CV.check==min(CV.check)]
  #find beta parameter b
  b = (a-mu*a)/mu
  x = seq(Min,1,0.001)  
  pdf = dbeta(x,a,b)  
  plot(x,pdf,type="l",xlim=range(x[pdf>0.01]),xlab=paste(Prior),ylab="",yaxt="n")
  polygon(c(x,rev(x)),c(rep(0,length(x)),rev(ifelse(pdf==Inf,100000,pdf))),col="grey")
  
  return(c(a,b))
  }

#--------------------------------------------------
# Function to get gamma prior parameters
#--------------------------------------------------

get_gamma<-function(mu,CV,Prior="x"){
  a = seq(0.00001,10000,0.0001)
  b = a/mu
  s2 = (a/b^2)
  sdev = sqrt(s2)
  # find beta )parameter a
  CV.check = (sdev/mu-CV)^2
  a = a[CV.check==min(CV.check)]
  #find beta parameter b
  b = a/mu
  x = sort(rgamma(1000,a,b))  
  pdf = dgamma(x,a,b)  
  plot(x,pdf,type="l",xlim=range(x[pdf>0.01]),xlab=paste(Prior),ylab="",yaxt="n")
  polygon(c(x,rev(x)),c(rep(0,length(x)),rev(ifelse(pdf==Inf,100000,pdf))),col="grey")

  return(c(a,b))
  }


if (FALSE){
  cat(paste0("\n","><> Plot Prior distributions in Input subfolder  <><","\n"))
  
  Par = list(mfrow=c(1,3),mai=c(0.5,0.1,0,.1),omi = c(0.1,0.2,0.1,0) + 0.1,mgp=c(2,1,0), tck = -0.02,cex=0.8)
  png(file = paste0(input.dir,"/Priors_",assessment,"_",Scenario,".png"), width = 9, height = 3, 
      res = 200, units = "in")
  par(Par)
  
  mtext(paste("Density"), side=2, outer=TRUE, at=0.5,line=1,cex=0.9)
  dev.off() 
}


if (FALSE){
  cat(paste0("\n","><> Plot assumed Surplus Production shape in Input subfolder  <><","\n"))
  
  
  # Plot MSY
  Par = list(mfrow=c(1,1),mai=c(0.6,0.3,0,.15),omi = c(0.1,0.2,0.2,0) + 0.1,mgp=c(2,1,0), tck = -0.02,cex=0.8)
  png(file = paste0(input.dir,"/Production",assessment,"_",Scenario,".png"), width = 6, height = 5, 
      res = 200, units = "in")
  par(Par)
  
  # Get Bmsy/B0 as a fucntion of M 
  Pmsy=(m)^(-1/(m-1))
  P = seq(0.0001,1,0.001) 
  SP = ifelse(P>Plim,r.pr[1]/(m-1)*P*(1-P^(m-1)),r.pr[1]/(m-1)*P*(1-P^(m-1))*4*P)
  #if(is.null(refBmsy)==TRUE) refBmsy = Bmsy
  plot(P,SP/max(SP),type="l",ylab="Relative Yield",xlab="B/B0",lwd=2)
  mtext(paste("Relative Yield"), side=2, outer=TRUE, at=0.6,line=1,cex=0.9)
  legend("topright",c("SPM"),col=c(1),lwd=2,bty="n")  
  
  
  if(Model==4){
    # shape density
    #dm = dgamma(seq(0.001,5,0.1),5,5)*m
    dm = dlnorm((seq(0.001,5,0.1)),log(m),shape.CV)
    dm = dm/max(dm)
    bmsyk  = (seq(0.001,5,0.1))^(-1/(seq(0.001,5,0.1)-1))
    
    polygon(c(bmsyk,rev(bmsyk)),c(dm,rep(0,length(dm))),col="grey",border=0)  
  }
  abline(v=Pmsy,lty=2)
  mtext(paste("Relative Yield"), side=2, outer=TRUE, at=0.6,line=1,cex=0.9)
  legend("topright",c("SPM"),col=c(1),lwd=2,bty="n")  
  dev.off()
}


#------------
# Plot Catch
#------------
# if (CATCH.plot){
#   cat(paste0("\n","><> Plot Catch in Input subfolder <><","\n","\n"))
#   
#   Par = list(mfrow=c(1,1),mar = c(5, 5, 1, 1), mgp =c(3,1,0), tck = -0.02,cex=0.8)
#   png(file = paste0(input.dir,"/Catches_",assessment,".png"), width = 7, height = 5, 
#       res = 200, units = "in")
#   par(Par)
#   plot(catch[,1],catch[,1],ylim=c(0,max(catch[,2:ncol(catch)],na.rm=TRUE)),ylab=paste0("Catch ",catch.metric),xlab="Year",type="n")
#   for(i in 2:ncol(catch)) lines(catch[,1],catch[,i],lty=(i-1),lwd=2)
#   legend("topright",paste(names(catch)[2:ncol(catch)]),lty=1:(ncol(catch)-1),bty="n")
#   dev.off()
# }
# 
# 
# 
# if (STATE.plot){  
#   cat(paste0("\n","><> Plot State-Space CPUE fits  in Input subfolder <><","\n"))
#   # get individual trends
#   fitted <- lower <- upper <- NULL
#   cpue.yrs = years[q1.y:n.years]
#   
#   for (t in 1:nrow(mCPUE)){
#     fitted[t] <- median(mod.cpue$BUGSoutput$sims.list$Y.est[,t])
#     lower[t] <- quantile(mod.cpue$BUGSoutput$sims.list$Y.est[,t], 0.025)
#     upper[t] <- quantile(mod.cpue$BUGSoutput$sims.list$Y.est[,t], 0.975)}
#   
#   
#   q.adj = apply(mod.cpue$BUGSoutput$sims.list$q,2,median)
#   
#   
#   Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
#   png(file = paste0(input.dir,"/CPUE_",assessment,"_",Scenario,".png"), width = 5, height = 3.5, 
#       res = 200, units = "in")
#   par(Par)
#   u.ylim = NULL
#   for(i in 1:n.indices){ u.ylim = c(u.ylim,exp(log(mCPUE[,i]/q.adj[i])+1.96*sqrt(mSE2[,i])))}  
#   ylim = c(0,max(u.ylim,na.rm=TRUE))
#   plot(0, 0, ylim = ylim, xlim = range(cpue.yrs), ylab = "Expected CPUE", xlab = "Year", col = "black", type = "n")
#   legend("topright",paste(indices),lwd=2,col=(jabba.colors)[1:n.indices],bty="n")
#   polygon(x = c(cpue.yrs,rev(cpue.yrs)), y = c(lower,rev(upper)), col = "gray", border = "gray90")
#   
#   for(i in 1:n.indices)
#   {
#     shift = runif(1,-0.1,0.1)
#     cols=jabba.colors[qs[i]]
#     plotCI(cpue.yrs+shift,mCPUE[,i]/q.adj[i],ui=exp(log(mCPUE[,i]/q.adj[i])+1.96*sqrt(mSE2[,i])),li=exp(log(mCPUE[,i]/q.adj[i])-1.96*sqrt(mSE2[,i])),add=TRUE,col= cols,pt.bg = cols,pch=21,gap=0)
#     lines(cpue.yrs+shift,mCPUE[,i]/q.adj[i], col = cols,lwd=2)
#     points(cpue.yrs+shift,mCPUE[,i]/q.adj[i], bg = cols,pch=21)
#   }
#   lines(cpue.yrs,fitted,lwd=2)
#   
#   dev.off()
# }
# 
# 
# 
# #----------------
# # Total Landings
# #----------------
# if (FALSE){
# Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
# 
# png(file = paste0(output.dir,"/Landings_",assessment,"_",Scenario,".png"), width = 5, height = 3.5, 
#     res = 200, units = "in")
# par(Par)
# 
# cord.x <- c(years,rev(years))
# y<-rep(0,length(years))
# plot(years,(TC),type="l",ylim=c(0,max(TC,na.rm=T)),lty=1,lwd=1.3,xlab="Year",ylab=paste0("Catch ",catch.metric),main="")
# polygon(cord.x,c(TC,rev(y)),col="gray",border=1,lty=1)
# dev.off()}
# 
# # if Catch estimated with CV
# if(add.catch.CV==TRUE){
#   Par = list(mfrow=c(1,1),mar = c(3.5, 3.5, 0.1, 0.1), mgp =c(2.,0.7,0), tck = -0.02,cex=0.8)
#   
#   png(file = paste0(output.dir,"/Catch.fit_",assessment,"_",Scenario,".png"), width = 5, height = 3.5, 
#       res = 200, units = "in")
#   par(Par)
#   # estimated Catch
#   estC = posteriors$estC
#   predC = apply(estC,2,quantile,c(0.5,0.025,0.975)) 
#   cord.x <- c(years,rev(years))
#   cord.y<-c(predC[2,],rev(predC[3,]))
#   plot(years,(TC),type="n",ylim=c(0,max(predC,na.rm=T)),lty=1,lwd=1.3,xlab="Year",ylab=paste0("Catch ",catch.metric),main="")
#   polygon(cord.x,cord.y,col="gray",border=0,lty=1)
#   lines(years,predC[1,],lwd=2,col=4)
#   points(years,(TC),pch=21,bg=0,cex=1.5)
#   legend("topright",c("Observed","Predicted"),pch=c(21,-1),bg=0,lwd=c(-1,2),col=c(1,4),bty="n")
#   dev.off() 
#   }
# 
# if (FALSE){
#   
#   #------------------------------
#   # Plot Posteriors
#   #------------------------------
#   sel.par = c(1,2,7,4,3,5)
#   
#   out=data.frame(posteriors[params[sel.par]])
#   if(nSel>1) out=out[,-c(3:(3+nSel-2))]
#   
#   node_id = names(out)
#   #informative priors
#   Prs = as.matrix(cbind(K.pr,r.pr,c(0,0),psi.pr))
#   
#   #Posteriors
#   Par = list(mfrow=c(round(length(node_id)/3+0.33,0),3),mai=c(0.4,0.1,0,.1),omi = c(0.3,0.5,0.1,0) + 0.1,mgp=c(1,0.1,0), tck = -0.02,cex=0.8)
#   png(file = paste0(output.dir,"/Posteriors_",assessment,"_",Scenario,".png"),width  = 8, height = 2.5*round(length(node_id)/3,0), 
#       res = 200, units = "in")
#   par(Par)
#   
#   
#   node_id = names(out)
#   
#   
#   #par(mfrow=c(4,2),oma=c(0,1,1,0), mar=c(4,4,1,1))
#   
#   for(i in 1:length(node_id))
#   {
#     
#     post.par = as.numeric(unlist(out[paste(node_id[i])]))
#     
#     if(i==1){
#       
#       rpr = rlnorm(10000,log(K.pr[1]),K.pr[2]) 
#       pdf = stats::density(post.par,adjust=2)  
#       prior = dlnorm(sort(rpr),log(K.pr[1]),K.pr[2])   
#       plot(pdf,type="l",ylim=range(prior,pdf$y),xlim=range(c(pdf$x,quantile(rpr,c(0.0001,0.95)))),yaxt="n",xlab="K",ylab="",xaxs="i",yaxs="i",main="")
#       
#       polygon(c(sort(rpr),rev(sort(rpr))),c(prior,rep(0,length(sort(rpr)))),col=gray(0.4,1))
#       polygon(c(pdf$x,rev(pdf$x)),c(pdf$y,rep(0,length(pdf$y))),col=gray(0.7,0.7))
#       legend('right',c("Prior","Posterior"),pch=22,pt.cex=1.5,pt.bg = c(grey(0.4,1),grey(0.8,0.6)),bty="n")
#       PPVR = round((sd(post.par)/mean(post.par))^2/(sd(rpr)/mean(rpr))^2,3)  
#       PPVM = round(mean(post.par)/mean(rpr),3)
#       legend("topright",c(paste("PPMR =",PPVM),paste("PPVR =",PPVR)),cex=1,bty="n")  
#       
#     }  
#     
#     
#     if(i==2){
#       
#       rpr = rlnorm(10000,log(Prs[1,i]),Prs[2,i]) 
#       pdf = stats::density(post.par,adjust=2) 
#       prior = dlnorm(sort(rpr),log(Prs[1,i]),Prs[2,i])   
#       plot(pdf$x,pdf$y,type="l",ylim=range(prior,pdf$y),xlim=range(c(post.par,quantile(rpr,c(0.0001,0.95)))),yaxt="n",xlab=paste(node_id[i]),ylab="",xaxs="i",yaxs="i")
#       polygon(c(sort(rpr),rev(sort(rpr))),c(prior,rep(0,length(sort(rpr)))),col=gray(0.4,1))
#       polygon(c(pdf$x,rev(pdf$x)),c(pdf$y,rep(0,length(pdf$y))),col=gray(0.7,0.7))
#       PPVR = round((sd(post.par)/mean(post.par))^2/(sd(rpr)/mean(rpr))^2,3)  
#       PPVM = round(mean(post.par)/mean(rpr),3)
#       legend("topright",c(paste("PPMR =",PPVM),paste("PPVR =",PPVR)),cex=1,bty="n")
#       
#       
#     }
#     
#     if(i==3){
#       if(Model<4){
#         plot(1,1,type="n",xlim=range(0.5,2.5),yaxt="n",xlab=paste(node_id[i]),ylab="",xaxs="i",yaxs="i")  
#         abline(v=m,lwd=2)}
#       if(Model==4){
#         mpr = rlnorm(10000,log(m),shape.CV) 
#         pdf = stats::density(post.par,adjust=2) 
#         prior = dlnorm(sort(mpr),log(m),shape.CV)   
#         plot(pdf$x,pdf$y,type="l",ylim=range(prior,pdf$y),xlim=range(c(post.par,quantile(rpr,c(0.0001,0.95)))),yaxt="n",xlab=paste(node_id[i]),ylab="",xaxs="i",yaxs="i")
#         
#         polygon(c(sort(mpr),rev(sort(mpr))),c(prior,rep(0,length(sort(rpr)))),col=gray(0.4,1))
#         polygon(c(pdf$x,rev(pdf$x)),c(pdf$y,rep(0,length(pdf$y))),col=gray(0.7,0.7))
#         PPVR = round((sd(post.par)/mean(post.par))^2/(sd(rpr)/mean(rpr))^2,3)  
#         PPVM = round(mean(post.par)/mean(rpr),3)
#         legend("topright",c(paste("PPMR =",PPVM),paste("PPVR =",PPVR)),cex=1,bty="n")
#         
#         
#       }
#     }
#     
#     
#     if(i==4){
#       if(psi.dist=="beta"){
#         parm = fitdist(post.par[post.par<1 & post.par>0.01], "beta")$estimate
#         rpr = rbeta(10000,(psi.pr[1]),psi.pr[2]) 
#         pdf = stats::density(post.par,adjust=2)  
#         prior = dbeta(sort(rpr),psi.pr[1],psi.pr[2])   
#         PPVR = round((sd(post.par)/mean(post.par))^2/(sd(rpr)/mean(rpr))^2,3)  
#         PPVM = round(mean(post.par)/mean(rpr),3)
#         legend("topright",c(paste("PPMR =",PPVM),paste("PPVR =",PPVR)),cex=1,bty="n")
#         
#       } else {
#         rpr = rlnorm(10000,log(psi.prior[1]),psi.prior[2]) 
#         pdf = stats::density(post.par,adjust=2)  
#         prior = dlnorm(sort(rpr),log(psi.prior[1]),psi.prior[2])}
#       plot(pdf,type="l",ylim=range(quantile(c(prior,pdf$y,c(0,0.95)))),xlim=range(c(0.5,post.par,pdf$x,quantile(rpr,c(0.001,0.999)))),yaxt="n",xlab=paste(node_id[i]),ylab="",xaxs="i",yaxs="i",main="")
#       polygon(c(sort(rpr),rev(sort(rpr))),c(prior,rep(0,length(sort(rpr)))),col=gray(0.4,1))
#       polygon(c(pdf$x,rev(pdf$x)),c(pdf$y,rep(0,length(pdf$y))),col=gray(0.7,0.7))
#       PPVR = round((sd(post.par)/mean(post.par))^2/(sd(rpr)/mean(rpr))^2,3)  
#       PPVM = round(mean(post.par)/mean(rpr),3)
#       legend("topright",c(paste("PPMR =",PPVM),paste("PPVR =",PPVR)),cex=1,bty="n")
#       
#       #legend('topright',c("Prior","Posterior"),pch=22,pt.cex=1.5,pt.bg = c(grey(0.4,1),grey(0.8,0.6)),bty="n")
#     }        
#     
#     if(i>4){
#       if(sigma.proc!=TRUE & i==length(node_id)) {
#         plot(1,1,type="n",xlim=range(0,0.15^2),yaxt="n",xlab=paste(node_id[i]),ylab="",xaxs="i",yaxs="i")  
#         abline(v=sigma.proc^2,lwd=2)} else {
#           
#           pdf = stats::density(post.par,adjust=2)  
#           plot(pdf,type="l",xlim=range(0,post.par),yaxt="n",xlab=paste(node_id[i]),ylab="",xaxs="i",yaxs="i",main="")
#           if(i==length(node_id)& igamma[1]>0.9){
#             rpr = 1/rgamma(10000,igamma[1],igamma[2])
#             prior = stats::density(rpr,adjust=2)
#             polygon(c(prior$x,rev(prior$x)),c(prior$y,rep(0,length(prior$y))),col=gray(0.4,1))
#             PPVR = round((sd(post.par)/mean(post.par))^2/(sd(rpr)/mean(rpr))^2,3)  
#             PPVM = round(mean(post.par)/mean(rpr),3)
#             legend("topright",c(paste("PPMR =",PPVM),paste("PPVR =",PPVR)),cex=1,bty="n")
#             
#           }
#           
#           polygon(c(pdf$x,rev(pdf$x)),c(pdf$y,rep(0,length(pdf$y))),col=gray(0.7,0.7))
#           #legend('topright',c("Posterior"),pch=22,pt.cex=1.5,pt.bg = c(grey(0.8,0.6)),bty="n")
#         } }         
#     
#   }
#   mtext(paste("Density"), side=2, outer=TRUE, at=0.5,line=1,cex=0.9)
#   dev.off()   
#   
# }
  