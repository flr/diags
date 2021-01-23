fnlim<-function(p=1.01,r=0.5,K=1,B=K*0.0001){
  fd<-deriv(~r/p*B*(1-(B/K)^p),"B")
  
  attributes(eval(fd))$gradient}

fnmsy<-function(p=1.01,r=0.5)
  r*(1/(1+p))

fn<-function(par=c(0.5,1.01),fmsy=0.25,flim=0.5) {
  r=par[1]
  p=par[2]
  (flim-fnlim(p,r))^2+(fmsy-fnmsy(p,r))^2}

#dat=subset(ices,!is.na(Flim)&!is.na(FMSY))[,c("Source","Flim","FMSY")]

#res=ddply(dat,.(Source), with, {
#  res=optim(c(r=0.5,p=0.6),fn,flim=1-exp(-Flim),fmsy=1-exp(-FMSY))$par
#  if ("try-error"%in%is(res)) return(NULL) else return(res)})
