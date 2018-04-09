residual.nr<-function(obs,hat,se){
  cv      =se/obs
  sigma   =(log(1+(cv)^2))^0.5
  
  residual=log(obs/exp)/sigma

  data.frame(cv=cv,
             sigma=sigma,
             residual=residual)}

residual.nr.ln<-function(obs,hat,bin,n){
  p          =n/sum(n)  
  mu.obs     =sum(bin*obs)/sum(obs)
  mu.hat     =sum(bin*hat)/sum(hat)

  n          =mean(n)  
  sigma.nr   =(sum(p*(bin-mu.obs)^2)/n)^0.5
  residual   =(mu.obs-mu.hat)/sigma.nr
  residual.nr=residual/sigma.nr

  data.frame(mu.obs     =mu.obs, 
             mu.hat     =mu.hat, 
             residual.nr=residual.nr,
             sigma.nr   =sigma.nr)}
