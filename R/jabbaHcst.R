setUp<-function(year,name,jbinput,key){
    
    if (is.null(name))
        key[key$year>year,"value"]=NA
    else
        key[!(!(key$name%in%name)|(key$name%in%name&key$year<=year)),"value"]=NA
    
    Is       =seq(dim(jbinput$data$cpue)[2]-1)
    names(Is)=dimnames(jbinput$data$cpue)[[2]][-1]
    
    dat=cast(key,year~name)
    dimnames(dat)[[1]]=dat[,1]-jbinput$data$cpue[1,1]+1
    dat=dat[,-1]
    
    jbinput$jagsdata$I[as.numeric(dimnames(dat)[[1]]),Is[dimnames(dat)[[2]]]][]=as.matrix(dat)
    
    jbinput}

hcstJabba<-function(year,name,jb,key,K,r,q){
    jb$settings$scenario = paste0("hc",year,paste(name,collapse="."))  
    
    jb=setUp(year,name,jb,key)
    
    fit_jabba(jb,init.values=TRUE,
              init.K=K,
              init.r=r,
              init.q=q,
              ni    =5500,
              nt    =1,
              nb    =500,
              nc    =2)$diags}
