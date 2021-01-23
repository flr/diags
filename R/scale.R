scale<-function(x,y,...){
  args=list(...)
  
  if (length(args)==0) group=rep(1,length(x)) else group=args[[1]]  
  
  gm=gam(y~lo(x)+group,data=data.frame(x=x,y=y,group=group))
  
  res=data.frame(hat =predict(gm),
                 y     =gm$y,
                 x     =x,
                 group =group,
                 scl   =c(0,coefficients(gm)[-(1:2)])[as.numeric(as.factor(group))]
  )
  res$y  =res$y  -res$scl
  res$hat=res$hat-res$scl
  
  if (length(args)==1) names(res)[4]=names(args)[1]  
  
  names(res)[1:4]=c("hat","data","year","name")
  res[,c(4,3,2,1)]}
