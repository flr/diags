mkTmp<-function(){
  
  dr=tempfile()
  system(paste("mkdir",dr))
  dr}

rmTmp<-function(dir)
  t=system(paste("rm -R",dir))

