#x="/home/laurie/Desktop/Dropbox/ICCAT/SCRS/BIL/2012/section_6/Inputs/SS3/mcmc_diagnostics/Run_1"

utils::globalVariables(c("SS_output","FleetName","Yr","Obs","Exp","Dev","index","residual",".diagUbsp"))
utils::globalVariables(c("ddply"))
utils::globalVariables(c("."))
utils::globalVariables(c("diagsFn"))

.diagUss=function(x){
  res=subset(SS_output(x, forecast  =FALSE, 
                          covar     =FALSE,
                          verbose   =FALSE, 
                          printstats=FALSE, 
                          hidewarn  =TRUE, 
                          NoCompOK  =TRUE)$cpue, !is.na(Dev),select=c(Fleet_name,Yr,Seas,Obs,Exp,Dev,SE))
  

  names(res)=c("name","year","season","obs","hat","residual","se")
     
  res=subset(ddply(res,.(name,season),diagsFn),!is.na(residual))
  
  res}

# 
# if (FALSE){
# 	### SS3
# 	rns=c("/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/SS3_Final2/Run_1",
# 	      "/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/SS3_Final2/Run_2",
# 	      "/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/SS3_Final2/Run_3",
# 	      "/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/SS3_Final2/Run_4",
# 	      "/home/lkell/Dropbox/MyStuff/WHM/analysis/Inputs/SS3_Final2/Run_5")
# 
# 	tm=SS_output(rns[1])
# 
# 	ssCpue=fnDiags(transform(tm$cpue, year=Yr, cpue=Fleet, obs=Obs, hat=Exp, residual=log(Obs/Exp))[,c("year","cpue","obs","hat","residual")])
# 	ssCpue=merge(ssCpue,transform(tm$timeseries, year=Yr, stock=Bio_all)[,c("year","stock")])
# 	ssCpue=merge(ssCpue,ssU)
# 
# 	rm(SS_output)
# 	rm(SS_plots)
# 	update_r4ss_files()                                                                     
# 
# 	#no hessian
# 	rm(myreplist)
# 	myreplist <- SS_output(dir = "C:\\Work\\Assess\\ICCAT\\BUM\\SS\\7_2sex_v320", 
# 		                      printstats = T, covar=F, cormax=0.70, forecast=F,printhighcor=50, printlowcor=50)
# 	SS_plots(replist = myreplist,plot=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,22:49), 
# 		                     uncertainty=F, sprtarg=0.40, btarg=0.4, datplot=F, minbthresh=-1)                    
# 		                     print = 1:49, printfolder="R_plots", dir="default", pdf=F,
# 
# 
# 	#with hessian
# 	rm(myreplist)
# 	myreplist <- SS_output(dir = "C:\\Work\\Assess\\ICCAT\\BUM\\SS\\7_2sex_v320", 
# 		               printstats = T, covar=T, cormax=0.70, forecast=F,printhighcor=50, printlowcor=50)
# 	SS_plots(replist = myreplist,plot=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,22:49), 
# 		 uncertainty=T, sprtarg=0.40, btarg=0.4, datplot=F, minbthresh=-1                     
# 		 ,print = 1:49, printfolder="R_plots", dir="default")
# 
# 	SSplotPars(dir = "C:\\Work\\Assess\\ICCAT\\BUM\\SS\\Copy of 6_2sex_v320", fitrange=F)
# 	}
