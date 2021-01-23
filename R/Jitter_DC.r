if (FALSE){
##
# Jitter Test for Global Convergence with Stock Synthesis 
# Date: Oct 10, 2019
# DC
##
# Adapted from 
# jitter.R 
# Massimiliano Cardinale (July 31, 2019) 
# and 
# Michael Schirripa's (Downloaded from G drive Oct 2, 2019) 
# Example implementation for ICCAT white marlin (WHM) 
##
##
# Stock Synthesis (tested in version 3_30_X for Windows) 
# r4ss (tested in version(s) 1.35.1 - 1.35.3)  
# R (tested in version(s) 3.3.2 - 3.4.4 64 bit Windows)
##
# Option 1 - implemented
# run with parameter values obtained from a completed model run using the file ss.par 
#
# Option 2 - not implemented
# run with parameter values obtained from a initial values for the completed model run using the control file  
# This may have no effect if using the control.ss_new file produced by stock synthesis (if intitial values are automatically replaced by their estimates in control.ss_new) 
##

library(r4ss)
#devtools::install_github("r4ss/r4ss", ref="development")
?r4ss

#-------------------Housekeeping (DC)------------------------------------
#### Change starter file appropriately (can also edit file directly)
#mydir <- "C:\\Users\\massi\\OneDrive\\Dokument\\Max files for backup\\Documents\\Commitees\\ICES\\WKBENCH\\WKBENCH 2020\\Witch flounder\\Jittering"
#mydir <- "C:\\Simple"
#jitter.dir <- "D:\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\Jitter"
#base.dir   <- "D:\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\base"
#plot.dir   <- "D:\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\plots"

# Define the root directory for the run
#DC desktop
dirname.root <- "D:\\1005_Diagnostics_pub\\05_Jitter\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04"
#Dc travel laptop
#dirname.root <- "C:\\000\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04"
dirname.root

# Define the directory where a completed "base" model run is located
dirname.base <- paste0(dirname.root,'/base')
dirname.base

# Create a subdirectory for the jitter run
dirname.jitter <- paste0(dirname.root,'/jitter')
dirname.jitter
dir.create(path=dirname.jitter, showWarnings = TRUE, recursive = TRUE)

# Create a subdirectory for the output
dirname.plots <- paste0(dirname.root,"/plots")
dirname.plots
dir.create(dirname.plots)

#----------------- copy base model files to jitter directory (DC) ----------------------------------------
# Option 1
file.copy(paste(dirname.base,       "starter.ss", sep="/"),
          paste(dirname.jitter,     "starter.ss", sep="/"))
file.copy(paste(dirname.base,       "_whm.ctl", sep="/"),
          paste(dirname.jitter,     "_whm.ctl", sep="/"))
file.copy(paste(dirname.base,       "_whm.dat", sep="/"),
          paste(dirname.jitter,     "_whm.dat", sep="/"))	
file.copy(paste(dirname.base,       "forecast.ss", sep="/"),
          paste(dirname.jitter,     "forecast.ss", sep="/"))
file.copy(paste(dirname.base,       "ss.exe", sep="/"),
          paste(dirname.jitter,     "ss.exe", sep="/"))
file.copy(paste(dirname.base,       "ss.par", sep="/"),
          paste(dirname.jitter,     "ss.par", sep="/"))


#Make Changes to the Starter.ss file (r4ss example)
starter <- SS_readstarter(file.path(dirname.jitter, 'starter.ss'))

# Change to use .par file
starter$init_values_src = 1

# Change jitter (0.1 is an arbitrary, but common choice for jitter amount)
starter$jitter_fraction = 0.1

# write modified starter file
SS_writestarter(starter, dir=jitter.dir, overwrite=TRUE)

#Begin Option 2
#file.copy(paste(dirname.base,       "starter.ss", sep="/"),
#          paste(dirname.jitter,     "starter.ss", sep="/"))
#file.copy(paste(dirname.base,       "control.ss_new", sep="/"),
#          paste(dirname.jitter,     "control.ss_new", sep="/"))
#file.copy(paste(dirname.base,       "_whm.dat", sep="/"),
#          paste(dirname.jitter,     "_whm.dat", sep="/"))	
#file.copy(paste(dirname.base,       "forecast.ss", sep="/"),
#          paste(dirname.jitter,     "forecast.ss", sep="/"))
#file.copy(paste(dirname.base,       "ss.exe", sep="/"),
#          paste(dirname.jitter,     "ss.exe", sep="/"))
# Make Changes to the Starter.ss file (r4ss example)
#starter <- SS_readstarter(file.path(dirname.jitter, 'starter.ss'))
#starter$jitter_fraction = 0.1
#S_writestarter(starter, dir=jitter.dir, overwrite=TRUE)
#end Option 2

#------------ Run Jitter Test for Global Convergence with Stock Synthesis (MS MC) -------------------------------
#Set the number of iteration  
Njitter=200
#Njitter=1 #test

#### Run jitter using this function (deafult is nohess)
jit.likes <- SS_RunJitter(mydir=dirname.jitter, Njitter=Njitter, extras="")

setwd(dirname.plots)
getwd()

# DC Notes Save image of "jit.likes" for later analysis
#file.name<-paste('jit.likes',format(Sys.time(), "%Y%m%d_%H%M"))
#save.image(paste0(dirname.plots, "/",file.name, ".RData"))
# DC desktop
#load("D:\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\plots\\jit_likes 20191010_1531.RData")
# DC laptop
#load("C:\\000\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\plots\\jit_likes 20191010_1531.RData")

#DC Notes - total likelihoods necessary to assess global convergence are saved to "jit.likes"
x<-as.numeric(jit.likes)
global.convergence.check<-table(x,exclude = NULL)
write.table(jit.likes,"jit_like.csv")
write.table(global.convergence.check,"global_convergence_check.csv")


#------------ Summarize more Jitter Test Results (MS MC) -------------------------------
#DC Notes - Save image for later analysis
#wd <- ("C:\\Users\\massi\\OneDrive\\Dokument\\Max files for backup\\Documents\\Commitees\\ICES\\WKBENCH\\WKBENCH 2020\\Witch flounder\\Jittering")
#wd <- ("C:\\Simple")
wd <- dirname.jitter

jitter=seq(1:Njitter)
n=length(jitter)
n
witch_j <- SSgetoutput(keyvec=1:n, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary <- SSsummarize(witch_j)

#Likelihood across runs
likes=witch_j_summary$likelihoods

#Derived quants across runs
quants=witch_j_summary$quants

#Estimated parameters across runs
pars=witch_j_summary$pars

#Write more output tables to jitter directory
write.table(quants,"Quants.csv")
write.table(pars,"Pars.csv")
write.table(likes,"Likelihoods.csv")

#DC Notes - retabulate total likelihoods necessary to assess global convergence and compare to jit.likes from above 
x<-as.numeric(likes[likes$Label=="TOTAL",1:n])
global.convergence<-table(x,exclude = NULL)
write.table(global.convergence,"global_convergence.csv")


#------------ Make plots with r4ss for runs ending at a converged solution (Dc) -------------------------------
# DC notes - base case read in manually
# If the base run is needed for comparison, the jitter function also mentions something about including the base as rep0?
#Base <- SS_output(dir="C:\\Simple",covar=T) 
Base <- SS_output(dir=dirname.base,covar=T,forecast=T)

#make some plots#make some plots
plotdir <- dirname.plots
setwd(plotdir)
getwd()

png("Jittering results.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(1:Njitter), witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter],ylab="LL",
     ylim=c(0,max(na.omit(jit.likes))*1.05)) ; mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F, plotdir = plotdir, ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()

png("jit likes.png", width = 480, height = 480)
par(mfrow=c(1,1), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(1:Njitter), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter model runs at a converged solution"
)
#mtext(side=3, line=0, "Jittering")     
abline(h=Base$likelihoods_used[1,1], col=2)
dev.off()


# DC Notes Repeat for all converged runs 
# probably an easier way to do this, but I could not figure it out
x<-which(!is.na(witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter]))

jitter.converged=x
jitter.converged
n.converged=length(jitter.converged)
n.converged
witch_j.converged <- SSgetoutput(keyvec=jitter.converged, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary.converged <- SSsummarize(witch_j.converged)

png("Jittering results at converged solution.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(jitter.converged), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL", jitter.converged],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter runs at a converged solution"
)
mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary.converged,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F, plotdir = plotdir, ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()


# DC Notes Repeat for converged runs at the minimum solution 
# Converged runs at min converged solution (should be same as base case to pass the test) 
#min(na.omit(jit.likes))
y<-which(witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL",1:Njitter]==min(na.omit(jit.likes)))


jitter.min=y
jitter.min
n.min=length(jitter.min)
n.min
witch_j.min <- SSgetoutput(keyvec=jitter.min, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary.min <- SSsummarize(witch_j.min)


png("Jittering results at min converged solution.png", width = 480, height = 480)
par(mfrow=c(2,2), mai=c(.6,.6,.3,.2), mex=.5)
plot(seq(jitter.min), 
     witch_j_summary$likelihoods[witch_j_summary$likelihoods$Label=="TOTAL", jitter.min],
     ylab="Total likelihood",
     ylim=c(0,max(na.omit(jit.likes))*1.05),
     xlab="Jitter runs at the minimum converged solution"
     )
mtext(side=3, line=0, "Jittering")
abline(h=Base$likelihoods_used[1,1], col=2)

SSplotComparisons(witch_j_summary.min,     subplots =  c(2,8,18) , pch = "",legend=FALSE  ,lwd = 1 ,new = F, plotdir = plotdir, ylimAdj=1)
mtext(outer=T, side=3, line=-2.5, "Jitter results")
dev.off()

# DC Notes Save image of all run data for later analysis
#file.name<-paste('jitter',format(Sys.time(), "%Y%m%d_%H%M"))
#save.image(paste0(dirname.plots, "/",file.name, ".RData"))
#DC desktop
load("D:\\1005_Diagnostics_pub\\05_Jitter\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\plots\\jitter 20191010_1959.RData")
#DC laptop
#load("C:\\000\\1005_Diagnostics_pub\\05_Jitter_03_WHM\\01_WHM_DC\\01_DC_try_04\\plots\\jitter 20191010_1959.RData")

#DC Notes
# Stock Synthesis user's manual (v 3.30.14): 
# Methot Jr., R. D., Wetzel, C. R., and I. G. Taylor. 2019. User manual for Stock Synthesis model version 3.30.14, July 16, 2019. NOAA Fisheries, Seattle, WA. Available: https://vlab.ncep.noaa.gov/web/stock-synthesis/home (Accessed October 11, 2019).
#p 19 "To evaluate the jittering, the bounds, and the original initial values, a jitter_info table is available from
#r4ss, including sigma, CV and InitLocation columns (the latter referring to location within the
#cumulative normal - too close to 0 or 1 indicates a potential issue)."
#Google "jitter_info r4ss"
#https://github.com/r4ss/r4ss/blob/master/R/SS_output.R
#jitter_info <- parameters[!is.na(parameters$Active_Cnt) &
#                            !is.na(parameters$Min),
#                          c("Value","Min","Max","Init")]
#jitter_info$sigma <- (jitter_info$Max - jitter_info$Min)/(2*qnorm(.999))
#jitter_info$CV <- jitter_info$sigma/jitter_info$Init
#jitter_info$InitLocation <- pnorm(q = jitter_info$Init,
#                                  mean = (jitter_info$Max + jitter_info$Min)/2,
#                                  sd = jitter_info$sigma)


# example of jitter_infor converged
witch_j.check <- SSgetoutput(keyvec=1, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary.check <- SSsummarize(witch_j.check)
str(witch_j_summary.check)

str(witch_j.check)
names(witch_j.check)
which(names(witch_j.check$replist1)=="jitter_info")
witch_j.check$replist1$jitter_info

#Pars
witch_j_summary.check$parsSD
witch_j_summary.check$pars
#estimated par Labels
na.omit(witch_j_summary.check$parsSD)
na.omit(witch_j_summary.check$parsSD)$Label

#estimated pars
jitter.check.converged <- witch_j.check$replist1$jitter_info[na.omit(witch_j_summary.check$parsSD)$Label,]

# Example of jitter_info not converged
witch_j.check.n.conv <- SSgetoutput(keyvec=2, getcomp=FALSE, dirvec=wd, getcovar=F)
witch_j_summary.check.n.conv <- SSsummarize(witch_j.check.n.conv)

str(witch_j.check.n.conv)
names(witch_j.check.n.conv)
which(names(witch_j.check.n.conv$replist2)=="jitter_info")
witch_j.check.n.conv$replist2$jitter_info

#Pars
witch_j_summary.check.n.conv$parsSD
witch_j_summary.check.n.conv$pars
#estimated par Labels
na.omit(witch_j_summary.check.n.conv$parsSD)
na.omit(witch_j_summary.check.n.conv$parsSD)$Label


#Jitter results for estimated pars
jitter.check.n.converged <- witch_j.check.n.conv$replist2$jitter_info[na.omit(witch_j_summary.check.n.conv$parsSD)$Label,]

# Compare a converged run [1] with a run [2] that did not converge
# Stock Synthesis user's manual (v 3.30.14): 
#p 19
#InitLocation column ( referring to location within the cumulative normal
#- too close to 0 or 1 indicates a potential issue).
jitter.check.converged
jitter.check.n.converged

#Write more output tables to jitter directory
write.table(jitter.check.converged,"jitter_check_converged.csv")
write.table(jitter.check.n.converged,"jitter_check_not_converged.csv")

}