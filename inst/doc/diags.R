### R code from vignette source 'diags.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: prelim
###################################################
stampIt=function(...){
   require(plyr)

   args=list(...)
   
   ldply(args, function(x) { res=packageDescription(x)
                            c(Package=x,
                              Version=packageDescription(x)$Version,
                              Date   =packageDescription(x)$Date)})}

smry=stampIt("diags")


###################################################
### code chunk number 2: prelim
###################################################
options(digits=2)


###################################################
### code chunk number 3: prelim
###################################################
library(ggplot2)


###################################################
### code chunk number 4: intro (eval = FALSE)
###################################################
## qplot(x, ..., data, geom) 


###################################################
### code chunk number 5: fig1
###################################################
dat =data.frame(x=1:10,y=rnorm(10))
plot(dat$x,dat$y)


###################################################
### code chunk number 6: fig2
###################################################
qplot(dat$x,dat$y)


###################################################
### code chunk number 7: diags.Rnw:177-181
###################################################
library(diags)

data(rsdl)
head(rsdl)


###################################################
### code chunk number 8: data (eval = FALSE)
###################################################
## u2box=readCpue("unisex09.c01","2box")
## uSS  =readCpue("myDir","ss")


###################################################
### code chunk number 9: data (eval = FALSE)
###################################################
## u2box=readCpue("unisex09.c01","2box")
## uSS  =readCpue("myDir","ss")


###################################################
### code chunk number 10: data (eval = FALSE)
###################################################
## stdz(  rnorm(10,1,.3))
## minMax(rnorm(10,1,.3))


###################################################
### code chunk number 11: data (eval = FALSE)
###################################################
## rsdl=ddply(rsdl, .(name), transform, stdRsdl=stdz(residual))


###################################################
### code chunk number 12: diags.Rnw:237-244
###################################################
library(gam)
gm  =gam(log(obs)~lo(year)+name,data=rsdl)
rsdl=data.frame(rsdl,gam=predict(gm),gamRsdl=residuals(gm))
scl =coefficients(gm)[3:9]
names(scl)=substr(names(scl),5,nchar(names(scl)))
rsdl=transform(rsdl,scl=scl[as.character(name)])
rsdl[is.na(rsdl$scl),"scl"]=0


###################################################
### code chunk number 13: diags.Rnw:271-277
###################################################
ggplot(rsdl)+ geom_line(aes(year,exp(gam)),col="red")  +
              geom_smooth(aes(year,obs),se=FALSE)      +           
              geom_point(aes(year,obs,col=name))       +
              facet_wrap(~name,ncol=1,scale="free_y")  +
              theme_ms(legend.position="none")         +
              xlab("Year") + ylab("Index")


###################################################
### code chunk number 14: diags.Rnw:287-299
###################################################
uMat=ddply(rsdl,.(name),transform, obs=stdz(obs))
uMat=cast(uMat,year~name,value="obs")
uMat=uMat[apply(uMat,1,function(x) !all(is.na(x))),]

pM=plotmatrix(uMat[,-c(1:2,4)])
pM$layers[[2]]=NULL
mns=ddply(subset(pM$data,!(is.na(x) & !is.na(y))),.(xvar,yvar), function(x) mean(x$y,na.rm=T))
pM+geom_hline(aes(yintercept=V1),data=mns,col="red") +
   geom_smooth(method="lm",fill="blue", alpha=0.1)  + 
   theme(legend.position="bottom")                   +
   xlab("Index")+ylab("Index")                       +
   theme_ms()


###################################################
### code chunk number 15: diags.Rnw:311-317
###################################################
cr=cor(uMat[,-1],use="pairwise.complete.obs")
dimnames(cr)=list(gsub("_"," ",names(uMat)[-1]),gsub("_"," ",names(uMat)[-1]))
cr[is.na(cr)]=0
corrplot(cr,diag=F,order="hclust",addrect=2)  +          
             theme(legend.position="bottom")  +
             theme_ms()


###################################################
### code chunk number 16: diags.Rnw:329-334
###################################################
ggplot(subset(rsdl,name %in% c("Japan LL II","Japan LL III","South Africa BB II")))+
             geom_point(aes(year,exp((gam+gamRsdl)-scl),col=name))+
             geom_smooth(aes(year,exp(gam+gamRsdl-scl),group=name,col=name),se=T,fill="blue", alpha=0.1)+
             theme_ms(legend.position="bottom") +
             xlab("Year") +ylab("Index")


###################################################
### code chunk number 17: diags.Rnw:343-348
###################################################
    ggplot(subset(rsdl,name %in% c("South Africa BB I","South Africa BB II","Uruguay LL"))) +   
            geom_point(aes(year,exp((gam+gamRsdl)-scl),col=name))+
             geom_smooth(aes(year,exp(gam+gamRsdl-scl),group=name,col=name),se=T,fill="blue", alpha=0.1)+
             theme_ms(legend.position="bottom")  +
             xlab("Year") +ylab("Index")


###################################################
### code chunk number 18: diags.Rnw:361-370
###################################################
dat=ddply(rsdl, .(name), with, data.frame(obs=stdz(obs),hat=stdz(hat)))

ggplot(dat) +
          geom_abline(aes(0,1))                         +
          geom_point( aes(obs,hat))                     +
          stat_smooth(aes(obs,hat),method="lm",fill="blue", alpha=0.1)       +
          facet_wrap(~name,ncol=3)                      +
          theme_ms(legend.position="bottom")            +
          xlab("Fitted") + ylab("Observed")


###################################################
### code chunk number 19: diags.Rnw:379-387
###################################################
dat=ddply(rsdl, .(name), transform, residual=stdz(residual,na.rm=T))

ggplot(aes(year,residual),data=dat) +
  geom_hline(aes(yintercept=0))      +
  geom_point()                       +
  stat_smooth(,method="loess",se=T,fill="blue", alpha=0.1)  +
  facet_wrap(~name,scale="free",ncol=2)   +
             theme_ms() 


###################################################
### code chunk number 20: diags.Rnw:396-404
###################################################
ggplot(rsdl)                                              +
  geom_point( aes(residual,residualLag))                  +
  stat_smooth(aes(residual,residualLag),method="lm",se=T,fill="blue", alpha=0.1)      +
  geom_hline(aes(yintercept=0))                           +
  facet_wrap(~name,scale="free",ncol=3)                   +
  xlab(expression(Residual[t])) + 
  ylab(expression(Residual[t+1])) +
  theme_ms(legend.position="bottom")  


###################################################
### code chunk number 21: diags.Rnw:413-419
###################################################
ggplot(rsdl)                                           +
  geom_point( aes(qqx,qqy))                            +
  stat_smooth(aes(qqx,qqHat),method="lm",se=T,fill="blue", alpha=0.1)         +
  facet_wrap(~name)                                    +
  theme_ms(legend.position="bottom")                   +
             theme_ms()


###################################################
### code chunk number 22: diags.Rnw:427-433
###################################################
ggplot(aes(hat, residual),data=rsdl)   +
  geom_hline(aes(yintercept=0))         +
  geom_point()                          +
  stat_smooth(method="loess",span=.9,fill="blue", alpha=0.1)   +
  facet_wrap(~name,scale="free",ncol=3) +
             theme_ms()


###################################################
### code chunk number 23: diags.Rnw:458-461
###################################################
data(ce)
uStd=glm(log(u)~factor(yr)+factor(flt)+factor(mo),na.action=na.omit,data=transform(ce,u=catch/effort))
diags(uStd)


