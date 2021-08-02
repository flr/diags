library(ggplot2)
library(plyr)
library(dplyr)
library(grid)

load("/home/laurence-kell/Desktop/papers/froese/data/ices_trj_lk.rdata")

ts=ddply(ts, .(Stock), transform, production=c(ssb[-1]-ssb[-length(SSB)]+ct[-length(ct)],NA)/Btr)

trend2=function(trks,pts,
                ylab=expression(B/B[MSY]),
                ylim=5,
                quadcol=c("tomato","chartreuse33","chartreuse"),
                layer=NULL){
  
  if (!("run" %in% names(pts)))
    pts=cbind(pts,run=factor(1))
  if (!is.null(trks) & !("run" %in% names(trks)))
    trks=cbind(trks,run=factor(1))
  
  
  ##### Density plots   #############################################################################################
  marginal<-ggplot(pts) + 
    geom_density(aes(x = series, y =  ..count..), fill="white",col="black",position = "stack")+ 
    geom_vline(xintercept=1.2,col="green", data=data.frame(fref=1))+
    geom_vline(xintercept=0.8,col="tomato",data=data.frame(fref=1))+
    coord_cartesian(xlim=c(0,ylim))   +
    scale_x_continuous(limits=c(0,ylim))   +
    scale_fill_manual(values=col)          +
    xlab(xlab) + ylab(ylab)                +
    theme(legend.position = "none", 
          axis.title.x = element_text(colour ='NA'), 
          axis.text.x  = element_text(colour ="NA"), 
          axis.ticks.x = element_line(colour ="NA"),
          axis.ticks =   element_line(colour ="NA"),
          
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(), 
          
          plot.margin = unit(c(1, 0, 0, 0), "lines"),
          panel.background = element_rect(fill   ="NA", colour ="NA"), 
          panel.border     = element_rect(fill   ="NA", colour ="NA"), 
          panel.grid.major = element_line(colour ="NA"), 
          panel.grid.minor = element_line(colour ="NA")                    
    )
  
  series=ggplot(trks)+ 
    geom_ribbon(aes(year,ymin=1.2,ymax=ylim),fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"tomato","chartreuse3"),alpha=0.5)+
    geom_ribbon(aes(year,ymin=0.8,ymax=1.2),fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"yellow","yellow"),alpha=0.5)+
    geom_ribbon(aes(year,ymin=0,ymax=0.8), fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"chartreuse3","tomato"),alpha=0.5)+ 
    geom_line(aes(year,series,colour=run,group=run),data=trks,size=0.25)+
    geom_hline(yintercept=1.2,col="green", data=data.frame(fref=1))+
    geom_hline(yintercept=0.8,col="tomato",data=data.frame(fref=1))+
    theme_bw()+theme(legend.position="bottom")+
    scale_y_continuous(lim=c(0,ylim))+
    theme_bw()+ 
    theme(legend.position="none",
          axis.text.y=element_text(colour="black", angle=90), 
          plot.margin = unit(c(0, 1, 0, 1), "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    xlab("Year")+ylab(ylab)
   
  fnVP=function(marginal,series){
    vplayout <- function(x, y)
      viewport(layout.pos.row = x, layout.pos.col = y)
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(5, 6)))  # 5 by 5 grid
    print(marginal +coord_flip(xlim=c(0,ylim)), vp=vplayout(2:5,6))         # 2nd to the left +opts(legend.position = c(0,1.05)) + opts(legend.text = theme_text(colour = "black")) 
    print(series, vp=vplayout(2:5,1:5))                     # the main x/y plot will instead spread across most of the grid
  }
  
  fnVP(marginal,series)
  
  invisible(list(series,marginal))}
trend2(transmute(ts,series=bbmsy,year=yr,run=Stock),transmute(subset(ts,yr==2017),series=bbmsy,year=yr,run=Stock))
trend2(transmute(ts,series=bbmsy*(1-exp(-ffmsy)),year=yr,run=Stock),transmute(subset(ts,yr==2017),series=bbmsy*(1-exp(-ffmsy)),year=yr,run=Stock),
       ylab=expression(Catch/MSY))


ggplot(ts)+
  geom_line(aes(yr,production,col=Stock))+
  theme(legend.position="none")

var=ddply(ts,.(Stock), with, var(production,na.rm=T)^0.5)
bin=cbind(var,bin=cut(var[,2],breaks=quantile(var[,2],c(0,.25,.5,.75,1))*c(0.9,1,1,1,1.1)))
ts=subset(merge(ts,bin,by="Stock"),!is.na(production))

ggplot(ts)+
  geom_line(aes(yr,production,group=Stock,col=substr(Stock,1,3)))+
  theme(legend.position="bottom")+
  facet_grid(bin~.,scale="free")+
  xlab("Year")+ylab("Production")


ggplot(ts)+
  geom_path(aes(bbmsy,production,group=Stock,col=substr(Stock,1,3)))+
  geom_point(aes(bbmsy,production),data=subset(ddply(ts,.(Stock), subset, yr==max(yr))))+
  geom_point(aes(bbmsy,production),data=subset(ddply(ts,.(Stock), subset, yr==min(yr))),col="red")+
  theme(legend.position="right")+
  facet_wrap(~Stock,scale="free")+
  xlab("Year")+ylab("Production")

ggplot(ts)+
  geom_path(aes(bbmsy,production,group=Stock,col=bin))+
  geom_point(aes(bbmsy,production),data=subset(ddply(ts,.(Stock), subset, yr==max(yr))))+
  geom_point(aes(bbmsy,production),data=subset(ddply(ts,.(Stock), subset, yr==min(yr))),col="red")+
  theme(legend.position="right")+
  facet_wrap(~Stock,scale="free")+
  xlab("Biomass")+ylab("Production")
