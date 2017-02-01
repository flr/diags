utils::globalVariables(c("qqnorm"))
utils::globalVariables(c("quantile"))
utils::globalVariables(c("str_trim"))

utils::globalVariables(c("read.table"))
utils::globalVariables(c("melt"))
utils::globalVariables(c("ddply"))
utils::globalVariables(c("."))

utils::globalVariables(c("start"))
utils::globalVariables(c("end"))
utils::globalVariables(c("melt"))
utils::globalVariables(c("broomify"))
utils::globalVariables(c("y"))


#' @title diags.vpa2box
#' 
#' @description 
#' Reads in the fits to the indices of abundance from VPA2Box
#' 
#' @param file the results file
#' @param ... any other arguments
#' 
#' @return an \code{data.frame} object
#' @aliases diags.vpa2box-method diags.vpa2box,character-method
#' 
#' @export
#' @rdname diags.vpa2box
#' 
#' @examples
#' \dontrun{
#' diags.vpa2box(file)
#' } 
#' 
setGeneric('diags.vpa2box',   function(file,...) standardGeneric('diags.vpa2box'))
setMethod('diags.vpa2box',    signature(file='character'), function(file,...){

    tab5<-scan(file,what="",sep="\n")
    tab5<-tab5[grep("TABLE 5.",tab5):length(tab5)]

    pos  <-grep("Chi-sq. discrepancy=",tab5)
    nms  <-substr(tab5[pos-7],9,30)
    str  <-pos+5
    end  <-grep("Selectivities",tab5)-2

    fn<-function(uDiag) {
        uDiag<-unlist(strsplit(uDiag," "))
        as.numeric(uDiag[nchar(uDiag)>0])}

    uDiag<-tab5[unlist(mapply(seq,str,end))]
    dim1 <-length(uDiag)
    uDiag<-fn(uDiag)
    dim2 <-length(uDiag)
    nVar<-dim2/dim1
    uDiag<-as.data.frame(t(array(uDiag,c(nVar,length(uDiag)/nVar))))
    uDiag<-data.frame(name=unlist(mapply(rep, nms,(end-str)+1)),uDiag,row.names=NULL)

    #uDiag$name<-factor(uDiag$name)
    if (nVar==9) {
      names(uDiag)<-c("name","year","x","y","residual","sd","q","x2","y2","chi2")
      uDiag=transform(uDiag,yhat=y+residual)
      uDiag<-uDiag[,c("name","year","x","y","yhat","residual","sd","q","x2","y2","chi2")]
      }
    if (nVar==7) {
      names(uDiag)<-c("name","year","x","y","residual","sd","q","chi2")
      uDiag=transform(uDiag,yhat=y+residual)
      uDiag<-uDiag[,c("name","year","x","y","yhat","residual","sd","q","chi2")]
      }
    
    uDiag$residualLag<-c(uDiag$residual[-1],NA)
    uDiag[!duplicated(uDiag[,"name"]),"residualLag"]<-NA

    fnQQ<-function(object){
       qq.          =qqnorm(c(object$residual),plot.it=FALSE)
       qqx          =qq.$x
       qqy          =qq.$y

       res<-data.frame(qqx=qqx,qqy=qqy)

       return(res)}

    uDiag<-data.frame(uDiag,fnQQ(uDiag)[,c("qqx","qqy")])

    qqLine<-function(obj){
           x<-obj$qqx
           y<-obj$qqy

           qtlx<-quantile(x,prob=c(0.25,0.75),na.rm=T)
           qtly<-quantile(y,prob=c(0.25,0.75),na.rm=T)

           a=(qtly[1]-qtly[2])/(qtlx[1]-qtlx[2])
           b=qtly[1]-qtlx[1]*a

           res<-c(a,b)
           names(res)<-NULL
           names(res)<-c("a","b")

           return(res)
           return(data.frame(y=x*res["a"]+res["b"]))}

    par<-qqLine(uDiag)
    uDiag$qqHat<-uDiag$qqx*par["a"]+par["b"]
    
    uDiag$name=str_trim(uDiag$name)
    
    return(uDiag)})
  
getPos<-function(file){
    lns   <-scan(file,what="",sep="\n")

    n     <-length(lns)
    minus1<-(1:n)[substr(lns,1,2)=="-1"]
    hash  <-(1:n)[substr(lns,1,1)=="#"]
    start <-c(hash[(hash[-1]-hash[-length(hash)])>1],hash[length(hash)])+1
    hash  <-rev(hash)
    end   <-c(rev(hash[hash[-length(hash)]-hash[-1]>1]-1),n)
    end[end %in% minus1]<-end[end %in% minus1]-1
    
    return(data.frame(start=start,end=end))}

utils::globalVariables(c("indexPos"))

getIdx<-function(x){
  posRng  <-1
  posSpc  <-5
  posIdx  <-6
  posVul  <-7
  posWaa  <-8
  
  # range
  t.        <-as.numeric(scan(x,skip=start[posRng]-1,nlines=end[posRng]-start[posRng]+1,what=character()))
  rng       <-t.[!is.na(t.)]
  names(rng)<-c("minyear","maxyear","min","max","plusgroup","pgproj")[1:length(rng)]
  
  # Specifications
  t.        <-as.numeric(scan(x,skip=start[posSpc]-1,nlines=end[posSpc]-start[posSpc]+1,what=character()))
  specs     <-t.[!is.na(t.)]
  specs     <-as.data.frame(t(array(specs,c(7,length(specs)/7))))
  names(specs)<-c("index","pdf","units","vul","timing","min","max")

  # index
  idx       <-as.numeric(scan(x,skip=start[indexPos]-1,nlines=end[indexPos]-start[indexPos]+1,what=character()))
  idx       <-idx[!is.na(idx)]
  n         <-length(idx)
  idx       <-as.data.frame(t(array(idx,dim=c(4,n/4))))
  names(idx)<-c("index","year","data","cv")

  # Vulnerabilities
  vul       <-as.numeric(scan(x,skip=start[posVul]-1,nlines=end[posVul]-start[posVul]+1,what=character()))
  vul       <-vul[!is.na(vul)]
  n         <-length(vul)
  nvar      <-diff(rng[c("min","max")])+3
  vul       <-as.data.frame(t(array(vul,dim=c(nvar,n/nvar))))  
  names(vul)[1:2]<-c("index","year")
  names(vul)[1:2]<-c("index","year")
  names(vul)[-(1:2)]<-seq(rng[c("min","max")])
  vul<-melt(vul,id=c("index","year"),variable="age")
  names(vul)[4]<-"data"

  # catch weight
  waa       <-as.numeric(scan(x,skip=start[posWaa]-1,nlines=end[posWaa]-start[posWaa]+1,what=character()))
  waa       <-waa[!is.na(waa)]
  n         <-length(waa)
  nvar      <-diff(rng[c("min","max")])+3
  waa       <-as.data.frame(t(array(waa,dim=c(nvar,n/nvar))))
  
  names(waa)[1:2]<-c("index","year")
  names(waa)[-(1:2)]<-seq(rng[c("min","max")])
  waa<-melt(waa,id=c("index","year"),variable="age")
  names(waa)[4]<-"data"

  return(list(range=rng,specs=specs,index=idx,vul=vul,waa=waa))} 

if (FALSE){
  #http://ggobi.github.io/ggally/#ggally
    
  library(diags)
  library(plyr)
  library(dplyr)
  library(GGally)
  library(stringr)
  dirVPA="/home/laurie/Desktop/kobe/inputs/bfte/2014/vpa/reported/med"
  rsdl=diags:::diags.vpa2box(file.path(dirVPA,"MINUS0.R"))
  
  rsdl=cbind(rsdl[,-(3:4)],(ddply(rsdl,.(name),with, broomify(lm(y~x-1)))))
  rsdl=transform(rsdl,yhat=y+residual)
  
  lm_with_cor <- function(data, mapping, ..., method = "pearson") {
    x <- data[[deparse(mapping$x)]]
    y <- data[[deparse(mapping$y)]]
    cor <- cor(x, y, method = method)
    ggally_smooth_lm(data, mapping, ...) +
      ggplot2::geom_label(
        data = data.frame(
          x = min(x, na.rm = TRUE),
          y = max(y, na.rm = TRUE),
          lab = round(cor, digits = 3)
        ),
        mapping = ggplot2::aes(x = x, y = y, label = lab, color = NULL),
        hjust = 0, vjust = 1,
        size = 5, fontface = "bold"
      )
  }
  
  ggduo(rsdl,c("year","y"),c("residual",".fitted"),
        mapping=aes(color=name),
  types = list(continuous = wrap(lm_with_cor, alpha = 0.25)),
  showStrips = FALSE,
  title = "Between Academic and Psychological Variable Correlation",
  xlab = "",
  ylab = "",
  #legend = c(5,2)
  ) +
  theme(legend.position = "bottom")
  
  cast(rsdl,name~+.sigma,.resid)

  rsdl=transform(rsdl,yhat=y-residual)
  c(".resid",".sigma",".hat",".cockssd")
  
  ggplot(melt(rsdl[,c("name","year","residual",".hat",".cooksd",".sigma")],id=c("name","year")))+
    geom_point(aes(year,value))+
    facet_grid(variable~name,scale="free",space="free")

}