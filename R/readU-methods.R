#' readCpue
#' @description 
#' read catch per unit effort data from a variety of stock assessment text files.
#'       
#' @aliases 
#' readCpue-method
#' readCpue,character,character-method
#' readCpue,character,missing-method
#' 
#' @param file; the name of the file or dir which the data are to be read from. 
#' @param method; stock assessment method type one of \code{c("aspic","adapt","bsp","mfcl","ss","sam","vpa")}
#' @return a \code{data.frame} with the CPUE series.
#' @export
#' @docType methods
#' @rdname readCpue
#' 
#' @examples
#' \dontrun{
#'    dirMy="/home/laurie/Desktop/gcode/gbyp-sam/tests/aspic/bet/2010/run3"
#'    udat=readCpue(,what="")   
#'    head(udat)
#'    }
setGeneric('readCpue',   function(file,method,...) standardGeneric('readCpue'))

setMethod('readCpue',  signature(file='character',method="missing"), function(file,method=method,...) {
  
  ln=tolower(scan(file,nlines=1,what=character(),sep="\n"))
  
  if (length(grep("multifan",ln))>0 | length(grep("mfcl",ln))>0)
    method="mfcl"
  if (length(grep("bot",ln))>0)
    method="aspic"
  
  switch(substr(method[1],1,2),
         ad=cpue2box( file,...),
         as=cpueAspic(file,...),
         bs=cpueBSP(  file,...),
         mf=cpueMFCL( file,...),
         ss=cpueSS(   file,...),
         vp=cpueVpa(  file,...))
})

setMethod('readCpue',  signature(file='character',method="character"), function(file,method=c("aspic","adapt","bsp","mfcl","ss","sam","vpa"),...) {
      
    type=tolower(method)
    if (any("2box" == method)) method["2box" == method]="adapt"   
    switch(substr(method[1],1,2),
           ad=cpue2box( file,...),
           as=cpueAspic(file,...),
           bs=cpueBSP(  file,...),
           mf=cpueMFCL( file,...),
           ss=cpueSS(   file,...),
           vp=cpueVpa(  file,...))
    })

setGeneric('cpue2box',   function(x,...) standardGeneric('cpue2box'))
setGeneric('cpueVpa',    function(x,...) standardGeneric('cpueVpa'))
setGeneric('cpueSS',     function(x,...) standardGeneric('cpueSS'))
setGeneric('cpueMFCL',   function(x,...) standardGeneric('cpueMFCL'))
setGeneric('cpueBSP',    function(x,...) standardGeneric('cpueBSP'))
setGeneric('cpueAspic',  function(x,...) standardGeneric('cpueAspic'))
 
setMethod('cpue2box',  signature(x='character'), function(x,...) iU2box(    x,...))
setMethod('cpueVpa',   signature(x='character'), function(x,...) iUVPASuite(x,...))
setMethod('cpueSS',    signature(x='character'), function(x,...) iUSS(      x,...))
setMethod('cpueMFCL',  signature(x='character'), function(x,...) iUMFCL(    x,...))
setMethod('cpueBSP',   signature(x='character'), function(x,...) iUBSP(     x,...))
setMethod('cpueAspic', signature(x='character'), function(x,...) iUAspic(   x,...))