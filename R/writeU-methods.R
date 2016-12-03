setGeneric('writeCpue',   function(object,file,method,...) standardGeneric('writeCpue'))

utils::globalVariables(c(".writeUbsp",".writeUSS",".writeUVPASuite"))

#' writeCpue
#' @description 
#' Writes catch per unit effort in a data frame to a variety of stock assessment text files.
#'       
#' @aliases 
#' writeCpue-method
#' writeCpue,data.frame,character,character-method
#' 
#' @param object; the name of the file or dir which the data are to be read from. 
#' @param file; name of text file to save data to.
#' @param method; stock assessment method type one of \code{c("bsp","ss","vpa")}
#' 
#' @export
#' @docType methods
#' @rdname writeCpue
#' 
#' @examples
#' \dontrun{
#'    data(diagAspic)
#'    writeAspic(diagAspic,"aspoc.inp","aspic")
#'    }
setMethod('writeCpue',  signature(object="data.frame",file='character',method="character"), function(object,file,method=c("bsp","ss","vpa"),...) {
  
  method=tolower(method)
  if (any("2box" == method)) method["2box" == method]="adapt"   
  switch(substr(method[1],1,2),
         bs=writeUbsp(  object,file,...),
         ss=writeUss(   object,file,...),
         vp=writeUvpa(  object,file,...))})

setGeneric('writeUvpa',    function(object,file,...) standardGeneric('writeUvpa'))
setGeneric('writeUss',     function(object,file,...) standardGeneric('writeUss'))
setGeneric('writeUbsp',    function(object,file,...) standardGeneric('writeUbsp'))

setMethod('writeUvpa',   signature(object="data.frame",file='character'), function(object,file,...) .writeUVPASuite(object,file,...))
setMethod('writeUss',    signature(object="data.frame",file='character'), function(object,file,...) .writeUSS(      object,file,...))
setMethod('writeUbsp',   signature(object="data.frame",file='character'), function(object,file,...) .writeUbsp(     object,file,...))
