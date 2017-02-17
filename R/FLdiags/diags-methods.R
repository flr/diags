utils::globalVariables(c("model.frame"))
utils::globalVariables(c("FLQuants"))
utils::globalVariables(c("ssb"))
utils::globalVariables(c("rec"))
utils::globalVariables(c("predict"))
utils::globalVariables(c("residuals"))
utils::globalVariables(c("ldply"))

#' diags
#' @description 
#' Reads catch per unit effort (CPUE) diagnostics from stock assessment files.
#'       
#' @aliases 
#' diags-method
#' diags,character,character-method

#' @param object name of file or dir where the data are read from. 
#' @param method stock assessment method type one of \code{c("2box","aspic","adapt","bsp","mfcl","ss","sam","vpa")}
#' @param ... any other parameters
#' @return a \code{data.frame} with observed and fitted values by CPUE series.
#' @docType methods
#' @rdname diags
#' 
#' @examples
#' \dontrun{
#'   ## read in data from an ASPIC results file
#'   diagAsp=diags("aspic.prn","aspic")
#'   
#'   ## example data set
#'   data(diagAspic)
#'   head(diagAspic)
#'   }
setGeneric('diags',   function(object,method,...) standardGeneric('diags'))

setMethod('diags',  signature(object='character',method="character"), function(object,method=c("aspic","adapt","bsp","mfcl","ss","sam"),...) {
  
  method=tolower(method)
  if (any("2box" == method)) method["2box" == method]="adapt"   
  switch(substr(method[1],1,2),
         ad=diagsVpa2box( object,...),
         as=diagUaspic(object,...),
         bs=diagUbsp(  object,...),
         mf=diagUmfcl( object,...),
         ss=diagUss(   object,...),
         sa=diagUsam(  object,...))
})

setGeneric('diagUsam',    function(x,...) standardGeneric('diagUsam'))
setGeneric('diagUss',     function(x,...) standardGeneric('diagUss'))
setGeneric('diagUmfcl',   function(x,...) standardGeneric('diagUmfcl'))
setGeneric('diagUbsp',    function(x,...) standardGeneric('diagUbsp'))
setGeneric('diagUaspic',  function(x,...) standardGeneric('diagUaspic'))


setMethod('diagUsam',   signature(x='character'), function(x,...) .diagUsam(  x,...))
setMethod('diagUss',    signature(x='character'), function(x,...) .diagUss(   x,...))
setMethod('diagUmfcl',  signature(x='character'), function(x,...) .diagUmfcl( x,...))
setMethod('diagUbsp',   signature(x='character'), function(x,...) .diagUbsp(  x,...))
setMethod('diagUaspic', signature(x='character'), function(x,...) .diagUaspic(x,...))
