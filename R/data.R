#' ce
#' 
#' An example dataset with catch and effort by year, month, week and fleet based on North 
#' Atlantic Albacore, derived from the ICCAT multifan-CL assessment 
#'
#' @format An \code{data.frame} object
#' \describe{
#'   \item{yr}{year, \code{numeric}}
#'   \item{mo}{month, \code{numeric}}
#'   \item{wk}{week, \code{numeric}}
#'   \item{flt}{fleet, \code{numeric}}
#'   \item{catch}{catch, \code{numeric}}
#'   \item{effort}{effort, \code{numeric}}
#'   ...
#' }
#' @name ce
#' @docType data
#' @keywords data
NULL

#' dgs
#' 
#' An example dataset with catch and effort by year, month, week and fleet based on North 
#' Atlantic Albacore, derived from the ICCAT multifan-CL assessment 
#'
#' @format An \code{data.frame} object
#' \describe{
#'   \item{name}{The abundance series, \code{character}}        
#'   \item{year}{Year, \code{numeric}}
#'   \item{x}{The assessment estimate of quantity the the index is tracking, log transformed\code{numeric}}
#'   \item{y}{The index value, log transformed\code{numeric}}
#'   \item{yhat}{The predicted index value from the regression of y on x, \code{numeric}}        
#'   \item{residual}{Residual from the regression of y on x, \code{numeric}}    
#'   \item{sd}{Standard deviation, \code{numeric}}         
#'   \item{q}{catchability, \code{numeric}}           
#'   \item{x2}{The assessment estimate of quantity the the index is tracking, untransformed\code{numeric}}
#'   \item{y2}{The index value, untransformed\code{numeric}}
#'   \item{chi2}{The chi-square discrepancy computed for each point to identifying possible outliers, \code{numeric}}        
#'   \item{residualLag}{Residual shifted by one time step (t-1), \code{numeric}} 
#'   \item{qqx}{The theoretical sample quantile, \code{numeric}}         
#'   \item{qqy}{The actual sample quantile, \code{numeric}}        
#'   \item{qqHat}{Allows a a line to be plotted for comparison, \code{numeric}}
#'   ...
#' }
#' @name dgs
#' @docType data
#' @keywords data
NULL
