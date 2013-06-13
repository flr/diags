#' fget
#' 
#' @description 
#' Creates a path to a file in a package subdirectory
#'      
#' @param file the name of the file with subdir in a pacakge to reference
#' @param package directs you to where you have installed the package 
#' @return \code{char} with path and file 
#' @export
#' @docType functions
#' @rdname fget
#' 
#' @examples
#' x=1
fget=function(file,package="diags")        
    paste(system.file(package=package, mustWork=TRUE), paste(file, sep=""), sep="/")