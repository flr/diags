\docType{methods}
\name{readCpue}
\alias{readCpue}
\alias{readCpue,character,character-method}
\alias{readCpue,character,missing-method}
\alias{readCpue-method}
\title{readCpue}
\arguments{
  \item{file;}{the name of the file or dir which the data
  are to be read from.}

  \item{method;}{stock assessment method type one of
  \code{c("aspic","adapt","bsp","mfcl","ss","sam","vpa")}}
}
\value{
  a \code{data.frame} with the CPUE series.
}
\description{
  read catch per unit effort data from a variety of stock
  assessment text files.
}
\examples{
\dontrun{
   dirMy="/home/laurie/Desktop/gcode/gbyp-sam/tests/aspic/bet/2010/run3"
   udat=readCpue(,what="")
   head(udat)
   }
}

