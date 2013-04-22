\docType{methods}
\name{writeCpue}
\alias{writeCpue}
\alias{writeCpue,data.frame,character,character-method}
\alias{writeCpue-method}
\title{writeCpue}
\arguments{
  \item{object;}{the name of the file or dir which the data
  are to be read from.}

  \item{file;}{name of text file to save data to.}

  \item{method;}{stock assessment method type one of
  \code{c("bsp","ss","vpa")}}
}
\description{
  Writes catch per unit effort in a data frame to a variety
  of stock assessment text files.
}
\examples{
\dontrun{
   data(diagAspic)
   writeAspic(diagAspic,"aspoc.inp","aspic")
   }
}

