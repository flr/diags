\docType{methods}
\name{diags}
\alias{diags}
\alias{diags,character,character-method}
\alias{diags-method}
\title{diags}
\arguments{
  \item{file;}{name of file or dir where the data are read
  from.}

  \item{method;}{stock assessment method type one of
  \code{c("aspic","adapt","bsp","mfcl","ss","sam","vpa")}}
}
\value{
  a \code{data.frame} with observed and fitted values by
  CPUE series.
}
\description{
  Reads catch per unit effort (CPUE) diagnostics from stock
  assessment files.
}
\examples{
\dontrun{
  ## read in data from an ASPIC results file
  diagAsp=diags("aspic.prn","aspic")

  ## example data set
  data(diagAspic)
  head(diagAspic)
  }
}

