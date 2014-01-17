\docType{methods}
\name{mfclVcov}
\alias{mfclVcov}
\title{mfclVcov}
\usage{
mfclVcov(fileVar, fileCor)
}
\arguments{
  \item{fileVar;}{the name of the file.}

  \item{fileCor;}{the name of the file or dir which the
  data are to be read from.}
}
\value{
a \code{list} with "hat","cor" and "cov".
}
\description{
reads Variance Covariance matrix from MFCL output files
}
\examples{
\dontrun{
   mfclVcov(...)
   }
}

