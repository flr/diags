\name{stdz}
\alias{stdz}
\title{stdz}
\usage{
  stdz(x, na.rm = TRUE)
}
\arguments{
  \item{x}{an numeric vector}
  \item{na.rm}{logical. Should missing values be removed}
}
\description{
  Transforms a vector so that its mean is 0 and its
  standard deviation is 1
}
\details{
  Useful for allowing time series with different
  variability and ranges to be compared when plotting
}
\examples{
x=rnorm(100,1,.3)
sd(stdz(x))
mean(stdz(x))
}
\seealso{
  \code{\link{minMax}}
}

