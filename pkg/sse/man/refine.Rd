\name{refine}
\docType{methods}
\alias{refine}
\title{Refining the estimation}
\description{
  Increasing the number of iterations for estimating the sample size for
  the theta and xi as specified for the example.
}
\usage{refine(object, n.iter = 10)}
\arguments{
  \item{object}{An object of class \code{power}.}
  \item{n.iter}{An integer larger than one that is multiplied with the
    available number of iterations the target number of iterations.}
}
\note{This function is only useful if the object of class \code{power}
  was generated using a resamling approach.}
\examples{
## defining the range of n and theta to be evaluated
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.1),
              n = seq(from = 20, to = 60, by = 2))

## defining a power-function               
powFun <- function(psi){
  x <- rnorm(n(psi)/2)
  y <- rnorm(n(psi)/2) + theta(psi)
  return(wilcox.test(x = x, y = y)$p.value < 0.05)
}


## evaluating the power-function for all combinations of n and theta
calc <- powCalc(psi, powFun, n.iter = 100)

## adding example at theta of 1 and power of 0.9
pow <- powEx(calc, theta = 1, power = 0.9)

## another 900 (= 1000 - 100) iterations
refine(pow)


}
\keyword{methods}

