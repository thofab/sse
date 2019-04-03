\name{inspect}
\docType{methods}
\alias{inspect}
\title{Inspection Plot}
\description{
  Creating a plot that allows to inspect the sample size calculation.
}
\usage{inspect(object)}
\arguments{
  \item{object}{An object of class \code{power}.}
}
\examples{
## defining the range of n and theta to be evaluated
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.1),
              n = seq(from = 20, to = 60, by = 2),
              muA = 0,
              muB = 1)

## defining a power-function               
powFun <- function(psi){
  power.t.test(n = n(psi)/2,
               delta = pp(psi, "muA") - pp(psi, "muB"),
               sd = theta(psi)
               )$power
}

## evaluating the power-function for all combinations of n and theta
calc <- powCalc(psi, powFun)

## adding example at theta of 1 and power of 0.9
pow <- powEx(calc, theta = 1, power = 0.9)

## drawing an inspection plot
inspect(pow)
}
\keyword{methods}

