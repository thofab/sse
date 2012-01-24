\name{pp}
\alias{pp}
\title{
Extracting all elements of an object of class powPar by their names
except theta and n.
}
\description{
  All information needed for the power-function should be provided by
  an object of class \code{powPar}.
  To extract this information the \code{pp} should be used.
}
\usage{pp(object, name)}
\arguments{
  \item{object}{An object of class \code{powPar}.}
  \item{name}{A character indicating the name of the object to be extracted.}
  }
\details{none
}
\value{Everything that can be stored within a list is possible.}
\note{The name \code{pp} is an abbreviation for power parameter}
\seealso{For extracting individual elements of \code{n}, \code{theta}
  and \code{xi} the functions \code{\link{n}}, \code{\link{theta}},
  \code{\link{theta}} should be used.}
\examples{
psi <- powPar(muA = 0,
              muB = 1,
              sA = 1,
              sB = round(seq(from = 0.5, to = 1.5, by = 0.05), 2),
              n = round(seq(from = 10, to = 30, by = 2),0),
              theta.name = "sB")
pp(psi, name = "muA")
## an example of usage
powFun <- function(psi){
  power.t.test(n = n(psi),
               delta = pp(psi, "muA") - pp(psi, "muB"),
               sd = theta(psi)
               )$power
}
## testing the power-function
powFun(psi)
}
\keyword{misc}
