\name{xi-methods}
\docType{methods}
\alias{xi}
\alias{xi-methods}
\alias{xi,powPar-method}
\title{
Extracting the actual xi.
}
\description{
  Extracting the actual \code{xi} from a \code{powPar}-object.
  This method is needed within the function (referred to as
  power-function) needed for evaluating the power.
}
\usage{\S4method{xi}{powPar}(object, ...)
}
\arguments{
  \item{object}{An object of class \code{powPar}.}
  \item{...}{Not used.}
}
\details{
During the evaluation process every \code{xi}, from a sequence defined
in the \code{powPar}-object, needs to be evaluated.
This function extracts the actual \code{xi} that varies during the
evaluation process.
When a \code{powPar}-object is created, the first element of \code{xi} is
also set to be the actual.
This allows to use this method also outside the evaluation for testing
the power function.
}
\value{A numeric value.}
\note{Do not use the method \code{pp} with argument \code{name =
    "xi"} inside the power-function unless you really need to extract the whole sequence of \code{xi}.}
\examples{
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05), #muA
              muB = 1,
              sA = 1,
              xi = c(1, 1.5), #sB
              n = seq(from = 10, to = 30, by = 2))
xi(psi)
## an example of usage
powFun <- function(psi){
  power.t.test(n = n(psi),
               delta = theta(psi) - pp(psi, "muB"),
               sd = xi(psi)
               )$power
}
## testing the power-function
powFun(psi)
## evaluation
calc <- powCalc(psi, powFun)
}
\keyword{misc}
