\name{n}
\alias{n}
\title{Extracting the actual n}
\description{
  Extracting the actual \code{n} from a \code{powPar}-object.
  This method is needed within the function (referred to as
  power-function) needed for evaluating the power.
}
\usage{n(object, ...)}
\arguments{
  \item{object}{An object of class \code{powPar}.}
  \item{...}{.}
  }
\details{
During the evaluation process every \code{n}, from a sequence defined
in the \code{powPar}-object, needs to be evaluated.
This function extracts the actual \code{n} that varies during the
evaluation process.
When a \code{powPar}-object is created, the first element of \code{n} is
also set to be the actual.
This allows to use this method also outside the evaluation for testing
the power function.
}
\value{A numeric value.}
\note{Do not use the method \code{pp} inside the power-function
  because this would extract the whole sequence of \code{n}.}
\examples{
psi <- powPar(muA = 0,
              muB = 1,
              sA = 1,
              sB = round(seq(from = 0.5, to = 1.5, by = 0.05), 2),
              n = round(seq(from = 10, to = 30, by = 2),0),
              theta.name = "sB")
n(psi)
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
