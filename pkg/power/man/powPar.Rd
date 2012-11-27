\name{powPar}
\alias{powPar}
\title{
  Constructing an object of class powPar.
}
\description{
A function for constructing an object of class \code{powPar}. Such an
object is used for evaluating the user defined``power function'' for a parameter range.
All information that is needed for calculating the power should be
provided to this function by making use of the \code{...} argument.
}
\usage{powPar(n, theta = NA, xi = NA, ...)}
\arguments{
  \item{n}{A numeric vector, indicating for which sample sizes
    to evaluate the power function.}
  \item{theta}{A numeric vector that will be used for evaluating the
    power function. The method \code{theta} can be used within the power
    function to extract the elements of this vector one by one.}
  \item{xi}{A numeric vector that will be used for evaluating the power function.
    Since for every element of\code{xi} an individuall sensitivity plot
    has to be constructed, the length of the \code{xi} vector is usually short.}
  \item{...}{This arguemt is used to provide all parameters needed by
    the power function for calculating the power.}
  }
\details{
  An object of class \code{powPar} is used to evaluate the power function for a range of
  \code{n} and \code{theta} and optionally for several \code{xi} values.

  The user can write a power function and extract the individual
  elements using the methods \code{n}, \code{theta}, and \code{xi}.

  It is a good practice to include everything that is needed for the
  calculation, also data sets etc.

  To extract the vector of theta, instead of individual values, you can
  use the method \code{pp} with the name theta.
  
  For historical reasons: If the argument \code{theta} is \code{NA} the argument
  \code{theta.name} (a character)  has to be used, to  indicate the name of a numeric
  vector that was passed to the argument (\code{...}).
  The same is true for the argument \code{xi}.
}
\value{An object of the class \code{powPar}}
\examples{

library(power)
psi <- powPar(n = seq(from = 20, to = 60, by = 2),
              theta = seq(from = 0.5, to = 1.5, by = 0.05)
              )
powFun <- function(psi){
  return(power.t.test(n = n(psi)/2, delta = theta(psi), sig.level = 0.05)$power)
}
pp(psi, name = "theta")
calc <- powCalc(psi, statistic = powFun)
ex <- powEx(theta = 1)
pow <- merge(calc, ex)
plot(pow, example = FALSE)
inspect(pow)
tex(pow, "nEval")  # How many subjects should be available for analysis

psi2 <- powPar(n = seq(from = 20, to = 60, by = 2),
              theta = seq(from = 0.5, to = 1.5, by = 0.05),
              xi = c(0.5, 1, 1.5)
              )

calc2 <- powCalc(psi2, statistic = powFun)
ex2 <- powEx(theta = 1, xi = 0.5)
pow2 <- merge(calc2, ex2)
plot(pow2)
tex(pow2, "nEval")  # How many subjects should be available for analysis

}
\keyword{misc}
