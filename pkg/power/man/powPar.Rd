\name{powPar}
\alias{powPar}
\title{
  Constructing a \code{powPar}-object
}
\description{
A function for constructing an object of class \code{powPar}. Such an
object is used for
evaluating the power for a parameter range.
All information that is needed for calculating the power should be
provided to this function by making use of the \code{...} argument.
}
\usage{powPar(n, theta.name, xi.name, ...)}
\arguments{
  \item{n}{A numeric vector, indicating for which range of sample sizes
    to evaluate the power.}
  \item{theta.name}{A character that indicates the name of a numeric
    vector that was passed to the function (\code{...}). This vector
    will later be used for evaluating the power. It will be referred to
    as the parameter called \code{theta}.}
    \item{xi.name}{A character that indicates the name of an optional numeric
    vector that was passed to the function (\code{...}). This vector
    will later be used for evaluating the power. It will be referred to
    as the parameter called \code{xi}. Since for every element of
    \code{xi} an individuall sensitivity plot will be constructed, the
    length of the \code{xi} vector is usually short.}
  \item{...}{This arguemt is used to provide all parameters needed for
    calculating the power.}
  }
\details{
An object of class \code{powPar} is used to evaluate the power for a range of
\code{n} and \code{theta} and optionally for several \code{xi} and
several \code{endpoints}. To be able to calculate the sample
size for one particular situation (corresponding to a particular
\code{theta}, \code{xi}, and \code{endpoint} and object of class
\code{powEx} is needed and such an object can be constructed using the
function \code{powEx}.
}
\examples{
psi <- powPar(muA = 0,
              muB = 1,
              sA = 1,
              sB = round(seq(from = 0.5, to = 1.5, by = 0.1), 2),
              n = round(seq(from = 10, to = 30, by = 2),0),
              theta.name = "sB")
}
\keyword{misc}
