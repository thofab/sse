\name{powCalc}
\alias{powCalc}
\title{
Power calculation
}
\description{
  The power for the whole range of \code{theta}, \code{xi} and \code{n},
  as specified in the \code{powPar}-object,
  is calculated using the user-defined power-function.
}
\usage{powCalc(object, statistic, ...)}
\arguments{
  \item{object}{An object of class \code{powPar}.}
  \item{statistic}{A function that takes an object of class
    \code{powPar} as argument. Ideally this is also the only
    argument. The function should return a vector of numeric values or a
    vector of logicals, depending on the \code{type}. See Details.
  }
  \item{...}{\itemize{
      \item{type}{A character string specifying the type of power
	calculation. There are two forms implemented:
	\sQuote{\code{"power"}} for power-functions that return a numeric
	value, the power and \sQuote{\code{"resample"}} for power-functions
	that return logicals. In the latter case the argument \code{n.iter}
	specifies how often the power-function is evaluated for estimating
	the number of successes, the power.}
      \item{n.iter}{A number specifying how often the power-function is evaluated.}
      \item{cluster}{documentation follows}
      }
  }}
\details{none
}
\value{An object of class \code{calc}.}
\seealso{The function \code{\link{merge}} will, together with an object
  of class \code{powEx} form an object of class \code{power}}
\examples{
## no example so far
}
\keyword{misc}
