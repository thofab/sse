\name{powPar-class}
\Rdversion{1.1}
\docType{class}
\alias{powPar-class}
\alias{n,powPar-method}
\alias{theta,powPar-method}
\alias{xi,powPar-method}
\alias{pp,powPar-method}
\alias{dim,powPar-method}
\alias{powCalc,powPar-method}

\title{Class "powPar"}
\description{A class used internally. Contains all the parameters
  needed to feed the \"power function\".
  For users there is a function
  \code{\link{powPar}} that constructs objects of this class and there are
  several functions for extracting information \code{\link{n}},
  \code{\link{theta}}, \code{\link{xi}}, \code{\link{pp}}.
}
\section{Slots}{
  \describe{
    \item{\code{list}:}{Object of class \code{"list"} ~~ }
    \item{\code{theta}:}{An \code{"numeric"} vector with possible values
      of a parameter that influences the power. The power will be calculated for
      every element of this vector to see how the power estimate depends
    on theta.}
    \item{\code{theta.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{theta.act}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{xi.act}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{n}:}{Object of class \code{"integer"} ~~ }
    \item{\code{n.act}:}{Object of class \code{"integer"} ~~ }
  }
}
\usage{
\S4method{theta}{powPar}(x)
\S4method{n}{powPar}(x)
\S4method{xi}{powPar}(x)
\S4method{pp}{powPar,character}(x, name)
\S4method{dim}{powPar}(x)
}

\seealso{
\code{\link{powPar}},\code{\linkS4class{power}}
}
\examples{
## showClass("powPar")
}
\keyword{internal}
\keyword{classes}
