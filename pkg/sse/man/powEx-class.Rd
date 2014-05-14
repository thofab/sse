\name{powEx-class}
\Rdversion{1.1}
\docType{class}
\alias{powEx-class}
\title{Class "powEx"}
\description{The power is always estimated for a range of }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("powEx", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{theta.example}:}{Object of class \code{"numeric"}, the
      value of the parameter theta used for illustration and for
      estimating sample size.}
    \item{\code{xi.example}:}{Object of class \code{"numeric"} Like
      theta.example but for xi-}
    \item{\code{endpoint.example}:}{Object of class \code{"character"},
      the endpoint to be used, only if several endpoints were evaluated.}
    \item{\code{power.example}:}{Object of class \code{"numeric"}}
    \item{\code{method}:}{Object of class \code{"character"} ~~ }
    \item{\code{lm.range}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{drop}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{forceDivisor}:}{Object of class \code{"logical"} ~~ }
    \item{\code{divisor}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{merge}{\code{signature(x = "powCalc", y = "powEx")}: ... }
	 }
}
\author{\email{thomas.fabbro@unibas.ch}}
\examples{
##showClass("powEx")
}
\keyword{classes}
