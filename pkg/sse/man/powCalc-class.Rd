\name{powCalc-class}
\Rdversion{1.1}
\docType{class}
\alias{powCalc-class}
\alias{powFun-class}
\alias{dim,powCalc-method}
\alias{update,powCalc-method}
\alias{powEx,powCalc-method}

\title{Class "powCalc" }
\description{A class for storing information of the power
  calculation. It inherits from "powPar", that contains the information
  for the calculation and from from "powEx", that contains information
  about the power-function.}
\section{Objects from the Class}{
Objects can be created by calling the creator \code{\link{powCalc}}.
}
\section{Slots}{
  \describe{
    \item{\code{core}:}{Object of class \code{"array"} with 4
      dimentions, n, theta, xi and endpoint.}
    \item{\code{core}:}{Object of class \code{"array"} with 4
      dimentions, n, theta, xi and endpoint.}
    \item{\code{statistic}:}{Object of class \code{"function"}, this
      function was used for calculating the power.}
    \item{\code{endpoint.name}:}{Object of class \code{"character"}
      names of the endpoints}
    \item{\code{iter}:}{Object of class \code{"integer"} how many
      iterations were used to estimate the power}
  }
}
\section{Slots inherited from powFun}{
  \describe{
    \item{\code{statistic}:}{Object of class \code{"function"}}
    \item{\code{return.power}:}{Object of class \code{"logical"}}
    \item{\code{return.list}:}{Object of class \code{"logical"}}
    \item{\code{return.n}:}{Object of class \code{"integer"}}
    \item{\code{return.names}:}{Object of class \code{"character"}}
  }
}
\section{Slots inherited from powPar}{
  \code{"\linkS4class{powPar}"}
}
\section{Extends}{
Class \code{"\linkS4class{powFun}"}, directly.
Class \code{"\linkS4class{powPar}"}, directly.
}
\usage{
\S4method{powEx}{powCalc}(x, theta, xi = NA, endpoint = NA, power = 0.9, drop = 0,
                   method = c("default", "lm", "step"), lm.range = NA,
                   forceDivisor = FALSE)
\S4method{update}{powCalc}(object, ...)
\S4method{dim}{powCalc}(x)
}
\note{ none }
\examples{
showClass("powCalc")
}
\keyword{internal}
\keyword{classes}
