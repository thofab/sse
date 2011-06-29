\name{powCalc-class}
\Rdversion{1.1}
\docType{class}
\alias{powCalc-class}
\alias{merge,powCalc,powEx-method}

\title{Class "powCalc" }
\description{A class for storing information of the power
  calculation. It inherits from "powPar", that contains the information
  for the calculation.}
\section{Objects from the Class}{
Objects can be created by calling the creator \code{"powCalc"}
}
\section{Slots}{
  \describe{
    \item{\code{core}:}{Object of class \code{"array"} with 4
      dimentions, n, theta, xi and endpoint }
    \item{\code{statistic}:}{Object of class \code{"function"}, this
      function was used for calculating the power }
    \item{\code{endpoint.name}:}{Object of class \code{"character"}
      names of the endpoints}
    \item{\code{n.iter}:}{Object of class \code{"integer"} how many
      iterations were used to estimate the power}
    \item{\code{list}:}{Object of class \code{"list"} from class \code{"powPar"} }
    \item{\code{theta}:}{Object of class \code{"numeric"}  from class \code{"powPar"} }
    \item{\code{theta.name}:}{Object of class \code{"character"}  from class \code{"powPar"} }
    \item{\code{theta.act}:}{Object of class \code{"numeric"}  from class \code{"powPar"} }
    \item{\code{xi}:}{Object of class \code{"numeric"}  from class \code{"powPar"} }
    \item{\code{xi.name}:}{Object of class \code{"character"}  from class \code{"powPar"} }
    \item{\code{xi.act}:}{Object of class \code{"numeric"}  from class \code{"powPar"} }
    \item{\code{n}:}{Object of class \code{"integer"} from class \code{"powPar"} }
    \item{\code{n.act}:}{Object of class \code{"integer"}  from class \code{"powPar"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Resample}"}, directly.
Class \code{"\linkS4class{powPar}"}, directly.
}
\section{Methods}{
  \describe{
    \item{merge}{\code{signature(x = "powCalc", y = "powEx")}: ... }
	 }
}
\references{ none }
\author{ \email{fabbrot@uhbs.ch} }
\note{ none }
\examples{
showClass("powCalc")
}
\keyword{classes}
