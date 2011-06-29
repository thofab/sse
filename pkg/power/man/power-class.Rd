\name{power-class}
\Rdversion{1.1}
\docType{class}
\alias{power-class}
\alias{show,power-method}
\alias{plot.power}
\title{Class "power"}
\description{follows}
\section{Objects from the Class}{
are ..
}
\section{Slots}{
  \describe{
    \item{\code{core}:}{Object of class \code{"array"} ~~ }
    \item{\code{endpoint.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{n.iter}:}{Object of class \code{"integer"} ~~ }
    \item{\code{list}:}{Object of class \code{"list"} ~~ }
    \item{\code{theta}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{theta.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{theta.act}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{xi.act}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{n}:}{Object of class \code{"integer"} ~~ }
    \item{\code{n.act}:}{Object of class \code{"integer"} ~~ }
    \item{\code{theta.example}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi.example}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{endpoint.example}:}{Object of class \code{"character"} ~~ }
    \item{\code{power.example}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{drop}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{powCalc}"}, directly.
Class \code{"\linkS4class{powEx}"}, directly.
Class \code{"\linkS4class{Resample}"}, by class "powCalc", distance 2.
Class \code{"\linkS4class{powPar}"}, by class "powCalc", distance 2.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "power", y = "missing")}: ... }
    \item{show}{\code{signature(object = "power")}: ... }
	 }
}
\author{\email{fabbrot@uhbs.ch}}
\examples{
showClass("power")
}
\keyword{classes}
