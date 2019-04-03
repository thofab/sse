\name{power-class}
\Rdversion{1.1}
\docType{class}
\alias{power-class}
\alias{powEx-class}
\alias{show,power-method}
\alias{update,power-method}
\alias{refine,power-method}
\alias{inspect,power-method}
\alias{dim,power-method}
\alias{plot,power-method}
\alias{tex,power,character-method}
\alias{plot,ANY,ANY-method}
\alias{plot,power,missing-method}

\title{Class "power"}
\description{follows}
\section{Objects from the Class}{
are ..
}
\section{Slots}{
  \describe{
    \item{\code{cluster}:}{Object of class \code{"list"} ~~ }
    \item{\code{core}:}{Object of class \code{"array"} ~~ }
    \item{\code{divisor}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{drop}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{endpoint.example}:}{Object of class \code{"character"} ~~ }
    \item{\code{forceDivisor}:}{Object of class \code{"logical"} ~~ }
    \item{\code{iter}:}{Object of class \code{"integer"} ~~ }    
    \item{\code{list}:}{Object of class \code{"list"} ~~ }
    \item{\code{lm.range}:}{Object of class \code{"list"} ~~ }
    \item{\code{method}:}{Object of class \code{"character"} ~~ }
    \item{\code{n}:}{Object of class \code{"integer"} ~~ }
    \item{\code{n.act}:}{Object of class \code{"integer"} ~~ }
    \item{\code{power.example}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{return.list}:}{Object of class \code{"character"} ~~}
    \item{\code{return.n}:}{Object of class \code{"character"} ~~ }
    \item{\code{return.names}:}{Object of class \code{"character"} ~~ }
    \item{\code{return.power}:}{Object of class \code{"character"} ~~ }
    \item{\code{size}:}{Object of class \code{"array"} ~~ }
    \item{\code{statistic}:}{Object of class \code{"function"} ~~ }
    \item{\code{theta}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{theta.act}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{theta.example}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{theta.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{xi}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{xi.act}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xi.example}:}{Object of class \code{"numeric"} ~~ }

  }
}
\section{Slots from powEx}{
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
\section{Extends}{
Class \code{"\linkS4class{powCalc}"}, directly.
Class \code{"\linkS4class{powEx}"}, directly.
Class \code{"\linkS4class{powFun}"}, by class "powCalc", distance 2.
Class \code{"\linkS4class{powPar}"}, by class "powCalc", distance 2.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "power", y = "missing")}: ... }
    \item{show}{\code{signature(object = "power")}: ... }
	 }
       }
\usage{
\S4method{plot}{power}(x = "power", y = "missing")
\S4method{show}{power}(object = "power")
\S4method{tex}{power,character}(x, type, ...)
\S4method{inspect}{power}(object)
\S4method{plot}{power,missing}(x, y, ...)
}
\author{\email{thomas.fabbro@unibas.ch}}
\examples{
showClass("power")
}
\keyword{internal}
\keyword{classes}
