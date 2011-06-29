\name{powPar-class}
\Rdversion{1.1}
\docType{class}
\alias{powPar-class}
\alias{dim,powPar-method}

\title{Class "powPar"}
\description{A class needed to calculate the power array. It containing
  all the parameters needed to feed a function that calculates the power. These are either numbers or vectors of numbers.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("powPar", ...)}.

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
\section{Methods}{
  \describe{
    \item{dim}{\code{signature(x = "powPar")}: ... }
	 }
}
\references{ none}
\author{\email{fabbrot@uhbs.ch}}
\note{none}

\seealso{
\code{\link{powPar}},\code{\linkS4class{power}}
}
\examples{
## showClass("powPar")
}
\keyword{classes}
