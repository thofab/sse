\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,power,missing-method}
\title{Plot functions}
\description{A sensitivity plot for the sample size calcualtion
  indicating, using a contour for a given power, how sample size changes
  if \code{theta} is varied.
}
\usage{\S4method{plot}{power,missing}(x, y, ...)}
\arguments{
  \item{x}{The object of class \code{power} used for plotting}
  \item{y}{Not used}
  \item{...}{additional arguments implemented:
    \itemize{
        \item{\code{at} numeric vector giving breakpoints along the
	  power range.
          Contours (if any) will be drawn at these power. The first
	  number indicates, which contour should be emphasised.}
	\item{\code{smooth} a logical that indicates if the countour should be
	smoothed. If TRUE a span of 0.5 will be used alternatively the
	argument smooth can also take a numeric value between 0 and 1
	that will be used for smoothing.}
      \item{\code{example} a logical indicating if an example should be
	drawn or not. An example is an arrow that points from the
	particular \code{theta} on the x-axis to the contour line and to
      the sample size on the y-axis.}
      }
    }
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY", y = "ANY")}}{ to follow}

\item{\code{signature(x = "power", y = "missing")}}{ Generates a
  contourplot with theta on the x-axis and n on the y-axis and the
  contours for the power indicated with the argument at.}
}}
\keyword{methods}

