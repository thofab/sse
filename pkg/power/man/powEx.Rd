\name{powEx}
\alias{powEx}
\docType{data}
\title{
  Constructing an object of class powEx.
}
\description{
A function for contrating an object of class \code{powEx} used for
drawing an example in a sensitivity plot and for evaluating sample size.

}
\usage{powEx(theta, xi = NA, endpoint = NA, power = 0.9, drop = 0,
method = c("default", "lm", "step"), lm.range = 0.2, forceDivisor = FALSE)}
\arguments{
  \item{theta}{a numeric value indicating for which \code{theta} to draw the
    example in the sensitivity plot and evaluate sample size. It makes
    only sense to use a \code{theta} in the range evaluated.}
  \item{xi}{a numeric value, as \code{theta} but for \code{xi}}
  \item{endpoint}{Object of class \code{character}, indidating for which
    endpoint sample size should be evaluated}
  \item{power}{Object of class \code{numeric}, indicating for what power
    samle size should be evaluated}
  \item{method}{Defining the method how the sample size for the
    indicated theta, xi, and power is "estimated". If the power object
    was created using resampling the "default" evaluates to "lm",
    otherwise to "step". For method "lm" a linear model is fit as lm(sample.size ~
  fisher-transform(power)) with all data where theta, and xi are equal
  to the theta and xi of the example and within the power-range as
  defined by the argument \code{lm.range}. This model is then used for
  predicting the sample size. The method "step" returns the last element
  in the sequence of sample sizes - power pairs, sorted with decreasing power, where the power is above the power defined for the example.}
  \item{lm.range}{The range of evaluations that are used for estimating
    power. See argument \code{method} for details.}
  \item{drop}{Object of class  \code{numeric} (range: 0 to 1), indicating
    how many drop outs are expected in the study. 
    This information is used to calculate the number of subject that
    should be recruited (addressed e.g. by the function \code{tex} using
    type \code{nRec})}.
  \item{forceDivisor}{If \code{TRUE} the biggest common divisor of all
    evaluated sample sizes is used as divisor and the estimated sample
    size is increased to be divisible by this divisor. If an integer is
    provided it is used as divisor.}
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
ex <- powEx(theta = 1, drop = 0.1)
}
\keyword{misc}
