\name{kobeProb}
\alias{kobeProb}
\title{Calculates 2 dimenisonal confidence intervals for data pairs.}
\usage{
  kobeProb(x, y, prob=c(0.5, 0.75, 0.95), na.rm=FALSE)
}
\arguments{
  \item{x}{numeric vector}

  \item{y}{numeric vector}

  \item{prob}{numeric vector of probabilities with values in [0,1].}

  \item{na.rm}{logical; if true, any NA and NaN's are removed from x before the quantiles are computed.}
}
\description{
  \code{kobeProb} Uses kde2d to calculate a bivariate density, then normalises the results and calculates the contour corresponding to a contained volume of the total volume under the surface (i.e. a two-dimensional Bayesian credible region).
}
\examples{

\dontrun{
data(prj)
geom_path(aes(x,y,group=level),colour="blue",
          data=kobeProb(prj$pts$stock,prj$pts$harvest,prob=c(0.7,.5,.25)))}

}
\seealso{

\code{\link{kobePhase}},\code{\link{kobeInterp}},\code{\link{kobeP}}

}

