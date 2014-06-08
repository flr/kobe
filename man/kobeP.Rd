\name{kobeP-methods}
\docType{methods}
\alias{kobeP}
\alias{kobeP-methods}

\title{Calculates whether each point corresponds to being over-fished or subject to over-fishing and being in the red, green or yellow regions of the kobe phase plots quadrants.}
\usage{
  kobeP(stock, harvest,...)
}
\arguments{
  \item{stock}{numeric vector representing stock or a data.frame with columns for stock and harvest}

  \item{harvest}{numeric vector resprenting harvest}
  
  \item{...}{other}
}
\description{
  \code{kobeP} 
}
\examples{

\dontrun{
data(sims)
## where is point
res=head(kobeP(sims$stock,sims$harvest))
}
}

\seealso{

\code{\link{kobeInterp}},\code{\link{kobeP}}


}

