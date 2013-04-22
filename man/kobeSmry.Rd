\name{kobeSmry-methods}
\docType{methods}
\alias{kobeSmry}
\alias{kobeSmry-methods}
\alias{kobeSmry,data.frame,missing-method}
\alias{kobeSmry,numeric,numeric-method}

\title{Calculates the probability of being over-fished or subject to over-fishing and being in the red, green or yellow regions of the kobe phase plots quadrants.}
\usage{
  kobeSmry(stock,harvest,...)
}


\section{Methods}{
\describe{

\item{\code{signature(stock="numeric,"harvest="numeric")}}{
takes as input numeric vectors of stock and harvest  
}

\item{\code{signature(stock="data.frame",harvest="missing")}}{
takes as input a data.frame with columns names as \code{stock} and \code{harvest}  
}
}}


\arguments{
  \item{stock}{numeric vector representing stock or a data.frame with columns for stock and harvest}

  \item{harvest}{numeric vector respresenting harvest}
  
  \item{...}{other}
}
\description{
  \code{kobeSmry} 
}
\examples{

\dontrun{
data(sims)
## calculate probabilities
res=ddply(subset(sims, year > 2010 & year <= 2020),.(year,TAC), kobeSmry)

## K2SM for B>BMSY
1-cast(res,TAC~year,value="overFished")

## K2SM for F<FMSY
1-cast(res,TAC~year,value="overFishing")

## K2SM for green quadrant
cast(res,TAC~year,value="green")
}

}

\seealso{

\code{\link{kobePhase}},\code{\link{kobeInterp}},\code{\link{kobeP}}


}

