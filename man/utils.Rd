\docType{functions}
\name{iav}
\alias{dRate}
\alias{hinge}
\alias{iav}
\alias{incr}
\alias{recovered}
\title{iav}
\usage{
  iav(x)

  incr(x)

  dRate(x, r)

  hinge(s, v, rec)

  recovered(stock, harvest)
}
\arguments{
  \item{x;}{a vector holding a time series}

  \item{x;}{a vector holding a time series}

  \item{x;}{a vector holding a time series}

  \item{x;}{a vector holding a time series}

  \item{x;}{a vector holding a time series}
}
\value{
  a \code{vector} with the inter-annual variation each time
  step

  a \code{logical} indicating an increase

  a \code{logical} indicating an increase

  a \code{logical} indicating an increase

  a \code{logical} indicating an recovered
}
\description{
  Calculates the inter-annual variation in a time series,
  i.e. \code{(x[t+1]-x[t])/x[t]} Used to show how variable
  a quantity like yield is under different management
  strategies within a Management Strategy Evaluation.

  Is a quantity increasing from 1 time step to another
  \code{(x[t+1]-x[t])>0}

  Is a quantity increasing from 1 time step to another
  \code{(x[t+1]-x[t])>0}

  Is a quantity increasing from 1 time step to another
  \code{(x[t+1]-x[t])>0}

  Has the stock recovered yet? i.e. stock>=1 and harvest<=1
  in the current or an earlier time step. In other words
  has it been in the green Kobe quadrant.
}
\examples{
x=rnorm(2)
   iav(x)
   ## inter-annual average variation
   mean(iav(x),na.rm=T)
x=rnorm(2)
   incr(x)
x=rnorm(2)
   incr(x)
x=rnorm(2)
   incr(x)
harvest=rlnorm(20)
   stock  =rlnorm(20)
   recovered(stock,harvest)
}

