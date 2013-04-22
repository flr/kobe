\name{kobeTrks-methods}
\docType{methods}
\alias{kobeTrks}
\alias{kobeTrks-methods}
\alias{kobeTrks,data.frame,missing-method}
\alias{kobeTrks,numeric,numeric-method}
\title{Calculates confidence intervals for different probabiity levels for pairs of data points.}

\section{Methods}{
\describe{

\item{\code{signature(stock="data.frame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(stock="numeric",harvest="numeric")}}{
%%  ~~describe this method here~~
}
}}

\usage{
  kobeTrks(stock,harvest,...)
}

\arguments{
  \item{stock}{numeric vector representing stock or a data.frame with columns for stock and harvest}
  
  \item{harvest}{character}
  
  \item{...}{character}
 }
               
               \description{
               \code{kobeTrks} Uses \code{kde2d} to calculate a bivariate density, normalizes the results and then calculates the contour(S) corresponding to a contained volume of points of the total volume under the surface (i.e. a two-dimensional Bayesian credible region).
               }
               \examples{
               
               #data(sims)
               #pts=subset(sims,year==2010)
               #kobeTrks(x=pts$stock, y=pts$harvest)
               
               }
 

