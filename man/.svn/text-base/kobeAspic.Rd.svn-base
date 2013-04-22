\name{kobeAspic}
\alias{kobeAspic}
\title{Reads ASPIC bootstraps and projections and returns stock and harvest rate relative to MSY reference points}

\usage{
  kobeAspic(object,prb,dir="", 
            what=c("sims","trks","pts","smry","wrms")[1], prob=c(0.75,0.5,0.25),year=NULL,nwrms=10)
}
\arguments{
  \item{object}{character: ".bio" file with estimates of stock biomass and harvest rate trajectories for each ASPIC bootstrap trial.}

  \item{prb}{character; ".prb" file(s) with projection results, i.e. extension of data in \code{bio} for TACs, 1 file per TAC.}
 
  \item{dir}{character; optional argument identifying directory where files are.}

  \item{what}{character; specifies the form of data returned.}
  
  \item{prob}{numeric; probabilities with values in [0,1] for percentiles returned if \code{what["trks"]} selected.}

  \item{year}{numeric; year(s) for points if \code{what["pts"]} selected.}

  \item{nwrms}{numeric; number of trajectories sampled at random to return if \code{what["wrms"]} selected.}
}
\description{
  Reads ASPIC output files with bootstrapped assesssment and projections and returns time series or summaries of B:BMSY and F:FMSY. \code{what} specifies how data are returned
, i.e. \code{sims} bootstraps for all years; \code{trks} percentiles given by \code{prob};
\code{year} selected year(s) given by \code{pts}; \code{smRY} probabilities of being in the kobe quadrants and; \code{wrms} bootstraps selected at random. 
If of length 1 then a data.frame is returned, if >1 then a list of data.frames.}

\examples{
\dontrun{

#aBio="http://gbyp-sam.googlecode.com/svn/trunk/data/ASPIC/whm/2012/noBrazil/aspic.bio"

res=kobeAspic(aBio,what=c("pts","trks"))

kobePhase(res$pts)+
     geom_point(aes(stock,harvest))+
     geom_path( aes(stock,harvest),data=subset(res$trks,Percentile=="50\%"))+
     scale_y_continuous(limits=c(0,3))
}
}

\seealso{
  \code{\link{kobe2box}},\code{\link{kobeMFCL}},\code{\link{kobeSS}}
}

