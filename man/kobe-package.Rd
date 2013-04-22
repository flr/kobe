\name{kobe-package}
\alias{kobe-package}
\docType{package}
\title{
Tools for Tuna RFMOs in the Kobe
}
\description{

Scientific Advice within the tuna Regional Fisheries Management Organisations (tRFMOs) is based on a common framework. This requires estimating the probability that fishing mortality is less than FMSY and SSB greater than BMSY	for a range of management options; e.g. a range of total allowable catchess (TAC). 

This package provides methods for summarising stock assessment results (i.e. estimates of stock status, reference points and projections) in the Kobe advice format.

}
\details{
\tabular{ll}{
Package: \tab kobe\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-06-22\cr
License: \tab GPL-2\cr
Depends: \tab methods, ggplot2, reshape, plyr, grid, MASS, coda, emdbook, tables\cr
Collate: \tab kobe-generics.R kobe-funcs.R kobe-methods.R kobe-aspic.R kobe-2box.R kobe-ss.R kobe-MFCL.R multiplot.R kobe-Phase.R kobe-k2sm-ggplot2.R kobe-themes.R\cr
Built: \tab R 2.15.1; ; 2012-10-11 17:37:36 UTC; unix\cr
}

Index:
\preformatted{
kobe-package            Provides tools for plotting advice in the tRFMO format
kobe2box                Reads pro2box projection results and returns
                        stock and harvest rate relative to BMSY and
                        FMSY
kobe2sm                 Calculates confidence intervals for different
                        probabiity levels for pairs of data points.
kobeAspic               Reads ASPIC bootstrap and projections outputs
                        and returns stock and harvest rate relative to
                        BMSY and FMSY
kobeMFCL                Reads Multifan-CL assessment results and
                        returns stock and harvest rate relative to BMSY
                        and FMSY
kobeP                   Calculates the probability of being over-fished
                        or subject to over-fishing and being in one of
                        the kobe phase plots quadrants.
kobePhase               Calculates confidence intervals for different
                        Phaseabiity levels for pairs of data points.
kobePhaseMar            Calculates confidence intervals for different
                        Phaseabiity levels for pairs of data points.
kobeProb                Calculates confidence intervals for different
                        probabiity levels for pairs of data points.
kobeSS                  Reads SS assessment results and returns stock
                        and harvest rate relative to BMSY and FMSY
kobeShade               Calculates confidence intervals for different
                        probabiity levels for pairs of data points.
kobeTrks                Calculates confidence intervals for different
                        Trksabiity levels for pairs of data points.
multiplot               Calculates confidence intervals for different
                        probabiity levels for pairs of data points.
sims                    kobe datasets
}

The package can be used to read in stock assessment results as text files from a variety of packages. These can then be summarised and plotted. 
}
\author{Laurence Kell \\ Maintainer: Laurence Kell <laurie.kell@iccat.int>}

\references{
Use \url{http://www.tuna-org.org/Documents/TRFMO3/K3-REC_ENG.pdf} %or \href{}{} for web.
}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
