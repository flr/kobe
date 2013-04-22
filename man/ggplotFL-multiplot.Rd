\name{multiplot}
\alias{multiplot}
\title{Calculates confidence intervals for different probabiity levels for pairs of data points.}
\usage{
  multiplot(..., plotlist=NULL, file, cols=1, layout=NULL)
}
\arguments{
  \item{...}{\code{ggplot2} objects}

  \item{plotlist}{list of \code{ggplot2} objects}

  \item{file}{character}
  
  \item{cols}{numeric; number of columns in layout}

  \item{layout}{numeric; A matrix specifying the layout. If present, 'cols' is ignored}
}
\description{
A multiple plot function, allowing ggplot2 objects to be plotted together. If the layout is something like matrix(c(1,2,3,3), nrow=2, by.row=TRUE),
then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.

Borrowed with thanks from http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
  
}
\examples{

\dontrun{
a=ggplot(data.frame(x=1:10,y=1:10))+geom_point(aes(x,y))
multiplot(a,a)

}
}
\seealso{

\code{\link{kobePhase}},\code{\link{kobeInterp}},\code{\link{kobeP}}

}

