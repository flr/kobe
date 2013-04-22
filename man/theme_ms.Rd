\name{theme_ms}
\alias{theme_ms}
\title{ggplot2 theme}

\description{A ggplot2 theme for presentation of plots}
\usage{theme_ms(base_size=12, base_family="", ...) }

\arguments{
  \item{base_size}{numeric: default font size.}

  \item{base_family}{character; default font.}
  
  \item{...}{any other arguments.}
}

\details{

The ggplot2 "grammar of graphics" package includes "Themes" for ggplot2 charts, 
making it possible to define and re-use your own preferences for title fonts and sizes, 
tick marks, grid color, etc. 
}

\source{see https://github.com/hadley/ggplot2/wiki/Themes}

\examples{
data(prj)
ggplot(subset(prj$trks, Percentile=="50\%"))             + 
           geom_line(aes(year,stock,group=TAC,col=TAC)) +
           theme_ms()
}
\keyword{ggplot2}
\keyword{theme}
\keyword{plot}
