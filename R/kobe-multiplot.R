#' @title multiplot
#' 
#' @description 
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, by.row=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#'
#' borrowed with thanks from
#' http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
#' 
#' @param ... \code{ggplot} objects
#' @param plotlist a list \code{ggplot} objects 
#' @param cols number of columns in plot grid, by default=1 
#' 
#' @return an \code{ggplot2} object
#' 
#' @export
#' @rdname multiplot
#' 
#' @examples
#' \dontrun{
#' multiplot(plot(1),plot(2))
#' } 

multiplot <- function(..., plotlist=NULL, cols=1) {
  #require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}