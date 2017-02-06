utils::globalVariables(c("bandwidth.nrd","HPDregionplot","mcmc"))

#' prob
#' @description 
#' Calculates the probability of an obervations occurring in a 2D cell using HPDregionplot 
#' Given a sample calculates the  bivariate region of highest marginal posterior density 
#' for two variables, using kde2d from MASS to calculate a bivariate density.
#'            
#' @aliases 
#' prob
#' 
#' @param x a vector
#' @param y a vector
#' @param prob probability levels
#' @param n number of points at which to evaluate the density grid
#' @param h bandwidth of 2D kernel smoother (previous default value was c(1,1), which worked 
#' poorly with some plots with very small scales; if not specified, defaults to values in kde2d)
#' @param lims limits, specified as (x.lower,x.upper,y.lower,y.upper) (passed to kde2d)
#' @param na.rm logical; if true, any NA and NaN's are removed from x before calculations
#' @param ... any other argument
#'
#' @return a \code{data.frame} with three variables
#' \code{x, y} coordinates of the grid points, vectors of length n.
#' \code{level} contours corresponding to \code{prob}
#' 
#' 
#' @export
#' @docType methods
#' @rdname prob
#' 
#' @examples
#' \dontrun{
#'    y=rnorm(20)
#'    x  =rnorm(20)
#'    prob(x,y)}
probFn2=function(x,y,prob=c(0.5, 0.75,0.95),n=21,h=c(bandwidth.nrd(x),bandwidth.nrd(y)),lims=NULL,na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
  
  tmp=HPDregionplot(mcmc(data.frame(x,y)),prob=prob,h=h)
  
  prb=ldply(tmp, function(dat) data.frame(level=dat$level,x=dat$x, y=dat$y))
  
  return(prb)}

setMethod('prob', signature(stock="numeric",harvest="numeric"),
    function(stock,harvest){
                                  
    res=probFn(data.frame(stock=stock,harvest=harvest))
      
    return(res)})

setMethod('prob', signature(stock='data.frame',harvest="missing"),
    function(stock){
     
    res=probFn(stock)
      
    return(res)})

probFn=function(x) {
  
  b =  pmax(pmin(as.integer(x$stock),  1),0)
  f =1-pmax(pmin(as.integer(x$harvest),1),0)
  p =f*b
  collapsed=(1-b)*(1-f)
  
  red   =collapsed
  green =p
  yellow=1-p-collapsed
  
  overFished =1-b
  overFishing=1-f  
  
  data.frame(red=red,green=green,yellow=yellow,overFished=overFished,overFishing=overFishing)}

