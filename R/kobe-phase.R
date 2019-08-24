#' @import ggplot2 

utils::globalVariables(c("x","y","fill"))

### provide a back drop on which to overlay data
kobePhaseFn=function(object,xlim,ylim,quadcol=c("red","green","yellow","yellow"),bref=1,  fref=1){    
  quads<- rbind(data.frame(x=c(bref[1],bref[1],-Inf,-Inf), y=c(fref[1], Inf,     Inf,     fref[1]), fill="1"),
                data.frame(x=c(bref[1],bref[1], Inf, Inf), y=c(-Inf,    fref[1], fref[1],-Inf),     fill="2"),
                data.frame(x=c(bref[1],bref[1],-Inf,-Inf), y=c(fref[1],-Inf,    -Inf,     fref[1]), fill="3"),
                data.frame(x=c(bref[1],bref[1], Inf, Inf), y=c( Inf,    fref[1], fref[1], Inf),     fill="4"))
  
  p=ggplot(object)+geom_polygon(data=quads,aes(x,y,fill=fill)) +
    scale_fill_manual(values=quadcol[1:4], guide="none") +
    ylab(expression(F/F[MSY]))        +
    xlab(expression(SSB/B[MSY]))      +
    coord_cartesian(xlim=xlim,ylim=ylim)
  
  invisible(p)}

##############################################################
#' kobePhase
#'
#' @name kobePhase
#' 
#' @description
#' Creates The Kobe Phase Plot
#' @aliases kobePhase-method kobePhase,missing-method kobePhase,data.frame-method
#'
#' @param  object an object of class \code{missing,data.frame,...}
#' @param  xlim a numeric vector with x-axis limits, by default is c(0.2) 
#' @param  ylim a numeric vector with y-axis limits, by default is the same as xlim 
#' @param  quadcol colours for the quadrants
#' @param  bref vertical seperation between quadants 
#' @param  fref horizontal seperation between quadants 
#' @export
#' @docType methods
#' @rdname  kobePhase-method
#'
#' @examples
#' \dontrun{kobePhase()}
setMethod('kobePhase', signature(object='missing'),
  function(object,xlim=c(0,2),ylim=xlim,quadcol=c("red","green","yellow","gold"),bref=1,fref=1){
   
       invisible(kobePhaseFn(NULL,xlim,ylim,quadcol=quadcol,bref=bref,fref=fref))})

setMethod('kobePhase', signature(object='data.frame'),
  function(object,xlim=c(0,ceiling(2*max(object$stock,  na.rm=TRUE))/2),
                  ylim=c(0,ceiling(2*max(object$harvest,na.rm=T))/2),
                  quadcol=c("red","green","yellow","gold"),
                  bref   =1,fref=1){
    
       invisible(kobePhaseFn(object,xlim,ylim,quadcol=quadcol,bref=bref,fref=fref))})

