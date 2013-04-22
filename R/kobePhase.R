utils::globalVariables(c("x","y","fill"))

### provide a back drop on which to overlay data
kobePhaseFn=function(object,xlim,ylim){    
  quads<- rbind(data.frame(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf), fill=as.factor("yellow")),
                data.frame(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf), fill=as.factor("green")),
                data.frame(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1), fill=as.factor("red")))
  
  p=ggplot(object)+geom_polygon(data=quads,aes(x,y,fill=fill)) +
    scale_fill_manual(values = c("yellow","green","red"), guide="none") +
    ylab(expression(F/F[MSY]))        +
    xlab(expression(SSB/B[MSY]))      +
    scale_y_continuous(limits=ylim)   +
    scale_x_continuous(limits=xlim)
  
  invisible(p)}

setMethod('kobePhase', signature(object='missing'),
  function(object,xlim=c(0,2),ylim=xlim){
    
       invisible(kobePhaseFn(NULL,xlim,ylim))})

setMethod('kobePhase', signature(object='data.frame'),
  function(object,xlim=c(0,ceiling(2*max(object$stock))/2),
                  ylim=c(0,ceiling(2*max(object$harvest))/2)){
    
       invisible(kobePhaseFn(object,xlim,ylim))})

