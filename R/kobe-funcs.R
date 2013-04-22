utils::globalVariables(c("xFac","yFac"))

ac=as.character

getExt <- function(file)
  tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))


## Calculates frequency of points in squares
kobeFreq=function(x,y,x.n=11,y.n=x.n,na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
  
  df=data.frame(x=x,y=y)
  df=data.frame(df,xFac=cut(df$x,seq(min(df$x),max(df$x),length.out=x.n)),
                yFac=cut(df$y,seq(min(df$y),max(df$y),length.out=y.n)))
  
  c.=ddply(data.frame(df,count=1),.(xFac,yFac), function(x) count(x$count))[,c("xFac","yFac","freq")]
  
  p.=merge(df,c.,by=c("xFac","yFac"))[,c("x","y","freq","xFac","yFac")]
  
  return(p.[order(p.$freq),])}

## calculates density of points
kobeDens=function(x,y,n=11,na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
  
  
  dat=data.frame(x=x,y=y,n=n)
  f1 =with(dat, kde2d(x,y,n=n)) 
  f2 =data.frame(expand.grid(x=f1$x, y=f1$y), z=as.vector(f1$z))
  
  return(f2)}

## calculates 2D probabilities
kobeProb=function(x,y,prob=c(0.5, 0.75,0.95),na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
    
  tmp=HPDregionplot(mcmc(data.frame(x,y)),prob=prob)
  
  
  prb=ldply(tmp, function(dat) data.frame(level=dat$level,x=dat$x, y=dat$y))
  
  return(prb)}

setMethod('kobe',  signature(object="data.frame",method="missing"), 
          function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){ 
            kobeFn(object,what=what,prob=prob,year=year,nwrms=nwrms)})

kobeFn=function(object,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10){         
            
            trks. =NULL
            pts.  =NULL
            smry. =NULL
            wrms. =NULL
            sims. =NULL
           
            ## trks
            if ("trks" %in% what){
              
              trks.=rbind(ddply(object,.(year), function(x) data.frame(quantity="stock",  pctl=prob,value=quantile(x$stock,    prob, na.rm=T))),
                          ddply(object,.(year), function(x) data.frame(quantity="harvest",pctl=prob,value=quantile(x$harvest,  prob, na.rm=T))))

             trks.=transform(trks.,pctl=paste(substr(ac(signif(pctl,2)),3,nchar(ac(signif(pctl,2)))),ifelse(nchar(ac(trks.$pctl))==3,"0",""),"%",sep=""))
             trks.=cast(trks.,year+pctl~quantity,value="value") 
              }
            
            if ("pts" %in% what & !is.null(year))
              pts. =object[object$year==year,]
            
            
            if ("smry" %in% what)
              smry. =ddply(kobeP(sims), .(year), function(x) data.frame(stock      =median(stock(object),       na.rm=T),
                                                                        harvest    =median(harvest(object),     na.rm=T),
                                                                        red        =mean(  x$red,         na.rm=T),
                                                                        yellow     =mean(  x$yellow,      na.rm=T),
                                                                        green      =mean(  x$green,       na.rm=T),
                                                                        overFished =mean(  x$overFished,  na.rm=T),
                                                                        overFishing=mean(  x$overFishing, na.rm=T)))
            if ("wrms" %in% what){          
              wrms =sample(unique(res$iter),nwrms)
              wrms.=sims[sims$iter %in% wrms,]
            }
            
            if ("sims" %in% what)     
              sims. =object
            
            res=list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.)
            
            res}



#Utility functions for summarising time series to create performance measures

#' iav
#' @description 
#' Calculates the inter-annual variation in a time series, i.e. \code{(x[t+1]-x[t])/x[t]}
#' Used to show how variable a quantity like yield 
#' is under different management strategies within a Management Strategy Evaluation.
#'      
#' @aliases 
#' iav
#' 
#' @param x; a vector holding a time series
#' @return a \code{vector} with the inter-annual variation each time step
#' @export
#' @docType functions
#' @rdname utils
#' 
#' @examples
#'    x=rnorm(2)
#'    iav(x)
#'    ## inter-annual average variation
#'    mean(iav(x),na.rm=T)
iav=function(x) if (length(x)==1) return(NA) else c(NA,(x[-1]-x[-length(x)])/x[-length(x)])


#' incr
#' @description 
#' Is a quantity increasing from 1 time step to another \code{(x[t+1]-x[t])>0} 
#'      
#' @aliases 
#' incr
#' 
#' @param x; a vector holding a time series
#' @return a \code{logical} indicating an increase
#' @export
#' @docType functions
#' @rdname utils
#' 
#' @examples
#'    x=rnorm(2)
#'    incr(x)
incr=function(x) c(NA,(x[-1]-x[-length(x)]>0))

#' incr
#' @description 
#' Is a quantity increasing from 1 time step to another \code{(x[t+1]-x[t])>0} 
#'      
#' @aliases 
#' incr
#' 
#' @param x; a vector holding a time series
#' @return a \code{logical} indicating an increase
#' @export
#' @docType functions
#' @rdname utils
#' 
#' @examples
#'    x=rnorm(2)
#'    incr(x)
#Discount rate is chosen which reflects the risk (the higher the risk the higher the
#discount rate) and this is used to discount all forecast future cash flows to calculate a present value:
#PV = (CF1)/(1+r) + (CF2)/((1+r)2) + (CF3)/((1+r)3) ?????????
dRate=function(x,r) x/(1+r)^(0:(length(x)-1))

#' incr
#' @description 
#' Is a quantity increasing from 1 time step to another \code{(x[t+1]-x[t])>0} 
#'      
#' @aliases 
#' incr
#' 
#' @param x; a vector holding a time series
#' @return a \code{logical} indicating an increase
#' @export
#' @docType functions
#' @rdname utils
#' 
#' @examples
#'    x=rnorm(2)
#'    incr(x)
#Break point for segmented regression for a given steepness and virgin biomass
hinge=function(s,v,rec){
  #slope
  x=c(0,v*0.2)
  y=c(0,rec*s)
  
  #y=a+b*x
  #y1=a+b*x1
  #y2=a+b*x2
  #y1-y2=b*(x1-x2)
  
  b=(y[1]-y[2])/(x[1]-x[2])
  a=b*x[1]-y[1]
  
  (rec-a)/b}

#' recovered
#' @description 
#' Has the stock recovered yet? i.e. stock>=1 and harvest<=1 in the current or an earlier time step.
#' In other words has it been in the green Kobe quadrant.
#'      
#' @aliases 
#' recovered
#' 
#' @param x; a vector holding a time series
#' @return a \code{logical} indicating an recovered
#' @export
#' @docType functions
#' @rdname utils
#' 
#' @examples
#'    harvest=rlnorm(20)
#'    stock  =rlnorm(20)
#'    recovered(stock,harvest)
recovered=function(stock,harvest) {
  
  stock  =pmin(floor(stock),1)
  harvest=1-pmin(floor(harvest),1)
  
  as.logical(cumsum(harvest*stock))}
  

