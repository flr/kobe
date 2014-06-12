########################################################################################
#### bdm stuff for Kobe ################################################################
########################################################################################

#### exported function
#### for objects of class "bdm"
kobeBdm=function(object,prb=NULL,dir="",
                   what=c("sims","trks","pts","smry","wrms")[1],
                   prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
    
	if (dir!="") cat("dir is ignored\n")
	
    res=ioBdm(object,what=what,prob=prob,nwrms=nwrms,year=year)
    
    if (length(what)==1)
       return(res[[what]])
    else
       return(res[what]) }

#### read MCMC stanfit output and return data.frame
#### with headers: iter,year,stock,harvest,bmsy,fmsy 
readBdm=function(object){
  
  years <- object@data$year
  niter <- object@nsamples
  iters <- 1:niter
  
  res <- object@trace$biomass
  dimnames(res) <- list(iter=iters,year=years)
  
  res <- melt(res,value.name='stock')
  res <- data.frame(res,harvest=melt(object@trace$harvest_rate)$value)
  
  bmsy <- object@trace$biomass_at_msy
  fmsy <- object@trace$harvest_rate_at_msy
  
  ref <- data.frame(iter=iters,bmsy=bmsy,fmsy=fmsy)
  
  res <- merge(res,ref,by="iter")
  
  res$stock   <- res$stock/res$bmsy
  res$harvest <- res$harvest/res$fmsy
  
  if(length(object@run)>0) {
    run <- object@run
    res <- data.frame(run=run,res)
  }

  return(res)}

## Heavy lifting functions 
ioBdm=function(object,prob,what,year,nwrms){
    
    if (!all(what %in% c("trks","pts","smry","wrms","sims"))) stop("what not in valid options")
    
    if (class(object) %in% "bdm")
       res=readBdm(object,run) else stop("object not of class bdm")
    
    if (is.null(year)) pts=max(object@data$year)
      
    trks. =NULL
    pts.  =NULL
    smry. =NULL
    wrms. =NULL
    sims. =NULL
        
    if ("trks" %in% what){ 
      stock  =ddply(res,.(year),function(x) quantile(x$stock,    prob, na.rm=TRUE))
      harvest=ddply(res,.(year),function(x) quantile(x$harvest,  prob, na.rm=TRUE))
      trks.=data.frame(melt(stock,id.vars="year"),"harvest"=melt(harvest,id.vars="year")[,3])
      names(trks.)[c(2,3)]=c("Percentile","stock")}
    
    if ("pts" %in% what)
      pts.=res[res$year %in% pts,]
    
    if ("sims" %in% what)
      sims.=res
    
    if ("smry" %in% what)
       smry. =ddply(data.frame(res,kobeP(as.numeric(res$stock),as.numeric(res$harvest))),
                           .(year), function(x) data.frame(stock      =median(x$stock,       na.rm=TRUE),
                                                           harvest    =median(x$harvest,     na.rm=TRUE),
                                                           red        =mean(  x$red,         na.rm=TRUE),
                                                           yellow     =mean(  x$yellow,      na.rm=TRUE),
                                                           green      =mean(  x$green,       na.rm=TRUE),
                                                           overFished =mean(  x$overFished,  na.rm=TRUE),
                                                           overFishing=mean(  x$overFishing, na.rm=TRUE)))
    
    if ("wrms" %in% what)
      wrms.=res[res$iter %in% sample(unique(res$iter),nwrms),c("iter","year","stock","harvest")]
    
    return(list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.))}
