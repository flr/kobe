########################################################################################
#### aspic stuff for Kobe ##############################################################
########################################################################################

#### exported function
kobeBsp=function(f,b,dir="",what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,.25),year=NULL,nwrms=10,minyear=0){
    
  getBSP=function(fileF,fileB,firstyear=0){
    resF=transform(melt(read.csv(fileF,header=F)),year=as.numeric(variable),
                   harvest=value)[,-(1:2)]
    resB=transform(melt(read.csv(fileB,header=F)),year=as.numeric(variable),
                   stock=value)[,-(1:2)]
    res=cbind(resF,stock=resB$stock)
    
    res$year=res$year-min(res$year)+firstyear
    return(res)}
  
  res=getBSP(f,b,minyear)
  
  res=ioBSP(res,what=what,prob=prob,year=year,nwrms=nwrms)
    
#   if (length(prb)>1){
#     res=mlply(prb, function(x,bio,prob=prob,nwrms=nwrms,what=what)
#       ioAspic(bio=bio,prb=x,prob=prob,nwrms=nwrms,what=what),
#               bio=bio,prob=prob,nwrms=nwrms,what=what)
#     
#     res=list(trks=ldply(res, function(x) x$trks),
#              pts =ldply(res, function(x) x$pts),
#              smry=ldply(res, function(x) x$smry),
#              wrms=ldply(res, function(x) x$wrms),
#              sims=ldply(res, function(x) x$sims))
#  }
  
  if (length(what)==1)
    return(res[[what]])
  else
    return(res[what]) }


## Heavy lifting functions ##############################################################
ioBSP=function(res,what=c("sims","trks","pts","smry","wrms")[1],prob=c(0.75,0.5,0.25),year=NULL,nwrms=10){
  
  if (!all(what %in% c("trks","pts","smry","wrms","sims"))) stop("what not in valid options")
  
  trks. =NULL
  pts.  =NULL
  smry. =NULL
  wrms. =NULL
  sims. =NULL
  
  if ("trks" %in% what){ 
    stock  =ddply(res,.(year),function(x) quantile(x$stock,    prob, na.rm=T))
    harvest=ddply(res,.(year),function(x) quantile(x$harvest,  prob, na.rm=T))
    trks.=data.frame(melt(stock,id.vars="year"),harvest=melt(harvest,id.vars="year")[,3])
    names(trks.)[c(2,3)]=c("Percentile","stock")}
  
  if ("pts" %in% what & !is.null(year))
    pts.=res[res$year %in% year,]
  
  if ("sims" %in% what)
    sims.=res
  
  if ("smry" %in% what)
    smry. =ddply(data.frame(res,kobeP(res$stock,res$harvest)),
                 .(year), function(x) data.frame(stock      =median(x$stock,       na.rm=T),
                                                 harvest    =median(x$harvest,     na.rm=T),
                                                 red        =mean(  x$red,         na.rm=T),
                                                 yellow     =mean(  x$yellow,      na.rm=T),
                                                 green      =mean(  x$green,       na.rm=T),
                                                 overFished =mean(  x$overFished,  na.rm=T),
                                                 overFishing=mean(  x$overFishing, na.rm=T)))
  
  #if ("wrms" %in% what)
  #  wrms.=res[res$iter %in% sample(unique(res$iter),nwrms),c("iter","year","ssb","harvest")]
  
  res=list(trks=trks.,pts=pts.,smry=smry.,wrms=wrms.,sims=sims.)
  
  return(res)}  
