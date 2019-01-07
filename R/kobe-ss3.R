# #######################################################################################
# ### SS3 stuff for Kobe ################################################################
# #######################################################################################

utils::globalVariables(c("yrs","yrs","pts"))

#setMethod('kobeSS3', signature(object='character'),
kobeSS3=function(object,nrows=-1,thin=1,what=c("sims","trks","pts","smry","wrms")[1],
                        prob=c(0.75,0.5,0.25),pts=NULL,yrs=NULL,nwrms=10,biomass="bratio"){

    #require(LaF)
    #if (any(length(grep("derived_posteriors.sso",object))<1)) 
    #    stop("file needs to be called 'derived_posteriors.sso'")

    if (length(object)==1)
       res=ioSS3(object,what=what,prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,biomass=biomass)
    
    if (length(object) >1){
       res=mlply(object, function(x,prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,what=what)
                                   ioSS3(x,prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,what=what,biomass=biomass),
                      prob=prob,yrs=yrs,pts=pts,nrows=nrows,nwrms=nwrms,thin=thin,what=what,biomass=biomass)
                 
       res=list(trks=ldply(res, function(x) x$trks),
                pts =ldply(res, function(x) x$pts),
                smry=ldply(res, function(x) x$smry),
                wrms=ldply(res, function(x) x$wrms),
                sims=ldply(res, function(x) x$sims))
    }
               
    if (length(what)==1)
      return(res[[what]])
    else
      return(res[what]) }

## Heavy lifting functions ##############################################################
ioSS3=function(x,prob=c(0.75,0.5,0.25),yrs=NULL,pts=NULL,nwrms=10,
               what=c("sims","trks","pts","smry","wrms"),nrows=-1,thin=1,biomass="bratio",
               refs=c("SSB_unfished",      "Totbio_unfished", "SmryBio_unfished", "Recr_unfished",     
                      "SSB_F01",           "SPR_F01",         "Fstd_F01",         "Dead_Catch_F01",    
                      "SSB_SPR",           "Fstd_SPR",        "Dead_Catch_SPR",   "SSB_MSY",           
                      "SPR_MSY",           "Fstd_MSY",        "Dead_Catch_MSY",   "Ret_Catch_MSY",     
                      "B_MSY.SSB_unfished")[c(12,14)]){
 
    if (is.null(yrs)){
       nms=names(read.csv(x,sep=" ",nrows=1))
       yrs=nms[substr(nms,1,3)=="Bra"]
       yrs=as.numeric(substr(yrs,8,nchar(yrs)))}

    if (is.null(pts)){
       nms=names(read.csv(x,sep=" ",nrows=1))
       pts=nms[substr(nms,1,3)=="For"]
       pts=min(as.numeric(substr(pts,11,nchar(pts))))-1
       }

    Fs  =paste("F",     yrs,sep="_")
    Bs  =paste("Bratio",yrs,sep="_")
    SSBs=paste("SSB",   yrs,sep="_")
    
 
    ops=options()
    options(warn=-1)
    hd=names(read.csv(x,sep=" ",nrows=1,header=T))

#     dat = laf_open_csv(filename=x,column_types=rep("double",length(hd)),
#                        column_names=hd,
#                        sep=" ",
#                        skip=2)
#     
    res =read.csv(x,sep=" ")
    
    dat = read.csv(file=x,colClasses=rep("double",length(hd)),
                       col.names=hd,
                       sep=" ",
                       skip=2)
  
    res <- dat[ ,]
    rfs<<-res[,c("Iter",refs)]
    names(rfs)[1]="iter"

    if (biomass=="bratio")
      res =res[,c("Iter",Bs,Fs)]
    else  
      res =res[,c("Iter",SSBs,Fs)]
    
    #res=data.frame(apply(read.csv(x,sep=" ",nrows=nrows)[,c("Iter",Bs,Fs,"Fstd_MSY")],2, function(x) as.numeric(as.character(x))))
    res=res[seq(1,dim(res)[1],thin),]
    
    if (biomass=="bratio"){
      res=melt(res[,c("Iter",Bs,Fs)],id.vars="Iter")
      res$year=as.numeric(gsub("Bratio_","",as.character((res$variable))))
      res$year[is.na(res$year)]=as.numeric(gsub("Bratio","",as.character(res[is.na(res$year),"variable"])))
    }else{  
      res=melt(res[,c("Iter",SSBs,Fs)],id.vars="Iter")
      res$year=as.numeric(gsub("SSB_","",as.character((res$variable))))
      res$year[is.na(res$year)]=as.numeric(gsub("SSB_","",as.character(res[is.na(res$year),"variable"])))
      }
    res$var=substr(res$variable,1,2)

    options(ops)    
    if (biomass=="bratio")
      res    =data.frame(res[res$var=="Br",  c("Iter","year","value")],harvest=res[res$var=="F_","value"])
    else  
      res    =data.frame(res[res$var=="SS",  c("Iter","year","value")],harvest=res[res$var=="F_","value"])
    names(res)[c(1,3)]=c("iter","stock")
    res[is.na(res)]=0

    sims=NULL
    trks=NULL
    pts.=NULL
    wrms=NULL
    smry=NULL
    
    if ("sims" %in% what)
      sims=merge(res,rfs,by="iter")
    
    if ("trks" %in% what){ 
      stock =ddply(res,.(year),function(x) quantile(x$stock,    prob))
      hvt =ddply(res,.(year),function(x) quantile(x$harvest,prob))
      trks=data.frame(melt(stock,id.vars="year"),harvest=melt(hvt,id.vars="year")[,3])
      names(trks)[c(2,3)]=c("Percentile","stock")}

    if ("pts" %in% what & !is.null(yrs))
       pts.=res[res$year %in% yrs,]
    
    if ("smry" %in% what)
       smry   =ddply(res,  .(year), function(x) data.frame(stock      =median(x$stock,     na.rm=TRUE),
                                                           harvest    =median(x$harvest,   na.rm=TRUE),
                                                           red        =mean(x$red,         na.rm=TRUE),
                                                           yellow     =mean(x$yellow,      na.rm=TRUE),
                                                           green      =mean(x$green,       na.rm=TRUE),
                                                           overFished =mean(x$overFished,  na.rm=TRUE),
                                                           overFishing=mean(x$overFishing, na.rm=TRUE)))
    
    if ("wrms" %in% what)
       wrms=res[res$iter %in% sample(unique(res$iter),nwrms),c("iter","year","stock","harvest")]
    
    return(list(trks=trks,pts=pts.,smry=smry,wrms=wrms,sims=sims))}

