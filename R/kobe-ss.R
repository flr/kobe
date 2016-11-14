# #######################################################################################
# ### SS stuff for Kobe #################################################################
# #######################################################################################

utils::globalVariables(c("yrs","pts"))

#setMethod('kobeSS', signature(object='character'),
kobeSS=function(object,nrows=-1,thin=1,what=c("sims","trks","pts","smry","wrms")[1],
                        prob=c(0.75,0.5,0.25),year=NULL,nwrms=10){

    yrs =year
    year=max(year)
    #require(LaF)
    if (any(length(grep("derived_posteriors.sso",object))<1)) 
        stop("file needs to be called 'derived_posteriors.sso'")

    if (length(object)==1)
       res=ioSS(object,what=what,prob=prob,yrs=yrs,year=year,nrows=nrows,nwrms=nwrms,thin=thin)
    
    if (length(object) >1){
       res=mlply(object, function(x,prob=prob,yrs=yrs,year=year,nrows=nrows,nwrms=nwrms,thin=thin,what=what)
                             ioSS(x,prob=prob,yrs=yrs,year=year,nrows=nrows,nwrms=nwrms,thin=thin,what=what),
                                    prob=prob,yrs=yrs,year=year,nrows=nrows,nwrms=nwrms,thin=thin,what=what)
                 
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
ioSS=function(x,prob=c(0.75,0.5,0.25),yrs=NULL,year=NULL,nwrms=10,
              what=c("sims","trks","pts","smry","wrms"),nrows=-1,thin=1){
 
    if (is.null(yrs)){
       nms=names(read.csv(x,sep=" ",nrows=1))
       yrs=nms[substr(nms,1,3)=="Bra"]
       yrs=as.numeric(substr(yrs,8,nchar(yrs)))}

    if (is.null(year)){
       nms=names(read.csv(x,sep=" ",nrows=1))
       pts=nms[substr(nms,1,3)=="For"]
       pts=min(as.numeric(substr(pts,11,nchar(pts))))-1
       }

    Fs =paste("F",     yrs,sep="_")
    Bs =paste("Bratio",yrs,sep="_")

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
    res =res[,c("Iter",Bs,Fs,"Fstd_MSY")]

    #res=data.frame(apply(read.csv(x,sep=" ",nrows=nrows)[,c("Iter",Bs,Fs,"Fstd_MSY")],2, function(x) as.numeric(as.character(x))))
    res=res[seq(1,dim(res)[1],thin),]
    res=melt(res[,c("Iter",Bs,Fs)],id.vars="Iter")
    
    res$year=as.numeric(gsub("Bratio_","",as.character((res$variable))))
 options(ops)    
    res$year[is.na(res$year)]=as.numeric(gsub("F_","",as.character(res[is.na(res$year),"variable"])))
    res$var=substr(res$variable,1,1)
    res    =data.frame(res[res$var=="B",c("Iter","year","value")],harvest=res[res$var=="F","value"])
    names(res)[c(1,3)]=c("iter","stock")
    res    =data.frame(res, prob(res$stock,res$harvest))
    res[is.na(res)]=0
   
    sims=NULL
    trks=NULL
    pts.=NULL
    wrms=NULL
    smry=NULL
    
    if ("sims" %in% what)
      sims=res
    
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

readSSBoot<-function(object){
  
  object=subset(object,object$Label%in%c("B_Bmsy","F_Fmsy","SSB_MSY","Fstd_MSY"))
  names(object)[5001:5002]=c("quantity","year")
  object=subset(object,)
  object=melt(object,id=c("quantity","year"))
  
  tmp=as.data.frame(t(array(unlist(strsplit(object[,1], "_")),dim=c(2,dim(object)[1]))))
  tac=as.data.frame(t(array(unlist(strsplit(as.character(object[,3]), "_")),dim=c(3,dim(object)[1]))))[,-1]
  tac=transform(tac,iter=V2,
                tac=1000*as.numeric(substr(as.character(V3),2,nchar(as.character(V3)))))
  object=cbind(tmp,tac,object)
  
  bmsy=subset(object,(V1%in%"SSB" &(V2%in%"MSY")))[,c("iter","tac","value")]
  fmsy=subset(object,(V1%in%"Fstd"&(V2%in%"MSY")))[,c("iter","tac","value")]
  rs1=subset(object,(V2%in%"Fmsy"&V1%in%"F"))[,     c("year","iter","tac","value")]
  rs2=subset(object,(V2%in%"Bmsy"&V1%in%"B"))[,     c("year","iter","tac","value")]
  res=cbind(rs1[,1:3],stock=rs2[,"value"],harvest=rs1[,"value"])
  
  res=merge(res,bmsy)
  names(res)[6]="bmsy"
  res=merge(res,fmsy)
  names(res)[7]="fmsy"
  
  res}

readSSCovar<-function(file){
  
  res=read.csv(file,stringsAsFactors=FALSE,
               skip=6,header=FALSE,sep="")
  
  names(res)=c("xActive","yActive","xAll","yAll","xType","yType","x","y","data")
  res[,-(1:4)]}

readSSCovarRefs<-function(file){
  refs=c("SPB_Virgin","SPB_Initial","SSB_Unfished",             "Bzero_again",
         "TotBio_Unfished",         "SmryBio_Unfished",         "Recr_Unfished",           
         "SSB_Btgt",                "SPR_Btgt",                 "Fstd_Btgt",               
         "TotYield_Btgt",           "SSB_SPRtgt",               "Fstd_SPRtgt",             
         "TotYield_SPRtgt",         "SSB_MSY",                  "SPR_MSY",                 
         "Fstd_MSY",                "TotYield_MSY",             "RetYield_MSY")
  
  res=readSSCovar(file)
  res=subset(res,x%in%refs&y=="_")[,c("x","data")]
  
  res}

readSSCovarSeries<-function(file,var=c("SPB","Recr","F","SPRratio","Bratio","Main_RecrDev")[1:3]){    
  
  #unique(subset(tmp,y=="_"&substr(x,nchar(x)-3,nchar(x))=="1999")$x)
  
  #SPR_ratio_basis: (1-SPR)/(1-SPR_40%)
  #F_report_basis: (F)/(Fmsy);_with_F=sum(full_Fs)
  #B_ratio_denominator: 40%*Virgin_Biomass
    
  
  res=readSSCovar(file)
  
  rtn=mdply(var,function(v){
    rs2=subset(res,substr(x,1,nchar(v)+1)==paste(v,"_",sep="")&y=="_")[,c("x","data")]
    
    wrn=options()$warn
    options(warn=-1)
    rs2=transform(rs2,year=as.numeric(substr(x,nchar(v)+2,nchar(x))))[,c("year","data")]
    options(warn=wrn)
    subset(rs2,!is.na(year))})
  
  transform(rtn,qname=var[X1])[,c("qname","year","data")]}

readSSCovarV2<-function(file,var){
  
  var=paste(var,"_",sep="")
  
  res=read.csv(file,stringsAsFactors=FALSE,
               skip=6,header=FALSE,sep="")
  
  cov=subset(res,substr(x,1,4)==var&substr(y,1,length(var))==var)[,c("x","y","data")]
  cov=as.matrix(cast(cov,x~y),dim(hat)[1],dim(hat)[1])
  cov[upper.tri(cov)]=t(cov[lower.tri(cov)])
  
  cov[upper.tri(cov)]=NA
  t.=t(cov)
  t.[lower.tri(cov)]=cov[lower.tri(cov)]
  diag(t.)=1
  cov=t.[-(dim(t.)[1]-0:1),-(dim(t.)[1]-0:1)]
  
  hat=subset(res,substr(x,1,4)==var&substr(y,1,1)=="_")[-(1:2),c("x","data")]
  
  list(hat=hat,cov=cov)}

# system("sed  's/SR_LN(R0)/r0_NA/g'  a.txt>b.txt")
#system("rename 's/^-/_/' /home/laurie/Desktop/Dropbox/scrsPapers/scrs-2016-014/inputs/covar/*")

tmp=t(matrix(c(
"SSB_Unfished","Unfished reproductive potential (SSB is commonly female mature spawning biomass)",
"TotBio_Unfished","Total age 0+ biomass on Jan 1",
"SmryBio_Unfished","Biomass for ages at or above the summary age on Jan 1",
"Recr_Unfished","Unfished recruitment",
"SSB_Btgt","SSB at user specified SSB target",
"SPR_Btgt","Spawner potential ratio (SPR) at F intensity that produces user specified SSB target",
"Fstd_Btgt","F statistic at F intensity that produces user specified SSB target",
"TotYield_Btgt","Total yield at F intensity that produces user specified SSB target",
"SSB_SPRtgt","SSB at user specified SPR target (but taking into account the spawner-recruitment relationship)",
"Fstd_SPRtgt","F intensity that produces user specified SPR target",
"TotYield_SPRtgt","Total yield at F intensity that produces user specified SPR target",
"SSB_MSY","SSB at F intensity that is associated with MSY; this F intensity may be directly calculated to produce MSY, or can be mapped to F_SPR or F_Btgt",
"SPR_MSY","Spawner potential ratio (SPR) at F intensity associated with MSY",
"Fstd_MSY","F statistic at F intensity associated with MSY",
"TotYield_MSY","Total yield (biomass) at MSY",
"RetYield_MSY","Retained yield (biomass) at MSY"),2,16))

refNames=tmp[,2]
names(refNames)<-tmp[,1]

rm(tmp)

