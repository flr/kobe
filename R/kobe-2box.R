utils::globalVariables(c("ddply",".","melt","grid.newpage","pushViewport","viewport",
                          "grid.layout","mlply","ldply"))


# #######################################################################################
# ### VPA2Box stuff for Kobe ############################################################
# #######################################################################################
#' @title kobe2box
#' 
#' @name kobe2box
#' 
#' @description Creates summary data.frame for presenting advice using VPA2Box projections
#' 
#' @param path \code{character} directory or directories with target files 
#' @param proxy \code{character} proxy for msy "f0.1" by default otherwise one of c("fmsy","fmax","f0.1","f20","f30","f40","f90max","f75max") 
#' @param what \code{character} output data.frame all "sims" by default or one of c("sims","trks","pts","smry","wrms")[1] 
#' @param prob \code{numeric} with probabilities, by default c(0.75,0.5,0.25)
#' @param year \code{numeric} for year when slecting points
#' @param nwrms \code{numeric} number of worms to select by default 10 
#' 
#' @return a \code{data.frame} or a list of \code{data.frame} with either all the simulations
#' from PRO2BOX, i.e. for both historical assessment results and projections. These can either
#' be in the form of summaries or all reults
#' 
#' @aliases kobe2box-method kobe2box,character-method
#' @export
#' @rdname kobe2box
#' 
#' 
#' @examples
#' \dontrun{
#' sims=kobe2box("vpa")
#' } 
setMethod('kobe2box', signature(path='character'),
          function(path,proxy=c("fmsy","fmax","f0.1","f20","f30","f40","f90max","f75max")[3], 
                   what=c("sims","trks","pts","smry","wrms")[1],
                   prob=c(0.75,0.5,0.25),year=NULL,nwrms=10){
            
            if (length(path)==1)
              res=io2box(path,proxy=proxy,what=what,prob=prob,nwrms=nwrms,year=year)
            else{
              res=mlply(path, function(x,proxy=proxy,what=what,prob=prob,nwrms=nwrms,year=year)
                io2box(x,proxy=proxy,what=what,prob=prob,nwrms=nwrms,year=year),
                proxy=proxy,what=what,prob=prob,nwrms=nwrms,year=year)
              
              res=list(trks=ldply(res, function(x) x$trks),
                       pts =ldply(res, function(x) x$pts),
                       smry=ldply(res, function(x) x$smry),
                       wrms=ldply(res, function(x) x$wrms),
                       sims=ldply(res, function(x) x$sims))}
            
            if (length(what)==1) 
              return(res[[what]]) 
            else 
              return(res[what])
  })

utils::globalVariables(c("stock","harvest","year","tac"))

nmsRef <- c("iter", 
            "fmsy",   "ymsy",   "yrmsy",    "srmsy",   "sprmsy",  "ssbmsy",
            "fmax",             "yrmax",    "srmax",   "sprmax",  "ssbmax",
            "f0.1",             "yr0.1",    "sr0.1",   "spr0.1",  "ssb0.1",
            "f20",              "yr20",     "sr20",               "ssb20",
            "f30",              "yr30",     "sr30",               "ssb30",
            "f40",              "yr40",     "sr40",               "ssb40",
            "f90max", "y90max", "yr90max",  "sr90max",            "ssb90max",
            "f75max", "y75max", "yr75max",  "sr75max",            "ssb75max")

nmsRef2<- c("iter", 
            "fmsy",   "ymsy",   "yrmsy",    "srmsy",   "sprmsy",  "ssbmsy",
            "fmax",   "ymax",   "yrmax",    "srmax",   "sprmax",  "ssbmax",
            "f0.1",   "y0.1",   "yr0.1",    "sr0.1",   "spr0.1",  "ssb0.1",
            "f20",    "y20",    "yr20",     "sr20",               "ssb20",
            "f30",    "y30",    "yr30",     "sr30",               "ssb30",
            "f40",    "y40",    "yr40",     "sr40",               "ssb40",
            "f90max", "y90max", "yr90max",  "sr90max",            "ssb90max",
            "f75max", "y75max", "yr75max",  "sr75max",            "ssb75max")

## Heavy lifting functions ##############################################################
read2box=function(path,proxy=c("fmsy","fmax","f0.1","f20","f30","f40","f90max","f75max")[3]){

  bFile=file.path(path,"BENCH-1.OUT")
  
  if (!file.exists(bFile))
    bFile=file.path(path,"BENCH.OUT")
  
  rfp=read.table(bFile,header=F,skip=1)
  
  if(dim(rfp)[2]==39) nms=nmsRef else nms=nmsRef2
  
  rfp=read.table(bFile,header=F,skip=1,col.names=nms)[,c("iter",proxy,paste("ssb",substr(proxy,2,nchar(proxy)),sep=""))]

  if(dim(rfp)[2]==39) ssbFile=file.path(path,"SSBIO-1.OUT") else ssbFile=file.path(path,"SStot.OUT")
  
  bio=read.table(ssbFile,header=F,skip=0)
  names(bio)=c("tac","iter",seq(dim(bio)[2]-2))
  bio=merge(bio,rfp)
  bio=melt(bio,id.vars=c("tac",names(rfp)),variable_name="year")
  names(bio)[6]="stock"
  
  if(dim(rfp)[2]==39) fFile=file.path(path,"Fapex-1.OUT") else fFile=file.path(path,"Fapex.OUT")
  
  hvt=read.table(fFile,header=F,skip=0)
  names(hvt)=c("tac","iter",seq(dim(hvt)[2]-2))
  hvt=merge(hvt,rfp)
  hvt=melt(hvt,id.vars=c("tac",names(rfp)),variable_name="year")
  names(hvt)[6]="harvest"
  
  res=cbind(bio,"harvest"=hvt[,"harvest"])
  res=transform(res,stock  =stock/res[,paste("ssb",substr(proxy,2,nchar(proxy)),sep="")],
                    harvest=harvest/res[,proxy],
                    year   =as.numeric(as.character(year)))
  
  res=res[do.call(order,res[,c("iter","tac","year")]),]
  
  dimnames(res)[[1]]=sort(as.numeric(dimnames(res)[[1]]))
    
  return(res)}

io2box=function(x,proxy=c("fmsy","fmax","f0.1","f20","f30","f40","f90max","f75max")[3],
                prob=c(0.75,0.5,.25),what=c("sims","trks","pts","smry","wrms")[1],nwrms=10,year=NULL){
  
  if (!all(what %in% c("trks","pts","smry","wrms","sims"))) stop("what not in valid options")
  
  res=read2box(x,proxy)

  sims.=NULL
  trks.=NULL
  pts. =NULL
  smry.=NULL
  wrms.=NULL
   
  if ("sims" %in% what)
      sims.=res

   if ("trks" %in% what){ 
       stock=ddply(res,.(year,tac),function(x) quantile(x$stock,  prob))
       hvt  =ddply(res,.(year,tac),function(x) quantile(x$harvest,prob))
       trks.=data.frame(melt(stock,id.vars=c("year","tac")),harvest=melt(hvt,id.vars=c("year","tac"))[,4])
       names(trks.)[3:4]=c("Percentile","stock")
       }

   if ("pts" %in% what){
     if (is.null(year)) year=max(as.numeric(as.character(res$year)))
        pts.=res[res$year %in% year,]}
           
   if ("smry" %in% what)
       smry.   =ddply(res,  .(year,tac), function(x){ 
                               x=smry(x)
                               data.frame(stock      =median(x$stock,     na.rm=TRUE),
                                          harvest    =median(x$harvest,   na.rm=TRUE),
                                          red        =mean(x$red,         na.rm=TRUE),
                                          yellow     =mean(x$yellow,      na.rm=TRUE),
                                          green      =mean(x$green,       na.rm=TRUE),
                                          overFished =mean(x$overFished,  na.rm=TRUE),
                                          overFishing=mean(x$overFishing, na.rm=TRUE))})
    
   if ("wrms" %in% what)
       wrms.=res[res$iter %in% sample(unique(res$iter),nwrms),c("iter","year","tac","stock","harvest")]
 
  
  return(list(sims=sims.,
              trks=trks.,
              pts =pts.,
              smry=smry.,
              wrms=wrms.))}

