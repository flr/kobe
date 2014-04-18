### R code from vignette source 'kobe.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: prelim
###################################################
library(kobe)

kobe.version <- packageDescription("kobe")$Version
kobe.date <- packageDescription("kobe")$Date
now.date <- strftime(Sys.Date(), "%B %d, %Y")

library(RCurl	)

data(prj1)
data(prj)
data(assmt)

TACs=seq(15000,35000,5000)


###################################################
### code chunk number 2: kobe.Rnw:150-153 (eval = FALSE)
###################################################
## ### Results from ASPIC bootstrapped assessment
## bio   ="http://www.iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspic.bio"
## assmt =kobeAspic(bio)


###################################################
### code chunk number 3: kobe.Rnw:155-156
###################################################
head(assmt)


###################################################
### code chunk number 4: kobe.Rnw:161-164 (eval = FALSE)
###################################################
## ## Results from an ASPIC Projection
## prb  ="http://www.iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspic_15000.prb"
## prj1 =kobeAspic(bio,prb)


###################################################
### code chunk number 5: kobe.Rnw:167-168
###################################################
tail(prj1)


###################################################
### code chunk number 6: kobe.Rnw:182-186 (eval = FALSE)
###################################################
## ## Projections
## TACs=seq(15000,35000,5000)
## prb ="http://www.iccat.int/stocka/Models/ASPIC/albs/2011/run2/aspic_"
## prb =paste(prb,TACs,".prb",sep="")


###################################################
### code chunk number 7: kobe.Rnw:190-192 (eval = FALSE)
###################################################
## ## Results
## prj=kobeAspic(bio,prb,what=c("pts","trks","smry"))


###################################################
### code chunk number 8: kobe.Rnw:195-197
###################################################
class(prj)
names(prj)


###################################################
### code chunk number 9: kobe.Rnw:200-202 (eval = FALSE)
###################################################
## ## add TAC column to data.frame
## prj=llply(prj, transform, TAC=TACs[X1])


###################################################
### code chunk number 10: kobe.Rnw:207-208
###################################################
head(prj$trks)


###################################################
### code chunk number 11: kobe.Rnw:213-214
###################################################
head(prj$smry)


###################################################
### code chunk number 12: kobe.Rnw:219-220
###################################################
data(sims)


###################################################
### code chunk number 13: kobe.Rnw:243-247
###################################################
ggplot(assmt)                                       + 
  geom_hline(aes(yintercept=1),col="red",size=2)    + 
  geom_line( aes(year,stock,group=iter,col=iter))   +
  theme(legend.position="none")


###################################################
### code chunk number 14: kobe.Rnw:261-268
###################################################
### tracks
ggplot(subset(prj$trks,year<=2020))                           +
  geom_line(aes(year,stock,  linetype=Percentile),col="blue") +
  geom_line(aes(year,harvest,linetype=Percentile),col= "red") +
  scale_linetype_manual(values=c(2,1,2))                      +
  coord_cartesian(ylim=c(0,3))                                +
  facet_wrap(~TAC,ncol=2)


###################################################
### code chunk number 15: kobe.Rnw:286-289
###################################################
kp=kobePhase(subset(sims, year==2010 & TAC==15000)) +
         geom_point(aes(stock,harvest,group=Run,col=Run)) 
kp


###################################################
### code chunk number 16: kobe.Rnw:300-302
###################################################
data(sims)
head(sims)


###################################################
### code chunk number 17: kobe.Rnw:314-318
###################################################
dat =subset(sims,year<=2010 & TAC==15000)
trks=ddply(dat,.(Run,year,TAC), function(x) kobeTrks(x$stock,x$harvest,prob=c(0.5)))

head(trks)


###################################################
### code chunk number 18: kobe.Rnw:327-331
###################################################
kp + geom_path( aes(stock,harvest,group=Run,col=Run), data=trks) +
     geom_point(aes(stock,harvest,group=Run), data=subset(trks,year==2010),col="cyan",size=3)+
     facet_wrap(~Run) + 
     theme(legend.position = "none")


###################################################
### code chunk number 19: kobe.Rnw:341-346 (eval = FALSE)
###################################################
## calculates 2D probabilities
kobeProb=function(x,y,prob=c(0.5, 0.75,0.95),na.rm=FALSE){
  
  if (na.rm){
    .na=is.na(x)|is.na(y)|is.nan(x)|is.nan(y)
    x=x[.na]
    y=y[.na]}
  
  tmp=HPDregionplot(mcmc(data.frame(x,y)),prob=prob)
  
  
  prb=ldply(tmp, function(dat) data.frame(level=dat$level,x=dat$x, y=dat$y))
  
  return(prb)}

kp2 = kp + geom_path(aes(x,y,group=level),colour="blue",
                     data=ddply(subset(sims,year==2010 & TAC==15000),.(Run), 
                                function(pts) kobeProb(pts$stock,pts$harvest,prob=c(0.7,.5,.25)))) +
                     facet_wrap(~Run) + 
                     theme(legend.position = "none")


###################################################
### code chunk number 20: kobe.Rnw:350-351 (eval = FALSE)
###################################################
## print(kp2)


###################################################
### code chunk number 21: kobe.Rnw:361-366
###################################################
pts =subset(sims, year==2010 & TAC==15000)

# stock density plot
ggplot(pts) + 
  geom_density(aes(x=stock, y= ..count.., group=Run, fill=Run, alpha=0.4))


###################################################
### code chunk number 22: kobe.Rnw:374-377
###################################################
ggplot(pts) + 
  geom_density(aes(x=stock, y=..count.., group=Run, fill=Run), 
                        fill="grey", col=grey(.9), position = "stack") 


###################################################
### code chunk number 23: kobe.Rnw:384-386
###################################################
### Bespoke Stuff ###
kobePhaseMar=function(pts,trks=NULL,mns=FALSE,size=1,
                      xlab=expression(B:B[MSY]),
                      ylab=expression(F:F[MSY]),
                      maxX=2,maxY=maxX,
                      col =colorRampPalette(c("orange","blue"),space="Lab"),
                      shade=.5,col2=grey(shade),col3=grey(shade*1.1)){
  
  if (!("run" %in% names(pts)))
    pts=cbind(pts,run=factor(1))
  if (!is.null(trks) & !("run" %in% names(trks)))
    trks=cbind(trks,run=factor(1))
  
  if ("function" %in% is(col))
    col=col(length(unique(pts$run)))
  
  if (length(size)==1) size=rep(size,2)
  
  ##### Density plots   #############################################################################################
  # stock density plot
  dS<-ggplot(pts) + 
    geom_density(aes(x = stock, y =  ..count.., group=run), fill=col2, col=col3, position = "stack") + 
    geom_density(aes(x = stock, y = -..count.., fill =run, alpha=0.4)) + 
    geom_vline(xintercept=1,col="red")       +
    scale_x_continuous(limits=c(0,maxX)) +
    scale_fill_manual(values=col)        +
    xlab(xlab) + ylab(ylab)              +
    theme(legend.position = "none", 
          axis.title.y = element_text(colour='NA'), 
          axis.text.y  = element_text(colour="NA", angle=90), 
          axis.ticks.y = element_line(colour="NA"),
          axis.ticks =   element_line(colour="NA"),
          
          axis.title.x = element_blank(), 
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(), 
          
          plot.margin = unit(c(0, 0, 0, 1), "lines"),
          panel.background = element_rect(fill   ="NA", colour="NA"), 
          panel.border     = element_rect(fill   ="NA", colour="NA"), 
          panel.grid.major = element_line(colour ="NA"), 
          panel.grid.minor = element_line(colour ="NA")              )
  
  # second density plot, oriented vertically (hence the 'coord_flip()' at the end
  dH<-ggplot(pts) + 
    geom_density(aes(x = harvest, y =  ..count.., group=run), fill=col2, col=col3, position = "stack") + 
    geom_density(aes(x = harvest, y = -..count..,               fill=run, alpha=0.4)) + 
    geom_vline(xintercept=1,col="red")  +
    scale_x_continuous(limits=c(0,maxY))   +
    scale_fill_manual(values=col)          +
    xlab(xlab) + ylab(ylab)                +
    theme(legend.position = "none", 
          axis.title.x = element_text(colour ='NA'), 
          axis.text.x  = element_text(colour ="NA"), 
          axis.ticks.x = element_line(colour ="NA"),
          axis.ticks =   element_line(colour ="NA"),
          
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(), 
          
          plot.margin = unit(c(0, 0, 1, 0), "lines"),
          panel.background = element_rect(fill   ="NA", colour ="NA"), 
          panel.border     = element_rect(fill   ="NA", colour ="NA"), 
          panel.grid.major = element_line(colour ="NA"), 
          panel.grid.minor = element_line(colour ="NA")                    
    )
  
  # kobe phase plot
  kC=kobePhase(pts) +
    geom_point(aes(stock,harvest,col=run,group=run),size=size[1]) +  
    scale_y_continuous(limits=c(0,maxY)) +
    scale_x_continuous(limits=c(0,maxX)) +
    scale_colour_manual(values=col)      +
    xlab(xlab) + ylab(ylab)              +
    theme(legend.position = "none",
          axis.text.y  = element_text(colour="grey", angle=90), 
          plot.margin = unit(c(0, 0, 1, 1), "lines")
    )
  
  #     if (length(run)>1){
  #         dS=dS+scale_fill_manual(values=col)
  #         dH=dH+scale_fill_manual(values=col)
  #         kC=kC+scale_colour_manual(values=col)      
  #         }
  #     
  if (mns)
    kC=kC+geom_point(aes(stock,harvest,col=run,group=run),size=6.0*size[1], colour="black",  data=ddply(pts,.(run),function(x) data.frame(stock=median(x$stock),harvest=median(x$harvest)))) +
    geom_point(aes(stock,harvest,col=run,group=run),size=4.5*size[1], colour="cyan",   data=ddply(pts,.(run),function(x) data.frame(stock=median(x$stock),harvest=median(x$harvest))))
  if (!is.null(trks))
    kC=kC+geom_path(aes(stock,harvest, col=run,group=run),size=1*size[2], data=trks)   
  
  fnVP=function(dH,dS,kC){
    vplayout <- function(x, y)
      viewport(layout.pos.row = x, layout.pos.col = y)
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(5, 5)))  # 5 by 5 grid
    print(dS, vp=vplayout(1,1:4))                       # the first density plot will occupy the top of the grid
    print(dH +coord_flip(), vp=vplayout(2:5,5))         # 2nd to the left +opts(legend.position = c(0,1.05)) + opts(legend.text = theme_text(colour = "black")) 
    print(kC, vp=vplayout(2:5,1:4))                     # the main x/y plot will instead spread across most of the grid
  }
  
  fnVP(dH,dS,kC)
  
  invisible(list(harvest=dH,stock=dS,phase=kC))}


print(kobePhaseMar(transform(pts,group=Run)))          


###################################################
### code chunk number 24: kobe.Rnw:397-411
###################################################
### Pies ###
pie.dat=ddply(subset(sims,year==2010 & TAC==15000),.(Run),kobeSmry,o=T)
pie.dat=ddply(melt(pie.dat,id.vars="Run"),.(Run,variable), 
              function(x) data.frame(value=mean(x$value)))

## pie charts
ggplot(subset(pie.dat,value>0), aes(x =factor(1), y=value, fill = variable)) + 
  geom_bar(width = 1) + 
  coord_polar(theta="y") +
  labs(fill='Kobe Quadrant') + xlab('') + ylab('')       +
  scale_fill_manual(values=c("red","green","yellow"))    + 
  facet_wrap(~Run)                                       + 
  scale_x_discrete(breaks=NULL)                          +
  scale_y_continuous(breaks=NULL) 


###################################################
### code chunk number 25: kobe.Rnw:420-459 (eval = FALSE)
###################################################
library(akima)

Interp=function(x,levels=seq(0.0,1.0,0.05),
               col   =c(colorRampPalette(c("red4","red"))(12),colorRampPalette(c("yellowgreen","darkgreen"))(8)),
               nIterp=101){

  x=x[!is.na(x[,1]) & !is.na(x[,2]) & !is.na(x[,3]),]
  
  ##### smooth
  t.<-interp(x[,1],x[,2],x[,3],
                    xo=seq(min(x[,1]),   max(x[,1]), length=nIterp),
                    yo=seq(min(x[,2]),   max(x[,2]), length=nIterp),
                    duplicate="mean")
  
  
  res=cbind(expand.grid(x=t.$x,y=t.$y),z=cut(t.$z,levels,include.lowest=T),w=c(t.$z))
  res$col=col[as.numeric(res$z)]
  
  res}

kobe2012=subset(sims,year %in% 2013:2022)
  
pdat=subset(ddply(kobe2012,.(year,TAC),kobeSmry),
            select=c(year,TAC,green,underFished,underFishing))
pdat=melt(pdat,id.vars=c("year","TAC"))
pdat=ddply(pdat, .(variable), function(x) Interp(data.frame(x$year,x$TAC,x$value)))

col.=c(colorRampPalette(c("red4","red"))(12),
       colorRampPalette(c("yellowgreen","darkgreen"))(8))

k2p = ggplot(aes(x=x,y=y,z=w),data=pdat)                 +
           geom_tile(aes(x,y,fill=z))                    +
           scale_fill_manual(values=col.,guide="none")   +
           stat_contour(aes(colour= ..level..),size=1.2,  
                            breaks=c(0.6,0.7,0.8,0.9))   +
           scale_colour_gradient(low="grey", high="black", 
                                 breaks=c(0.6,0.7,0.8,0.9),
                                  labels=c(0.6,0.7,0.8,0.9),limits=c(0.6,1))    +
           facet_wrap(~variable,ncol=1)                       +
           xlab("Year")+ylab("TAC") 
k2p


###################################################
### code chunk number 26: kobe.Rnw:479-485
###################################################
t.=ddply(subset(sims,year %in% 2013:2022),.(year,TAC),  kobeSmry)

k2smTab=list()
k2smTab[[1]]=cast(subset(t., year %in% 2013:2022),TAC~year,value="underFishing")
k2smTab[[2]]=cast(subset(t., year %in% 2013:2022),TAC~year,value="underFished")
k2smTab[[3]]=cast(subset(t., year %in% 2013:2022),TAC~year,value="green")


