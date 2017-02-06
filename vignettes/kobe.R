## ----knitr_init, echo=FALSE, results="hide"------------------------------
library(knitr)
## Global options
opts_chunk$set(echo    =!TRUE,
               eval    =TRUE,
               cache   =!FALSE,
               cache.path="cache/",
               prompt  =FALSE,
               comment =NA,
               message =FALSE,
               tidy    =FALSE,
               warnings=FALSE,
               fig.height=4.5,
               fig.width =4.5,
               fig.path  ="tex/")

## ---- pkgs, echo=FALSE, message=FALSE------------------------------------
sink(NULL)
warn=options()$warn
options(warn=-1)
library(ggplot2)
library(kobe)

theme_set(theme_bw())
options(digits=3)
options(warn=warn)
sink()

## ----install,echo=TRUE,eval=FALSE----------------------------------------
## install.packages("kobe", repos = "http://cloud.r-project.org/")

## ----lib,echo=TRUE-------------------------------------------------------
library(kobe)

## ----data-yft,echo=TRUE--------------------------------------------------
data(yft)

## ----data-yft2,echo=TRUE-------------------------------------------------
library(kobe)
data(yft)
head(yft)

## ----kpp,echo=TRUE-------------------------------------------------------
library(ggplot2)

kobePhase(subset(yft,year==2014))+
  geom_point(aes(stock,harvest))

## ----plyr,echo=TRUE------------------------------------------------------
library(plyr)

## ----plyr2,echo=TRUE-----------------------------------------------------
trks=ddply(yft, .(method,scenario), with, quantile(stock))

## ----plyr3,echo=TRUE-----------------------------------------------------
trks=ddply(yft, .(method,scenario,year), with, quantile(stock))

## ----plyr4,echo=TRUE-----------------------------------------------------
head(trks)

## ----plyr5,echo=TRUE,fig.height=4,fig.width=8----------------------------
ggplot(trks)+
  geom_line(aes(year,`50%`,
                col  =method,
                group=paste(method,scenario)))

## ----sa, eval=FALSE------------------------------------------------------
## read.kobe()

## ----sa2, eval=FALSE-----------------------------------------------------
## ?read.kobe

## ----sa3, eval=FALSE-----------------------------------------------------
## library(mpb)
## 
## kobe()

## ----echo=TRUE-----------------------------------------------------------
yft2014=subset(yft,year==2014)
kobePhase(yft2014)+
  geom_point(aes(stock,harvest,col=method))+
  facet_grid(method~scenario)

## ----echo=TRUE-----------------------------------------------------------
library(plyr)
trks=ddply(yft,.(method,scenario,year), 
           function(x) trks(x$stock,x$harvest,prob=c(0.5)))

## ----echo=TRUE-----------------------------------------------------------
kobePhase() + 
   geom_point(aes(stock,harvest), data=subset(yft,year==2014),col="cyan")+
   geom_path( aes(stock,harvest), data=trks) +
   facet_wrap(method~scenario) 

## ----method,echo=TRUE, eval=FALSE----------------------------------------
## shade

## ----method2,echo=TRUE, eval=FALSE---------------------------------------
## smry

## ----method3,echo=TRUE, eval=FALSE---------------------------------------
## prob

## ----method4,echo=TRUE, eval=FALSE---------------------------------------
## density

## ----method5,echo=TRUE, eval=FALSE---------------------------------------
## freq

## ----method6,echo=TRUE, eval=FALSE---------------------------------------
## interp

## ----method7,echo=TRUE, eval=FALSE---------------------------------------
## recovered

## ----mar1,eval=FALSE-----------------------------------------------------
## geom_path(aes(x,y,group=level),colour="blue",
##                     data=ddply(subset(sims,year==2010 & TAC==15000),.(Run),
##                                function(pts) kobeProb(pts$stock,pts$harvest,prob=c(0.7,.5,.25)))) +
##                     facet_wrap(~Run) +
##                     theme(legend.position = "none")

## ----mar2,echo=TRUE------------------------------------------------------
kobe:::kobePhaseMar(transform(yft2014,run=paste(scenario,method))[,c("stock","harvest","run")])

## ----mar3,echo=TRUE------------------------------------------------------
kobe:::kobePhaseMar2(transform(yft2014,run=paste(scenario,method))[,c("stock","harvest","run")])

## ----mar4,echo=TRUE------------------------------------------------------
kobe:::kobePhaseMar3(transform(yft2014,run=paste(scenario,method))[,c("stock","harvest","run")])

## ----mar5,eval=FALSE-----------------------------------------------------
## pie.dat=ddply(subset(sims,year==2010 & TAC==15000),.(Run),kobeSmry,o=T)
## pie.dat=ddply(melt(pie.dat,id.vars="Run"),.(Run,variable),
## function(x) data.frame(value=mean(x$value)))
## ## pie charts
## ggplot(subset(pie.dat,value>0), aes(x =factor(1), y=value, fill = variable)) +
## geom_bar(width=1,stat="identity") +
## coord_polar(theta="y") +
## labs(fill= ' Kobe Quadrant ' ) + xlab( '' ) + ylab( '' )+
## scale_fill_manual(values=c("red","green","yellow"))+
## facet_wrap(~Run)+
## scale_x_discrete(breaks=NULL)+
## scale_y_continuous(breaks=NULL)

## ----mse,echo=TRUE, eval=FALSE-------------------------------------------
## aav
## pid
## dRate
## hinge
## iav
## incr

## ----hcr,echo=TRUE-------------------------------------------------------
hcr= data.frame(stock  =c(0.0 ,0.1 , 0.6,2.0), 
                harvest=c(0.01,0.01, 0.7,0.7))
kobePhase()+
  geom_line(aes(stock,harvest),data=hcr,col="orange",size=2)

## ---- devtools, echo=TRUE, eval=FALSE------------------------------------
## 	library(devtools)
## 	install_github('flr/FLPKG')

