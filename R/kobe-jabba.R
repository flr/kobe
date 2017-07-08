library(FLCore)
library(ggplotFL)
library(kobe)

require(plyr)
require(dplyr)
require(reshape2)


kobeJabba<-function(x,minyear=1){
  
  res=cbind(melt(x[,,2]),c(x[,,3]))
  names(res)=c("iter","year","stock","harvest")
  res$year=res$year+minyear-1
  
  res}

if (FALSE){
  
  kb=kobeJabba(trajectories,1950)
  kobe:::kobePhaseMar3(subset(kb,year==2015))

  }

kobeJabbaProj<-function(x,minyear=1,tac=NULL){
  
  res=cbind(melt(x[,,,2]),c(x[,,,3]))
  names(res)=c("iter","year","tac","stock","harvest")
  res$year=res$year+minyear-1
  
  res}

if (FALSE){
  
  kbPrj=kobeJabbaProj(projections,2015)
  kbPrj=transform(kbPrj,tac=seq(4000,16000,1000)[tac])
  
  stk=FLQuants(dlply(kbPrj,.(tac), with, as.FLQuant(data.frame(year=year,iter=iter,data=stock))))
  p=plot(stk)$data
  ggplot(p)+
    geom_hline(aes(yintercept=1))+
    geom_line(aes(year,`50%`,col=qname))+
    xlab("Year")+ylab(expression(B/B[MSY]))+ 
    labs(colour = "TAC")+
    theme_bw()
  
  hvt=FLQuants(dlply(kbPrj,.(tac), with, as.FLQuant(data.frame(year=year,iter=iter,data=harvest))))
  p=plot(hvt)$data
  ggplot(p)+
    geom_hline(aes(yintercept=1))+
    geom_line(aes(year,`50%`,col=qname))+
    xlab("Year")+ylab(expression(F/F[MSY]))+ 
    labs(colour = "TAC")+
    theme_bw()
  
  t.=ddply(kbPrj,.(year,tac), with, kobe:::smry(stock,harvest))
  
  k2smTab=list()
  k2smTab[[1]]=cast(t.,tac~year,value="underFishing")
  k2smTab[[2]]=cast(t.,tac~year,value="underFished")
  k2smTab[[3]]=cast(t.,tac~year,value="green")
  
}
  

