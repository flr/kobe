utils::globalVariables(c("stock","..count..","group","series","run","is"))
#' @importFrom grid grid.newpage pushViewport viewport grid.layout unit
#' 
#' @name trendPhaseMar
#' @title trend Phase Plot with Marginal Densities
#' @description 
#' Plots a trend Phase Plot with marginal densities
#' '      
#' @aliases trendPhaseMar trendPhaseMar2 trendsPhaseMar3
#' 
#' @param  pts data.frame with stock and series variables respresenting points for a year
#' @param  trks data.frame with stock and series variables respresenting tracks, NULL by default
#' @param  mns logical, do you want mean by year of pts be plotted, FALSE  by default
#' @param  size of points1  by default
#' @param  xlab character or expression for x axis title, expression(B:B[MSY])  by default
#' @param  ylab character or expression for x axis title, expression(F:F[MSY])  by default
#' @param  xlim upper limit of x axis, 2  by default
#' @param  ylim upper limit of x axis, 2  by default
#' @param  quadcol colours for the quadrants
#' @param  col colorRampPalette(c("orange","blue"),space="Lab")  by default
#' @param  shade .5  by default
#' @param  col2 grey(shade)  by default
#' @param  col3 grey(shade*1.1)  by default
#' @param  layer a ggplot2 layer to add to phase plot
#' @param  bref vertical seperation between quadants and reference line for marginal densities
#' @param  fref horizontal seperation between quadants and reference line for marginal densities
#' 
#' @return a list with 3 ggplot objects, the 2 marginal densities and the phase plot
#' @export trendPhaseMar
#' @docType methods
#' @rdname  trendPhaseMar-method
#' 
#' @examples
#'  \dontrun{rnorm(10)}

trendPhaseMar=function(trks,pts=subset(trks,year==max(year)),
             ylab=expression(B:B[MSY]),
             ylim=5,
             #quadcol=c("red","green","blue","blue"),
             layer=NULL){
     
    if (!("run" %in% names(pts)))
       pts=cbind(pts,run=factor(1))
    if (!is.null(trks) & !("run" %in% names(trks)))
      trks=cbind(trks,run=factor(1))
 
    
    ##### Density plots   #############################################################################################
    marginal<-ggplot(pts) + 
            geom_density(aes(x = series, y =  ..count..), fill="blue",col="orange",position = "stack")+ 
            geom_vline(xintercept=1,col="red",data=data.frame(fref=1))+
            #coord_cartesian(xlim=c(0,ylim))   +
            #scale_x_continuous(limits=c(0,ylim))   +
            #scale_fill_manual(values=col)          +
                xlab("") + ylab("")                +
                theme(legend.position = "none", 
                      axis.title.x = element_text(colour ='NA'), 
                      axis.text.x  = element_text(colour ="NA"), 
                      axis.ticks.x = element_line(colour ="NA"),
                      axis.ticks =   element_line(colour ="NA"),
                      
                      axis.title.y = element_blank(), 
                      axis.text.y  = element_blank(), 
                      axis.ticks.y = element_blank(), 
                      
                      plot.margin = unit(c(1, 0, 0, 0), "lines"),
                      panel.background = element_rect(fill   ="NA", colour ="NA"), 
                      panel.border     = element_rect(fill   ="NA", colour ="NA"), 
                      panel.grid.major = element_line(colour ="NA"), 
                      panel.grid.minor = element_line(colour ="NA")                    
                )
  
    pbs=ddply(trks,.(year), with, t(quantile(series,probs=c(0,0.05,0.25,0.50,0.75,0.95,1),na.rm=T)))
    
    series=ggplot(pbs)+ 
      geom_ribbon(aes(year,ymin=0,ymax=ylim),fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"red","green"))+
      geom_ribbon(aes(year,ymin=0,ymax=1.0), fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"green","red"))+ 
      geom_line(aes(year,series,group=run),data=trks,col="grey10",size=0.05)+
      geom_line(aes(year,`50%`),size=0.5,  col="black")+
      geom_line(aes(year,`75%`),size=0.5,linetype=2,col="black")+
      geom_line(aes(year,`25%`),size=0.5,linetype=2,col="black")+
      geom_line(aes(year,`95%`),size=0.5,linetype=3,col="black")+
      geom_line(aes(year, `5%`),size=0.5, linetype=3,col="black")+
      theme_bw()+theme(legend.position="bottom")+
      scale_y_continuous(lim=c(0,ylim))+
      theme(legend.position="none",
            axis.text.y=element_text(colour="black", angle=90), 
            plot.margin = unit(c(0, 1, 0, 0), "lines"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())+
      xlab("Year")+ylab(ylab)
    
    series=series+layer
    
    fnVP=function(marginal,series){
        vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
          
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(5, 6)))  # 5 by 5 grid
        print(marginal +coord_flip(xlim=c(0,ylim)), vp=vplayout(2:5,6))         # 2nd to the left +opts(legend.position = c(0,1.05)) + opts(legend.text = theme_text(colour = "black")) 
        print(series, vp=vplayout(2:5,1:5))                     # the main x/y plot will instead spread across most of the grid
        }
    
    fnVP(marginal,series)

    invisible(list(series,marginal))}


trendPhaseMar2=function(object,pts=object[,ac(max(dimnames(object)$year))],
                       ylab=expression(B:B[MSY]),
                       ylim=5,
                       #quadcol=c("red","green","blue","blue"),
                       layer=NULL){
  
  ##### Density plots   #############################################################################################
  marginal<-ggplot(FLCore:::as.data.frame(pts,drop=T)) + 
    geom_density(aes(x = data, y =  ..count..), fill="blue",col="grey50",position = "stack")+ 
    geom_vline(xintercept=1,col="red")+
    #coord_cartesian(xlim=c(0,ylim))   +
    #scale_x_continuous(limits=c(0,ylim))   +
    #scale_fill_manual(values=col)          +
    xlab("") + ylab("")                +
    theme(legend.position = "none", 
          axis.title.x = element_text(colour ='NA'), 
          axis.text.x  = element_text(colour ="NA"), 
          axis.ticks.x = element_line(colour ="NA"),
          axis.ticks =   element_line(colour ="NA"),
          
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(), 
          
          plot.margin = unit(c(1, 0, 0, 0), "lines"),
          panel.background = element_rect(fill   ="NA", colour ="NA"), 
          panel.border     = element_rect(fill   ="NA", colour ="NA"), 
          panel.grid.major = element_line(colour ="NA"), 
          panel.grid.minor = element_line(colour ="NA")                    
    )
  
  pbs=ddply(as.data.frame(object,drop=T),.(year), with, t(quantile(data,probs=c(0,0.05,0.25,0.50,0.75,0.95,1),na.rm=T)))
  
  trend=
    ggplot(pbs)+ 
    geom_ribbon(aes(year,ymin=1,ymax=ylim),alpha=0.3,fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"red","green"))+
    geom_ribbon(aes(year,ymin=0,ymax=1.0), alpha=0.3, fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"green","red"))+ 
    #geom_line(aes(year,data,group=run),data=trks,col="grey10",size=0.05)+
    geom_ribbon(aes(year,ymin=`25%`,ymax=`75%`),size=0.5,linetype=2,fill="blue",alpha=1)+
    geom_ribbon(aes(year,ymin= `5%`,ymax=`95%`),size=0.5,linetype=2,fill="blue",alpha=0.5)+
    geom_line(aes(year,`25%`),size=0.5,col="grey85")+
    geom_line(aes(year,`75%`),size=0.5,col="grey85")+
    geom_line(aes(year,`95%`),size=0.5,col="grey15")+
    geom_line(aes(year, `5%`),size=0.5,col="grey15")+
    geom_line(aes(year,`50%`),size=1,col="black")+
    theme_bw()+theme(legend.position="bottom")+
    scale_y_continuous(lim=c(0,ylim))+
    theme(legend.position="none",
          axis.text.y=element_text(colour="black", angle=90), 
          plot.margin = unit(c(0, 1, 0, 0), "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    xlab("Year")+ylab(ylab)
  
  trend=trend+layer
  
  fnVP=function(marginal,trend){
    vplayout <- function(x, y)
      viewport(layout.pos.row = x, layout.pos.col = y)
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(5, 6)))  # 5 by 5 grid
    print(marginal +coord_flip(xlim=c(0,ylim)), vp=vplayout(2:5,6))         # 2nd to the left +opts(legend.position = c(0,1.05)) + opts(legend.text = theme_text(colour = "black")) 
    print(trend, vp=vplayout(2:5,1:5))                     # the main x/y plot will instead spread across most of the grid
  }
  
  fnVP(marginal,trend)
  
  invisible(list(trend,marginal))}


trendPhaseMar3=function(objects,pts=FLQuants(llply(objects, function(x) x[,ac(max(dimnames(x)$year))])),
                        ylab=expression(B:B[MSY]),
                        ylim=5,
                        #quadcol=c("red","green","blue","blue"),
                        layer=NULL){
  
  ##### Density plots   #############################################################################################
  marginal=
    ggplot(as.data.frame(pts,drop=T)) + 
    geom_vline(xintercept=1,col="red")+
    geom_density(aes(x = data, y =  ..count.., fill=qname, col=qname),position="identity",alpha=0.5)+ 
    #coord_cartesian(xlim=c(0,ylim))   +
    #scale_x_continuous(limits=c(0,ylim))   +
    #scale_fill_manual(values=col)          +
    xlab("") + ylab("")                +
    theme(legend.position = "none", 
          axis.title.x = element_text(colour ='NA'), 
          axis.text.x  = element_text(colour ="NA"), 
          axis.ticks.x = element_line(colour ="NA"),
          axis.ticks =   element_line(colour ="NA"),
          
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(), 
          
          plot.margin = unit(c(1, 0, 0, 0), "lines"),
          panel.background = element_rect(fill   ="NA", colour ="NA"), 
          panel.border     = element_rect(fill   ="NA", colour ="NA"), 
          panel.grid.major = element_line(colour ="NA"), 
          panel.grid.minor = element_line(colour ="NA")                    
    )
  
  pbs=ddply(as.data.frame(objects,drop=T),.(year,qname), with, t(quantile(data,probs=c(0,0.05,0.25,0.50,0.75,0.95,1),na.rm=T)))
  
  trend=
    ggplot(pbs)+ 
    geom_ribbon(aes(year,ymin=1,ymax=ylim),alpha=0.2,fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"red","green"))+
    geom_ribbon(aes(year,ymin=0,ymax=1.0), alpha=0.2,fill=ifelse(tolower(substr(as.character(ylab),1,1))%in%c("y","c","f"),"green","red"))+ 
    #geom_line(aes(year,data,group=run),data=trks,col="grey10",size=0.05)+
    geom_ribbon(aes(year,ymin=`25%`,ymax=`75%`,fill=qname),size=0.5,linetype=2,alpha=0.5)+
    geom_ribbon(aes(year,ymin= `5%`,ymax=`95%`,fill=qname),size=0.5,linetype=2,alpha=0.25)+
    geom_line(aes(year,`50%`,col=qname))+
    geom_line(aes(year,`25%`,col=qname),size=0.25,linetype=1)+
    geom_line(aes(year,`25%`,col=qname),size=0.25,linetype=1)+
    geom_line(aes(year,`95%`,col=qname),size=0.25,linetype=1)+
    geom_line(aes(year, `5%`,col=qname),size=0.25,linetype=1)+
    theme_bw()+theme(legend.position="bottom")+
    scale_y_continuous(lim=c(0,ylim))+
    theme(legend.position="left",
          axis.text.y=element_text(colour="black", angle=90), 
          plot.margin = unit(c(0, 1, 0, 0), "lines"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    xlab("Year")+ylab(ylab)
  
  trend=trend+layer
  
  fnVP=function(marginal,trend){
    vplayout <- function(x, y)
      viewport(layout.pos.row = x, layout.pos.col = y)
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(5, 6)))  # 5 by 5 grid
    print(marginal +coord_flip(xlim=c(0,ylim)), vp=vplayout(2:5,6))         # 2nd to the left +opts(legend.position = c(0,1.05)) + opts(legend.text = theme_text(colour = "black")) 
    print(trend, vp=vplayout(2:5,1:5))                     # the main x/y plot will instead spread across most of the grid
  }
  
  fnVP(marginal,trend)
  
  invisible(list(trend,marginal))}


