setMethod('kobeSmry', signature(stock="numeric",harvest="numeric"),
    function(stock,harvest,onlyColours=FALSE){
                                  
    res=kobeSmryFn(data.frame(stock=stock,harvest=harvest),onlyColours=onlyColours)
      
    return(res)})

setMethod('kobeSmry', signature(stock='data.frame',harvest="missing"),
    function(stock,onlyColours=FALSE){
     
    res=kobeSmryFn(stock,onlyColours=onlyColours)
      
    return(res)})

kobeSmryFn=function(x,onlyColours=onlyColours){
  
  res =cbind(x,   kobeP(x$stock,x$harvest))
  res = with(res, data.frame(stock       =median(stock,       na.rm=T),
                             harvest     =median(harvest,     na.rm=T),
                             red         =mean(  red,         na.rm=T),
                             yellow      =mean(  yellow,      na.rm=T),
                             green       =mean(  green,       na.rm=T),
                             overFished  =mean(  overFished,  na.rm=T),
                             overFishing =mean(  overFishing, na.rm=T),
                             underFished =1-mean(overFished,  na.rm=T),
                             underFishing=1-mean(overFishing, na.rm=T)))
  
  if (onlyColours) res=res[,3:5]
  return(res)}


