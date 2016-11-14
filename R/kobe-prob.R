setMethod('prob', signature(stock="numeric",harvest="numeric"),
    function(stock,harvest){
                                  
    res=probFn(data.frame(stock=stock,harvest=harvest))
      
    return(res)})

setMethod('prob', signature(stock='data.frame',harvest="missing"),
    function(stock){
     
    res=probFn(stock)
      
    return(res)})


probFn=function(x) {
  
  b =  pmax(pmin(as.integer(x$stock),  1),0)
  f =1-pmax(pmin(as.integer(x$harvest),1),0)
  p =f*b
  collapsed=(1-b)*(1-f)
  
  red   =collapsed
  green =p
  yellow=1-p-collapsed
  
  overFished =1-b
  overFishing=1-f  
  
  data.frame(red=red,green=green,yellow=yellow,overFished=overFished,overFishing=overFishing)}

