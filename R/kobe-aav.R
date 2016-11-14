av=function(x) {
  n  =length(x)
  
  if (!is.numeric(x[-1]) || !is.numeric(x[-n])) return(NULL)
  res1=sum(abs(x[-1]-x[-n]))
  res2=sum(x[-n])
  
  return(res1/res2)}


setMethod('aav', signature(object='data.frame'),
 function(object){
          
  yrs   =object$year
  object=object[order(yrs),]
  
  res  =apply(object, 2, av)
  res  =res[!(names(res) %in% "year")]

  return(res)})
  

setMethod('antiCurve', signature(object='data.frame'),
 function(object,tol=0.01){
          
  yrs   =object$year
  object=object[order(yrs),]
  
  anCr=function(x) {
      n  =length(x)
      if (!is.numeric(x[-(1:2)]) || !is.numeric(x[-(n-0:1)])) return(NULL)

      M  =(x[-(1:2)]+x[-(n-0:1)])/2
      o1 =x[-c(1,n)]

      res=mean(abs((o1-M)/pmax(M,tol)), na.rm=TRUE)
 
      return(res)}
 
  res  =apply(object, 2, anCr)
  res  =res[!(names(res) %in% "year")]

  return(res)})

