setMethod('kobe',  signature(object="data.frame",method="missing"),  
          function(object,method,what=c("sims","trks","pts","smry","wrms")[1],dir="",
                   prob=c(0.75,0.5,.25),year=NULL,nwrms=10){
  
  res=llply(object, function(x,what=what,prob=prob,year=year,nwrms=nwrms)
    kobeFn(object,what=what,prob=prob,year=year,nwrms=nwrms),
            what=what,prob=prob,year=year,nwrms=nwrms)
  
  res=list(trks=ldply(res, function(x) x$trks),
           pts =ldply(res, function(x) x$pts),
           smry=ldply(res, function(x) x$smry),
           wrms=ldply(res, function(x) x$wrms),
           sims=ldply(res, function(x) x$sims))
  
  if (length(what)==1)
    return(res[[what]])
  else
    return(res[what]) })

