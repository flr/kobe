setMethod('hcr', signature(object='missing'),
 function(object,ftar=0.7,btrig=0.7, fmin=0.01,blim=0.20,bmax=2){
  
  
  data.frame(stock  =c(   0,blim,btrig,bmax), 
             harvest=c(fmin,fmin, ftar,ftar))})

