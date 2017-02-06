pid<-function(index,setpt=0,nreg=5){
  err = index - setpt 
  n   =length(err)
  
  b=unlist(coefficients(lm(y~x,data=data.frame(y=index[(length(index)-(nreg-1)):length(index)],x=1:nreg)))["x"])
  names(b)="b"
  
  c(p=err[n],
    i=sum(err),
    d=err[n]-err[n-1],
    s=var(err)^0.5,
    b)}

