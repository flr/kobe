utils::globalVariables("str_trim")
utils::globalVariables("FLQuants")
utils::globalVariables("as.FLQuant")
utils::globalVariables("data")

readASPM<-function(file,what="sr"){
    tmp=tolower(str_trim(scan(file,sep="\n",what="string")))
    
    if (what=="f")  target=tolower("Overall_Fishing_mortality")
    if (what=="n")  target=tolower("Begin_year_numbers_at_age")
    if (what=="sr") target=tolower("Recruitment")
    
    pos=seq(length(tmp))[tmp==target]
    eob=seq(length(tmp))[tmp==""]
    eob=eob[pos<eob][1]-1
    
    pos=pos+max(grep("year",tmp[pos:eob]))
    nch=strsplit(tmp[pos-1],"\\s+")[[1]]
    if (what=="n")
      age=nch[-c(1,length(nch)-0:2)]
    if (what=="f")
      age=nch[-c(1)]
    
    if (what=="sr"){
      res=mdply(tmp[pos:eob],function(x) strsplit(x,"\\s+")[[1]])[,-1]
      res=FLQuants(
               ssb=as.FLQuant(transform(res,
                                        year=V1,
                                        data=as.numeric(as.character(V2)))[,c("year","data")]),
               rec=as.FLQuant(transform(res,
                                        year=V1,
                                        data=as.numeric(as.character(V3)))[,c("year","data")]))
      return(res)}
    
    res=tmp[pos:eob]
    res=mdply(res,function(x) strsplit(x,"\\s+")[[1]])[,-1]
    res=melt(res,id="V1")
    res=transform(res,year=V1,
                      age =age[as.numeric(variable)],
                      data=as.numeric(as.character(value)))[,c("year","age","data")]
    res=subset(res,!is.na(data))
    as.FLQuant(res)}