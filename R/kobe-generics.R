utils::globalVariables(c("read.csv","read.table","quantile","median",
                         "var","grey","colorRampPalette",
                         "V2","V3","V1","X1","par","mtext","rbind.fill",
                         "mdply","modifyList","coefficients","lm",
                         "kobeFn","kobe2smFn"))

setGeneric('kobe',      function(object,method,...) standardGeneric('kobe'))
setGeneric('read.kobe', function(object,method,...) standardGeneric('read.kobe'))

setGeneric('kobePhase',  function(object,...)         standardGeneric('kobePhase'))
setGeneric('shade',      function(object,...)         standardGeneric('shade'))
setGeneric('kobe2sm',    function(object,...)         standardGeneric('kobe2sm'))
  
setGeneric('prob',      function(stock,harvest,...)  standardGeneric('prob'))
setGeneric('smry',   function(stock,harvest,...)  standardGeneric('smry'))
setGeneric('trks',   function(stock,harvest,...)  standardGeneric('trks'))
  
setGeneric('aav', function(object, ...) standardGeneric('aav'))

setGeneric('antiCurve', function(object, ...) standardGeneric('antiCurve'))

setGeneric('kobePhase',  function(object,...) standardGeneric('kobePhase'))

setGeneric('hcr', function(object,...) standardGeneric('hcr'))


  

