utils::globalVariables(c("read.csv","read.table","quantile","median",
                         "var","grey","colorRampPalette",
                         "V2","V3","V1","X1","par","mtext","rbind.fill",
                         "mdply","modifyList","coefficients","lm",
                         "kobeFn","kobe2smFn"))

setGeneric('kobe',       function(path,method,...)     standardGeneric('kobe'))
setGeneric('kobe.2box',  function(path,...)            standardGeneric('kobe.2box'))
setGeneric('kobe.aspic', function(path,...)            standardGeneric('kobe.aspic'))
setGeneric('kobe.bsp',   function(path,...)            standardGeneric('kobe.bsp'))
setGeneric('kobe.mfcl',  function(path,...)            standardGeneric('kobe.mfcl'))
setGeneric('kobe.ss',    function(path,...)            standardGeneric('kobe.ss'))

setGeneric('kobePhase',  function(object,...)         standardGeneric('kobePhase'))
setGeneric('kobe2sm',    function(object,...)         standardGeneric('kobe2sm'))

setGeneric('shade',      function(object,...)         standardGeneric('shade'))
setGeneric('prob',       function(stock,harvest,...)  standardGeneric('prob'))
setGeneric('smry',       function(stock,harvest,...)  standardGeneric('smry'))
setGeneric('trks',       function(stock,harvest,...)  standardGeneric('trks'))
  
setGeneric('aav',        function(object,...)         standardGeneric('aav'))
setGeneric('antiCurve',  function(object,...)         standardGeneric('antiCurve'))
setGeneric('hcr',        function(object,...)         standardGeneric('hcr'))


  

