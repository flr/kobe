utils::globalVariables(c("read.csv","read.table","quantile","median",
                         "var","grey","colorRampPalette",
                         "V2","V3","V1","X1","par","mtext","rbind.fill",
                         "mdply","modifyList","coefficients","lm",
                         "kobeFn","kobe2smFn"))

setGeneric('kobe',       function(path,method,...)     standardGeneric('kobe'))
setGeneric('kobe2box',   function(path,...)            standardGeneric('kobe2box'))
setGeneric('kobeAspic',  function(path,...)            standardGeneric('kobeAspic'))
setGeneric('kobeBsp',    function(path,...)            standardGeneric('kobeBsp'))
setGeneric('kobeMfcl',   function(path,...)            standardGeneric('kobeMfcl'))
setGeneric('kobeSS',     function(path,...)            standardGeneric('kobeSS'))

setGeneric('kobePhase',  function(object,...)         standardGeneric('kobePhase'))
setGeneric('kobe2sm',    function(object,...)         standardGeneric('kobe2sm'))

setGeneric('shade',      function(object,...)         standardGeneric('shade'))
setGeneric('prob',       function(stock,harvest,...)  standardGeneric('prob'))
setGeneric('smry',       function(stock,harvest,...)  standardGeneric('smry'))
setGeneric('trks',       function(stock,harvest,...)  standardGeneric('trks'))
  
setGeneric('aav',        function(object,...)         standardGeneric('aav'))
setGeneric('antiCurve',  function(object,...)         standardGeneric('antiCurve'))
setGeneric('hcr',        function(object,...)         standardGeneric('hcr'))


  

