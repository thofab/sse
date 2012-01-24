 
setGeneric(# calculates the array with the power, stores it into powCalc
           name = "powCalc",
           def = function(object, statistic, ...){standardGeneric("powCalc")}
           )
  
setGeneric(# for programming on powPar parameters
           name = "pp",
           def = function(object, name){standardGeneric("pp")}
           )

setGeneric(# for programming on powPar parameters
           name = "n",
           def = function(object, ...){standardGeneric("n")}#,
#           useAsDefault = function(object = pow, ...){object@n.act}
           )

setGeneric(# for programming on powPar parameters
           name = "theta",
           def = function(object, ...){standardGeneric("theta")}
           )

setGeneric(# for programming on powPar parameters
           name = "xi",
           def = function(object, ...){standardGeneric("xi")}
           )

setGeneric(# estimats n from objects of class "power"
           name = "sampleSize",
           def = function(x, ...) {
             standardGeneric("sampleSize")
           })

setGeneric(# estimats n from power
           name = "exDat",
           def = function(x, y, ...) {
             standardGeneric("exDat")
           })

setGeneric(# estimats n from power
           name = "inspect",
           def = function(object, ...) {
             standardGeneric("inspect")
           })

setGeneric(# for reporting the sampling schema
           name = "tex",
           def = function(x, type, ...){standardGeneric("tex")}
           )
