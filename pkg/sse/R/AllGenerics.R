
setGeneric(# calculates the array with the power, stores it into powCalc
    name = "powCalc",
    def = function(object, statistic, n.iter = NA, cluster = FALSE){
      standardGeneric("powCalc")
    })

setGeneric(# for generating the example object in one step (replacing powEx and merge that are kept for back compatibility (and internal usage))
    name = "powEx",
    def = function(x, theta, xi = NA, endpoint = NA, power = 0.9, drop = 0,
                   method = c("default", "lm", "step"), lm.range = NA,
                   forceDivisor = FALSE){
      standardGeneric("powEx")
    })


setGeneric(name = "powFunGen",
           def = function(object, ...){standardGeneric("powFunGen")}
           )

setGeneric(
           name = "workhorse",
           def = function(object, ...){standardGeneric("workhorse")}
           )

  
setGeneric(# for programming on powPar parameters
           name = "pp",
           def = function(x, name){standardGeneric("pp")}
           )

setGeneric(# for programming on powPar parameters
           name = "n",
           def = function(x){standardGeneric("n")}#,
#           useAsDefault = function(object = pow, ...){object@n.act}
           )

setGeneric(
           name = "refine",
           def = function(object, n.iter = 10){standardGeneric("refine")}
           )

setGeneric(# for programming on powPar parameters
           name = "theta",
           def = function(x){standardGeneric("theta")}
           )

setGeneric(# for programming on powPar parameters
    name = "xi",
    def = function(x){
      standardGeneric("xi")
    })

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
    def = function(object) {
      standardGeneric("inspect")
    })

setGeneric(# for reporting the sampling schema
    name = "tex",
    def = function(x, type, ...){standardGeneric("tex")}
)
