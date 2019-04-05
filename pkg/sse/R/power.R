### ------------------------------------------------------------------ CLASSES
### FIXME description in
###   - man/powPar-class.Rd,
###   - man/powCalc-class.Rd, and
###   - man/power-class.Rd
setClass("powPar",
         representation(list = "list",
                        theta = "numeric",
                        theta.name = "character",
                        theta.act = "numeric",
                        xi = "numeric",
                        xi.name = "character",
                        xi.act = "numeric",
                        n = "integer",
                        n.act = "integer"))
##
setClass("powEx",
         representation(theta.example = "numeric",
                        xi.example = "numeric",
                        endpoint.example = "character",
                        power.example = "numeric",
                        iter.example = "integer",
                        method = "character",
                        lm.range = "numeric",
                        drop = "numeric",
                        forceDivisor = "logical",
                        divisor = "integer"))
##
setClass("powFun",
         representation(statistic = "function",
                        return.power = "logical",
                        return.list = "logical",
                        return.n = "integer",
                        return.names = "character"))
##
setClass("powCalc",
         representation(core = "array",
                        size = "array",
                        iter = "integer",
                        cluster = "list"),
         contains = c("powFun", "powPar"))

##
setClass("SampleSize",
         representation(estimate = "integer"))

##
setClass("power",
         contains = c("powCalc", "powEx"),
         validity=function(object){
           ## theta of powEx should be in the range of theta from powCalc
           if ( object@theta.example > max(object@theta) | object@theta.example < min(object@theta) ){
             stop("The theta that is chosen for the example should be within the range of evaluated values definded in the object generated with powPar.")
           }
           return(TRUE)
         }
         )

## validity=function(object){
##   if(length(object@n)!=dim(object@core)[1]) return("the slot \"n\" does not correspond to the dimention of the core")
##   if(length(object@theta)!=dim(object@core)[2]) return("the slot \"theta\" does not correspond to the dimention of the core")
##   if(length(object@xi)!=dim(object@core)[3]) return("the slot \"xi\" does not correspond to the dimention of the core")
##   if(length(object@endpoint)!=dim(object@core)[4]) return("the slot \"endpoint\" does not correspond to the dimention of the core")



### ------------------------------------------------------------------ FUNCTIONS / CONSTRUCTORS
### ---------------------------------
powPar <- function(n, theta = NA, xi = NA, ...){
  ## -----
  ## testing and preparing the input we get
  ## - n: should be anything that can be converted to numeric (logical is allowed but dangerous)
  ## - theta: default NA is only for backward compatibility (use of any name together with theta.name)

  
  ## ---
  dots.eval <- list(...)

  ## ---
  ## handling of theta
  if( all(is.na(theta)) ){
    if( any(names(dots.eval) == "theta.name") ){
      theta.name <- dots.eval[["theta.name"]]
      if( any(names(dots.eval) == theta.name) ){
        theta <- dots.eval[[theta.name]]
      }else{
        stop(paste("You said that the theta.name is \"", theta.name, "\" but there is no argument called", theta.name, sep = ""))
      }
      dots.eval[["theta.name"]] <- NULL
    }else{
      stop("If the argument theta is NA, you should provide a vector of values to be evaluated OR the argument theta.name with the name of the argument that provides the vector of values.")
    }
  }else{
    if( any(names(dots.eval) == "theta.name") ){
      theta.name <- dots.eval[["theta.name"]]
      dots.eval[["theta.name"]] <- NULL
    }else{
      theta.name <- as.character(NA)
    }
  }

  ## ---
  ## handling of xi
  if( all(is.na(xi)) ){
    if( any(names(dots.eval) == "xi.name") ){
      xi <- dots.eval[[xi.name]]
      dots.eval[["xi.name"]] <- NULL
    }else{
      xi.name <- as.character(NA)
      xi <- NA
    }
  }else{
    if( any(names(dots.eval) == "xi.name") ){
      xi.name <- dots.eval[["xi.name"]]
      dots.eval[["xi.name"]] <- NULL
    }else{
      xi.name <- as.character(NA)
    }
  }

  ## ---
  ## generate a new powPar object
  powPar <- new("powPar",
                list = dots.eval,
                theta = round(theta, 10), # there is for sure a nicer solution than rounding here ... the same is true for xi
                theta.name = theta.name,
                xi = round(xi, 10),
                xi.name = xi.name,
                n = as.integer(n),
                n.act = as.integer(NA),
                theta.act = as.numeric(NA),
                xi.act = as.numeric(NA))

  ## ---
  ## set the actual value to the first value in the sequence
  ## (because rounding is done in the previous step this step is separated)
  powPar@theta.act <- powPar@theta[1]
  powPar@xi.act <- powPar@xi[1]
  powPar@n.act <- powPar@n[1]

  ## ---
  return(powPar)
}








### ------------------------------------------------------------------ METHODS
### for users usage (and internal usage)

setMethod("powEx",
          signature = c(x = "powCalc"),
          definition = function(x, theta, xi = NA, endpoint = NA, power = 0.9, drop = 0, method = c("default", "lm", "step"), lm.range = NA, forceDivisor = FALSE){
            ## -----
            ## This method contructs a power-object from a powCalc-object and a powEx-object.
            ## The powEx-object is not visible for the user (this is different from older
            

            ## -----
            ## generating an object of class powEx
            y <- construct.powEx(theta, xi, endpoint, power, drop, method, lm.range, forceDivisor)


            ## -----
            ## testing if powCalc and powEx fit together.
            ## ---
            ## if powCalc has no xi, powEx should neither
            if (all(is.na(x@xi)) & all(!is.na(y@xi.example))){
              warning("The powCalc-object does not make use of xi, but you provide an example for xi. The example for xi will be ignored.")
              y@xi.example <- as.numeric(NA)
            }
            ## ---
            ## do powCalc has the same endpoint names as powEx?
            if (!is.na(y@endpoint.example) && all(x@return.names != y@endpoint.example)) {
              stop(paste("You have chosen an endpoint for the example that is not available, please select one of the following list: ", paste(x@return.names, collapse = ", ")), call. = FALSE)
            }

            ## -----
            ## generating a new power object
            return(new("power", x, y))
          })



setMethod("tex",
          signature(x="power", type = "character"),
          definition = function(x, type = c("drop", "nRec", "nEval", "sampling", "theta", "xi", "n.iter", "power"), ...){
            type <- match.arg(type)
            switch(type,
                   sampling = {paste("$n_{i=1,...,", length(x@n), "} = ",min(x@n), ", ..., ",max(x@n),"$", sep = "")},
                   theta = {x@theta.example},
                   xi = {x@xi.example},
                   n.iter = {ifelse(length(x@iter.example) == 0 || x@iter > x@iter.example, x@iter, x@iter.example)},
                   power = {x@power.example},
                   drop = {paste(round(100*x@drop), "~\\\\%", sep = "")},
                   nRec = {ceiling(sampleSize(x)@estimate/(1-x@drop))},
                   nEval = {ceiling(sampleSize(x)@estimate)},
                   {"undefinded sting provided for the argument type"}) # not used as long as match.arg is used because mach.arg returns with an error if it does not match!
          })



setMethod("pp", signature(x="powPar"), function(x, name){
  ##
  if (name %in% slotNames("power")) {
    slot(x, name)
  } else {
    eval(x@list[[name]])
  }
})



setMethod("n", signature(x = "powPar"), function(x){
  ##
  x@n.act
  ##
})



setMethod("theta", signature(x = "powPar"), function(x){
  ##
  x@theta.act
  ##
})



setMethod("xi", signature(x="powPar"), function(x){
  ##
  x@xi.act
  ##
})



setMethod("dim", signature(x = "powPar"), function(x){
  c(n = length(x@n), theta = length(x@theta), xi = length(x@xi), endpoint = NA)
})



setMethod("dim", signature(x = "powCalc"), function(x){
  c(n = length(x@n), theta = length(x@theta), xi = length(x@xi), endpoint = length(x@return.names))
})



setMethod("plot",
          signature(x = "power", y = "missing"),
          definition = function(x, ...) {
            plot.power(x, ...)
            })

## not sure if in the future it would make sense to draw the calc-obj already...FIXME
## Problems: - for which xi do we draw if there are several?
## technically it would meen: method exDat for powCalc-objects is needed 
## setMethod("plot",
##           signature(x = "powCalc", y = "missing"),
##           definition = function(x, ...) {
##             plot.power(x, example = FALSE, ...)
##             })


setMethod("inspect",
          signature = c(object = "power"),
          definition = function(object){
            invisible(sampleSize(x = object, inspect = TRUE))
          })

setMethod("refine",
          signature(object = "power"),
          definition = function(object, n.iter = 10){

            ## -----
            ## only useful if power object was generated with resampling
            if (object@return.power) {
              stop("Additional iterations for the chosen example are only meaningful if the object was created using resampling.")
            }

            
            ## -----
            ## prepare n.iter
            if (!is.numeric(n.iter) || is.na(n.iter) || n.iter <= 1) {
              stop("The argument <n.iter> shoulde be an integer larger than one that is used for multiplying the number of iterations")
            }
            n.iter <- as.integer(object@iter * n.iter)
            
          
            ## -----
            refinedObj <- workhorse(object,
                                    theta = object@theta == object@theta.example,
                                    n.iter = n.iter)
            refinedObj@iter.example <- n.iter

            ## -----
            return(refinedObj)
          })

setMethod("update",
          signature(object = "powCalc"),
          definition = function(object, ...){
            ## -----
            ## This method evaluates parts or the whole core.
            ## Some changes ask for a complete new evaluation without keeping anything from the past.
            ## If several things are to be changed the sequence of the steps is very important:
            ## 1. first n with old n.iter
            ## 2. then theta with old n.iter
            ## 3. then xi with old n.iter
            ## 4. then all to new n.iter
            ## 5. 
            ## - if any of the steps 1.-3. ask for complete new evaluation (sets new.calc to TRUE) then we skip to step 5.
            ## - if changes to n, theta, xi are provided but nothing needs to be done then any.change is kept to FALSE
            ##   step 4
  
            ## -----
            ## testing and preparing the input we get
            ## - object: not tested because it is in the signature
            ## - n, theta, xi: - only one at the time should be used!
            ##                 - all three must be a vector of logicals
            ##                 - indicating which elements of n, theta and xi should be evaluated
            ## - n.iter: - needs special attention because can be changed but there is no slot
            ##             the corresponding slot is called iter and holds something different
            ##           - is the total number of iterations that should be accieved AFTER running workhorse or NA
            ##           - must be integer of length 1

            dots <- list(...)
            dots.names <- names(dots)
            ## -----
            ## handle keep
            keep <- FALSE
            if (any(dots.names == "keep")) {
              keep <- dots[["keep"]]
              ## removing keep from dots and dots.names
              dots <- dots[which(dots.names != "keep")]
              dots.names <- dots.names[-which(dots.names == "keep")]
              if (!is.logical(keep)) {
                warning("The argument <keep> should be a logical, it will be set to FALSE")
                keep <- FALSE
              } 
            }
            
            # notAlloud <- c("core", "size", "iter")
            allowedSlots <- c("n", "theta", "xi", "statistic", "n.iter") # this list might be longer but other usages are not tested and documented!
            if (any(!dots.names %in% allowedSlots)){
              stop(paste("It is only allowed to update the following elements: ", paste(allowedSlots, collaplse = ", "), "."))
            }


            ## -----
            ## preparing a new object containing all slots from the historic and all changes from the user
            ## the historic object will be used later again!
            newObj <- object
            for (i in seq_along(dots.names)) {
              if( .hasSlot(newObj, dots.names[i]) ){
                if (dots.names[i] == "n") {
                  slot(newObj, dots.names[i]) <- as.integer(dots[[i]])
                } else {
                  slot(newObj, dots.names[i]) <- dots[[i]]
                }
              }
            }

            
            ## -----
            ## preparing n.iter to be used by the workhorse,
            ## see there what is expected:
            if (hasArg("n.iter")) {
              n.iter <- dots[["n.iter"]]
              if (length(n.iter) > 1){
                warning("Only the first element of 'n.iter' is used")
                n.iter <- n.iter[1]
              }
              n.iter <- as.integer(n.iter)
            } else {
              ## that does not meen that no iterations are done
              n.iter <- as.integer(NA)
            }


            ## -----
            ## a helper if any step switches this to TRUE then the workhorse should do a complete new evaluation
            new.calc <- FALSE
            any.change <- FALSE
            

### new statistic
            if ("statistic" %in% dots.names) {
              message("A new <statistic> is provided. All calculations will be done")
              new.calc <- TRUE
              ## updates all elements that are in the class powFun in the existing newObj by running the new statistic
              as(newObj, "powFun") <- powFunGen(newObj, newObj@statistic)
              ## newObj@core <- array(NA, dim = c(dim(newObj)[c("n", "theta", "xi")], newObj@return.n))
              ## newObj <- workhorse(newObj,
              ##                     n.iter = n.iter)
### new n, theta and/or xi
            } else if (any(c("n", "theta", "xi") %in% dots.names)) {
              ## new n
              if ("n" %in% dots.names) {
                message("A new <n> is provided.")
                s <- "n" ## we introduce s that takes now "n" (and later "theta" and "xi")
                message("n")
                if (identical(slot(newObj, s), slot(object, s))) {
                  warning("But it is the same -> No changes will be done.")
                } else if (!any(slot(newObj, s) %in% slot(object, s))) {
                  message("It is entirly different -> All calculations will be done.")
                  new.calc <- TRUE
                  any.change <- TRUE
                }else if (!new.calc & any(!(slot(newObj, s) %in% slot(object, s)))) {
                  message("It contains some available elements -> The new elements will be calculated.")
                  any.change <- TRUE
                  
                  ## generating a object and a vector for giving (existing elements) and taking (existing elements)
                  ## |                          | example | length | class   |
                  ## | old elements (e.g. of n) | xxxxx   |      5 |         |
                  ## | new elements             | ___xxx  |      3 |         | in this example the new n does not have the first tree elements but one additional
                  ## | giveElements             | FFFTT   |      5 | logical |
                  ## | takeElements             |    TTF  |      3 | logical |
                  # we stepwise increase and therefore do not directly work on the newObj
                  # if theta of xi are available we will "rotate", takeObj will then become giveObj, and newObj will become takeObj again etc
                  takeObj <- newObj 
                  giveObj <- object
                  takeElements <- round(slot(newObj, s), 10) %in% round(slot(giveObj, s), 10)
                  giveElements <- round(slot(giveObj, s), 10) %in% round(slot(newObj, s), 10)
                  ##
                  ## generating a core (with NA) of the right dimensions for the new obj (new in this step only!)
                  takeObj@core <- array(NA, dim = c(length(newObj@n), length(giveObj@theta), length(giveObj@xi), newObj@return.n))
                  ## filling in the elements that we keep from the past
                  takeObj@core[takeElements, , , ] <- giveObj@core[giveElements, , , ]
                  ##
                  slot(takeObj, "theta") <- round(slot(giveObj, "theta"), 10)
                  slot(takeObj, "xi") <- round(slot(giveObj, "xi"), 10)
                  ##
                  takeObj <- workhorse(takeObj,
                                       n = !takeElements) # those elements that we could take from the paste do not need to be changed again
                }
              }
              ## new theta
              if (!new.calc & "theta" %in% dots.names) {
                message("A new <theta> is provided.")
                s <- "theta"
                if (identical(slot(newObj, s), slot(object, s))) {
                  warning("But it is the same -> No changes will be done.")
                } else if (!any(slot(newObj, s) %in% slot(object, s))) {
                  message("It is entirly different -> All calculations will be done.")
                  new.calc <- TRUE
                  any.change <- TRUE
                }else if (!new.calc & any(!(slot(newObj, s) %in% slot(object, s)))) {
                  message("It contains some available elements -> Ony the new elements will be calculated.")
                  any.change <- TRUE
                  if ("n" %in% dots.names) { ## ATTENTION not the same as in "n" above
                    giveObj <- takeObj
                    takeObj <- newObj
                  } else {
                    takeObj <- newObj
                    giveObj <- object
                  }
                  takeElements <- round(slot(takeObj, s), 10) %in% round(slot(giveObj, s), 10)
                  giveElements <- round(slot(giveObj, s), 10) %in% round(slot(takeObj, s), 10)
                  ##
                  takeObj@core <- array(NA, dim = c(length(takeObj@n), length(takeObj@theta), length(giveObj@xi), newObj@return.n)) ## ATTENTION not the same as in "n" above
                  takeObj@core[ , takeElements,  , ] <- giveObj@core[ , giveElements, , ]
                  ##
                  slot(takeObj, "xi") <- round(slot(giveObj, "xi"), 10)
                  ##
                  takeObj <- workhorse(takeObj,
                                       theta = !takeElements)
                }
              }
              ## new xi
              if (!new.calc & "xi" %in% dots.names) {
                message("A new <xi> is provided.")
                s <- "xi"
                if (identical(slot(newObj, s), slot(object, s))) {
                  warning("But it is the same -> No changes will be done.")
                } else if (!any(slot(newObj, s) %in% slot(object, s))) {
                  message("It is entirly different -> All calculations will be done.")
                  new.calc <- TRUE
                  any.change <- TRUE
                }else if (!new.calc & any(!(slot(newObj, s) %in% slot(object, s)))) {
                  message("It contains some available elements -> Only the new elements will be calculated.")
                  any.change <- TRUE
                  if (c("theta", "n") %in% dots.names) { ## ATTENTION not the same as in "n" and "theta" above
                    giveObj <- takeObj
                    takeObj <- newObj
                  } else {
                    takeObj <- newObj
                    giveObj <- object
                  }
                  takeElements <- round(slot(takeObj, s), 10) %in% round(slot(giveObj, s), 10)
                  giveElements <- round(slot(giveObj, s), 10) %in% round(slot(takeObj, s), 10)
                  ##
                  takeObj@core <- array(NA, dim = c(length(newObj@n), length(newObj@theta), length(newObj@xi), newObj@return.n)) ## ATTENTION not the same as in "n" and "theta" above
                  takeObj@core[ , , takeElements, ] <- giveObj@core[ , , giveElements, ]
                  ##
                  takeObj <- workhorse(takeObj,
                                       xi = !takeElements)
                }
              }


              ## ---
              ## Now that takeObj has grown across "n", "theta", and "xi" but 
              ## ONLY if it did not step over a new.calc "request" and
              ## ONLY if it did step over any change
              ## otherwise there is no takeObj and we do not want to 
              if (!new.calc & any.change) {
                #print("route A")
                newObj <- takeObj
                
                ## ---
                ## For the "growing" we did not change n.iter but took the historic number
                ## Now we increase n.iter over the whole object
                if (!is.na(n.iter)) {
                  #print("route B")
                  newObj <- workhorse(newObj,
                                      n.iter = n.iter)
                }
              }
              
              
### new calc
            }
            
            ## it is possible to update "n", "theta", "xi" AND do a new.calc but if we do a new.calc
            ## we do not need to do it also for n.iter again.
            if (new.calc) {
              #print("route C")
              ## empty the historic core and fill it again
              newObj@core <- array(NA, dim = c(dim(newObj)[c("n", "theta", "xi")], newObj@return.n))
              newObj <- workhorse(newObj,
                                  n.iter = n.iter)
              
### new n.iter
            } else if (!is.na(n.iter)) {
              #print("route D")
              newObj <- workhorse(object,
                                  n.iter = n.iter)
            }
            return(newObj)
          })



setMethod("powCalc",
          signature(object="powPar"),
          definition = function(object, statistic, n.iter = NA, cluster = 8){

            
            ## -----
            ## testing and preparing the input we get
            ## - object: not tested because it is in the signature
            ## - statistic: tested by powFunGen()
            ## - n.iter
            ## - cluster
            powFun.info <- powFunGen(object, statistic)

            if (length(n.iter) > 1){
              warning("Only the first element of 'n.iter' is used")
              n.iter <- n.iter[1]
            }
            n.iter <- as.integer(n.iter)

            
            ## ----- FIXME
            ## handling the argument cluster
            ## FIXME zumbrunnt: Why not allow cluster computing if n.iter is not specified?
            if (powFun.info@return.power || (is.logical(cluster) && !cluster)){
              cl <- FALSE
            }
            ##
            if (!powFun.info@return.power && (is.integer(cluster) || is.numeric(cluster))) {
              if (length(cluster) > 1){
                warning("Only the first element of cluster will be used.")
                cl <- cluster[1]
              }else{
                cl <- cluster
              }
            }

            ## if a user specifies cluster = TRUE
            if (!powFun.info@return.power && is.logical(cluster) && cluster){
              cl <- 8
            }
            
            ## if 'cluster' is an object of class 'cluster'
            ## THIS does NOT WORK: FIXME
            if (!powFun.info@return.power && inherits(cluster, "cluster")) {
              cl <- TRUE
            }
            


            
            ## -----
            ## constructing an powCalc object with empty core, n.iter, and size
            size.array <-array(NA, dim = c(dim(object)[c("n", "theta", "xi")], powFun.info@return.n))
            power.array <- array(NA, dim = c(dim(object)[c("n", "theta", "xi")], powFun.info@return.n))
            ##
            newCalc <- new("powCalc",
                           size = size.array,
                           core = power.array,
                           iter = as.integer(NA),
                           cluster = list("cluster" = cl), # FIXME
                           powFun.info,
                           object)

            
            ## -----
            ## populate the core of the new object
            filledCalc <- workhorse(newCalc, n.iter = n.iter)

            
            ## -----
            return(filledCalc)
          })




setMethod("show",
          signature(object = "power"),
          definition = function(object) {
            cat("*---objcet of class power------------------------*\n")

            ## parameters that were used for calcualaton
            if(length(object@list) == 0){
                                        # cat("No additional parameters defined.\n")
            }else{
              cat("Additional parameters defined:\n")
              for (i in names(object@list)) {
                if (is.na(object@theta.name)) {
                  cat(paste("   ",i, ": ", eval(pp(object, i)), "\n", sep = ""))
                }else{
                  if (i != object@theta.name) {
                    cat(paste("   ",i, ": ", eval(pp(object, i)), "\n", sep = ""))
                  }
                }
              }
            }
            cat("\n")
            ## power array
            cat("Range and dimensions of the power array:\n")
            cat(paste("       n:  (dim: ", dim(object)[1], ") from ", range(object@n)[1], " to ", range(object@n)[2], "\n", sep = ""))
            cat(paste("   theta:  (dim: ", dim(object)[2], ") from ", range(object@theta)[1], " to ", range(object@theta)[2], "\n", sep = ""))
            
            cat(paste("      xi:  (dim: ", dim(object)[3], ") from ", range(object@xi)[1], " to ", range(object@xi)[2], "\n", sep = ""))
            cat(paste("endpoint: ", paste(object@return.names, collapse = ", ")))
            ## if (all(!is.na(object@xi))){
            ##   cat(paste("      xi: from ", range(object@xi)[1], " to ", range(object@xi)[2], " (dim: ", dim(object)[3], ")\n", sep = ""))
            ## }
            cat("\n")

            
            ## infos about calculations
            if (object@return.power) {
              cat("No iterations used.\n")
            } else {
              cat("Number of iterations:\n")
              cat(paste("  n.iter: ", object@iter, "\n", sep = ""))
              cat("\n")
            }

            
            ## range of power observed
            cat("Range of power observed:\n")
            cat(paste("     Min: ", round(range(object@core, na.rm = TRUE)[1],2), "\n     Max: ",round(range(object@core, na.rm = TRUE)[2],2), "\n"))
            cat("\n")

            
            ## example
            cat("Example: Sample size evaluation for:\n")
            if ( !is.na(object@endpoint.example)) {
              cat(paste("        endpoint: ", object@endpoint.example, "\n", sep = ""))
            }
              cat(paste("           theta: ", object@theta.example, "\n", sep = ""))
              if ( !is.na(object@xi.example)) {
                  cat(paste("            xi: ", object@xi.example, "\n", sep = ""))
              }
              cat(paste("           power: ", object@power.example, "\n", sep = ""))
              cat(paste("   drop out rate: ", object@drop, "\n", sep = ""))
            cat("*------------------------------------------------*\n")
          })







### ------------------------------------------------------------------ METHODS
### that are used only internally - therefore not exported


setMethod("powFunGen",
          signature(object="powPar"),
          definition = function(object, statistic){
  ## -----
  ## This function is only used internally (therefore not exported) to generate objects
  ## of class powFun.

  
  ## -----
  ## testing and preparing the input we get
  ## - object: not tested because it is in the signature
  ## - statistic: tested by new("powFun"... if is class function the rest is done here

  
  ## -----
  ## generating an empty object to be populated later
  x <- new("powFun",
           statistic = statistic)


  ## -----
  ## calling the function to test the output, called return
  statistic.return <- statistic(object)

  
  ## -----
  ## testing the return
  ## - class - logical (in resampling use)
  ##         - atomic or list (in power use) this is tested by the comparison that can only be done for this classes
  ##           by testing if between 0 and 1
  if (is.logical(statistic.return) | (all(statistic.return <= 1) & all(statistic.return >= 0))) {
    if (is.logical(statistic.return)) {
      x@return.power <- FALSE
    } else {
      x@return.power <- TRUE
    }
  } else { # everything else is not allowed
    stop("The function statistic does not return a logical or the power (which should always be numeric and between 0 and 1.")
  }

  
  ## ----- 
  ## preparing data and populating the powFun object
  if ( is.list(statistic.return) ){
    warning("The implementation that allows the statistic to return a list is under construction!") #FIXME
    x@return.list <- TRUE
    x@return.n <- length(statistic.return$power)
    x@return.names <- names(statistic.return$power)
    ## n.size <- length(statistic.return$size)
    ## size.name <- names(statistic.return$size)
    ## if (is.null(size.name)) {size.name <- paste("n", seq(1, n.size), sep = "")}
    ## ##
    ## if (!is.logical(statistic.return$power) & !powFun.info@return.power) {
    ##   warning("The function statistic does not return a logical vector for the list element power. I assume it is the power and will ignore your n.iter.")
    ##   n.iter <- as.integer(NA)
    ## }
    ## ##
    ## if (powFun.info@return.power){
    ##   size.array <- array(NA, dim = c(dim(object), n.size))
    ## } else {
    ##   size.array <- array(NA, dim = c(dim(object), n.size, n.iter))
    ## }
  } else {
    x@return.list <- FALSE
    x@return.n <- length(statistic.return)
    if (is.null(names(statistic.return))) {
      ## we give a name to the endpoints if not provided
      x@return.names <- paste("ep", seq(1, x@return.n), sep = "")
    } else {
      x@return.names <- names(statistic.return)
    }
  }

  
  ### -----
  return(x)
})
              
            

setMethod("workhorse",
          signature(object="powCalc"),
          definition = function(object, n = NULL, theta = NULL, xi = NULL, n.iter = NA){
            ## -----
            ## This method is only used internally to populate the core and n.iter of powCalc objects
            
  
            ## -----
            ## no test! expecting the correct input !
            ## - object: not tested because it is in the signature
            ## - n, theta, xi: - only one at the time should be used!
            ##                 - all three must be a vector of logicals
            ##                 - indicating which elements of n, theta and xi should be evaluated
            ## - n.iter: - is the total number of iterations that should be accieved AFTER running workhorse
            ##           - must be integer of length 1
            ## - cluster: - must come correct from object as object cluster or not

            
            ## -----
            ## preparing input
            ## same three lines of code as in update function
            if (is.null(n)) {n <- rep(TRUE, length(object@n))}
            if (is.null(theta)) {theta <- rep(TRUE, length(object@theta))}
            if (is.null(xi)) {xi <- rep(TRUE, length(object@xi))}


            ## -----
            ## preparing n.iter and relatives
            ## ... is done at this stage (and not in preparation, because available.iter is needed here)
            ## before this preparation:
            ##    n.iter: as provided to workhorse
            ##    object@iter: number of iterations in previous runs
            ##    available.iter: not existing
            if ( !object@return.power ) { 
              if (!is.na(n.iter)) {
                n.iter <- as.integer(n.iter)
                if ( !is.na(object@iter)) {
                  ## this is the case if objcect is updated only
                  available.iter <- object@iter ## how many iterations were done already
                  n.iter <- n.iter - available.iter ## how many iterations are we doing in this run
                  object@iter <- as.integer(available.iter + n.iter) ## how many after this run
                } else {
                  ## this is the case for new objects only 
                  available.iter <- as.integer(0)
                  object@iter <- n.iter
                }
              } else {
                ## n.iter is not provided for this run but available in the object
                ## this is the case for an update where n.iter is not changed
                ## in this case we use the same n.iter for the new objcects
                if ( !is.na(object@iter)) {
                  n.iter <- object@iter
                  available.iter <- as.integer(0)
                }
              }
            }
            ## after this preparation:
            ##    n.iter: the number of iterations for this run!
            ##    object@iter: how many iterations will be available after this run
            ##    available.iter: number of iterations available that are extended to n.iter


            ## -----
            ## the preparation is done in powCalc
            ## only using cluster if several n are needed
            with.cluster <- FALSE
            if ( sum(n) > 1 ){
              if ( is.numeric(object@cluster[[1]]) || (is.logical(object@cluster[[1]]) && object@cluster[[1]])) {
                cl <- makeCluster(detectCores())
                clusterEvalQ(cl, library(parallel))
                clusterEvalQ(cl, library(sse))
#                clusterEvalQ(cl, library(someR))
                with.cluster <- TRUE
              }
            }
            ## -----
            ## some extractions for simpler use (does this speed up?)
            power.array <- object@core
            statistic <- object@statistic

            ## -----
            ## 
            ## FIXME size.array <-


            ## -----
            ##            with.cluster <- TRUE # FIXME

            
### --- cluster
### --- resampling (automatically if with.cluster)
            if (with.cluster){
              message(paste("using cluster"))
              
              ## -----
              ## this step is not needed but speeds up a lot compared to applying the parSapply directly
              power.fun.resample <- function(statistic, object, n.iter){
                sig <- replicate(n.iter,
                                 statistic(object)
                                 )
                if(is.vector(sig)){ # this is the case if there is only one endpoint
                  sig <- matrix(sig, ncol = length(sig), dimnames = list(names(sig[1])))
                }
                return(apply(sig, 1, function(x) sum(x, na.rm = TRUE) / length(x[!is.na(x)])))
              }

              ## pos. :integer used for indexing within the core array
              pos.n <- seq(along = object@n)
              pos.theta <- seq(along = object@theta)
              pos.xi <- seq(along = object@xi)
              ## taking into account n, theta and xi we loop through all positions in the array
              ## that should be evaluated in this run
              for (theta.i in pos.theta[theta]) {
                for (xi.i in pos.xi[xi]) {
                  object@theta.act <- object@theta[theta.i]
                  object@xi.act <- object@xi[xi.i]
                  objects <- list()
                  i <- 1
                  for (n.i in pos.n[n]) {
                    object@n.act <- object@n[n.i]
                    objects[[i]] <- object
                    i <- i + 1
                  }
                  power.array[pos.n[n], pos.theta[theta.i], pos.xi[xi.i], ] <-
                    t(parSapply(cl,
                                objects,
                                function(x) power.fun.resample(statistic, x, n.iter)))
                  size.array <- array(NA)
                  message(paste("theta:", object@theta.act, Sys.time()))
                }
              }
              if (!all(is.na(object@core)) & available.iter != 0 ) {
                ## if iterations available from older runs they are use
                for (ep in object@return.n) { ## ?? is this loop still needed, when?
                  power.array[,,,ep] <- (object@core[,,,ep, drop = FALSE] * available.iter +  power.array[,,,ep, drop = FALSE] * n.iter) / object@iter
                }
              }
                stopCluster(cl)
            } else { # with.cluster
### --- no-cluster
              message(paste("not using cluster"))

              
### --- simple
            if (!object@return.list) {

              
### --- resampling
              if ( !object@return.power ){
                ## pos. :integer used for indexing within the core array
                pos.n <- seq(along = object@n)
                pos.theta <- seq(along = object@theta)
                pos.xi <- seq(along = object@xi)
                ## taking into account n, theta and xi we loop through all positions in the array
                ## that should be evaluated in this run
                for (n.i in pos.n[n]){
                  for (theta.i in pos.theta[theta]){
                    for (xi.i in pos.xi[xi]){
                      object@n.act <- object@n[n.i]
                      object@theta.act <- object@theta[theta.i]
                      object@xi.act <- object@xi[xi.i]
                      stat.eval <- vapply(X = seq(length.out = n.iter),
                                          FUN = function(x){statistic(object)},
                                          FUN.VALUE = logical(object@return.n))
                      if (object@return.n == 1){
                        power.array[pos.n[n.i], pos.theta[theta.i], pos.xi[xi.i], ] <- sum(stat.eval, na.rm = TRUE) / length(stat.eval[!is.na(stat.eval)])
                      } else {
                        power.array[pos.n[n.i], pos.theta[theta.i], pos.xi[xi.i], ] <- apply(stat.eval, 1, function(x) sum(x, na.rm = TRUE) / length(x[!is.na(x)]))
                      }
                      size.array <- array(NA) ## FIXME
                    } # xi.loop
                  } # theta.loop
                  message(paste("n:", object@n.act, Sys.time()))
                } # n.loop
                if (!all(is.na(object@core)) & available.iter != 0 ) {
                  ## if iterations available from older runs they are use
                  for (ep in object@return.n) { ## ?? is this loop still needed, when?
                    power.array[,,,ep] <- (object@core[,,,ep, drop = FALSE] * available.iter +  power.array[,,,ep, drop = FALSE] * n.iter) / object@iter
                  }
                }
                
### --- power
              } else {
                pos.n <- seq(along = object@n)
                pos.theta <- seq(along = object@theta)
                pos.xi <- seq(along = object@xi)
                for (n.i in pos.n[n]){
                  for (theta.i in pos.theta){
                    for (xi.i in pos.xi){
                      object@n.act <- object@n[n.i]
                      object@theta.act <- object@theta[theta.i]
                      object@xi.act <- object@xi[xi.i]
                      ##
                      stat.eval <- statistic(object) # for debugging it may help to change to stat.eval <<- statistic(object)
                      power.array[pos.n[n.i], pos.theta[theta.i], pos.xi[xi.i], ] <- stat.eval
                      size.array <- array(NA) ## FIXME
                    } # xi.loop
                  } # theta.loop
                  message(paste("n:", object@n.act, Sys.time()))
                } # n.loop
              }
            } # return.list
            } # with.cluster

            
            ## -----
            ## populate core and return object
            object@core <- power.array
            return(object)
          })

setMethod("exDat",
          signature = c(x = "power"),
          definition = function(x, y, ...){

            ## determin the index for xi
            xi.example <- x@xi.example
            if (is.na(xi.example)) {
              xi.example.integer <- 1
            } else {
              xi.example.integer <- which(xi.example == x@xi)
            }

            ## determin the index for endpoint
            endpoint.example <- x@endpoint.example
            if (is.na(endpoint.example)) {
              endpoint.example.integer <-  1
            } else {
                if(all(endpoint.example != x@return.names))stop(paste("You have chosen an endpoint name that is not available, please select one of the following list: ", paste(x@return.names, collapse = ", ")), call. = FALSE)
              endpoint.example.integer <- which(endpoint.example == x@return.names)
            }

            ## taking the subset of core
            dat <- data.frame(sample.size = rep(pp(x, "n"), times = dim(x)[2]),
                              theta = rep(pp(x, "theta"), each = dim(x)[1]),
                              power = c(x@core[ , , xi.example.integer, endpoint.example.integer]))
            return(dat)
          })


### ---------------------------------
setMethod("sampleSize",
          signature = c(x = "power"),
          definition = function(x, inspect = FALSE, ...){
            ## -----
            ## 

            
            ## -----
            ## extracting slots and making the elements easily available
            power.example <- x@power.example
            theta.example <- x@theta.example
            method <- x@method
            lm.range <- x@lm.range
            n.vec <- x@n
            return.power <- x@return.power
            forceDivisor <- x@forceDivisor
            divisor <- x@divisor

            
            ## -----
            ## set the default method
            if (method == "default") {
              if (return.power) {
                method <- "step"
              } else {
                method <- "lm"
              }
            }

            
            ## -----
            ## extracting the data from object of class power and
            ## providing a data.set with
            ## - sample.size
            ## - power
            ## - theta
            dat <- exDat(x)


            ## -----
            ## testing if parts from class powEx and powCalc (that were merged to class power) fit together.
            ## ?? should this be moved to example()?
            if (max(dat$power, na.rm=TRUE) < min(power.example, na.rm=TRUE) | min(dat$power, na.rm=TRUE) > max(power.example, na.rm=TRUE)) {
              stop(paste("The power of the example is outside of the observed range for the whole parameter space. The observed range is: ", round(min(dat$power, na.rm=TRUE), 2), "to" , round(max(dat$power, na.rm=TRUE),2), ". There will be no example." , sep = ""), call. = FALSE)
            }


            
            ## -----
            ## generating subset for the example
            dat.example <- dat[dat$theta == theta.example & dat$power > 0 & dat$power < 1, ]
            ##
            if (max(dat.example$power, na.rm=TRUE) < min(power.example, na.rm=TRUE) | min(dat.example$power, na.rm=TRUE)>max(power.example, na.rm=TRUE)) {
              warning(paste("The power of the example is outside of the observed range. The observed range is: ", round(min(dat.example$power, na.rm=TRUE), 2), "to" , round(max(dat.example$power, na.rm=TRUE),2), ". There will be no example." , sep = ""), call. = FALSE)
             method <- "na" # by setting the method to na sample size is not estimated but set to NA
            }

            
            ## -----
            transform <- function(x) {(qnorm(x) + qnorm(1 - 0.05))^2} #used in older versions to be: 0.5 * log((1 + x) / (1 - x))
#            untransform <- function(y) (exp(2 * y) -1)/(1 + exp(2 * y)) # FIXME look for backtransformation


            ## -----
            m.lm <- lm(sample.size ~ transform(power), data = dat.example)
            ##
### it is a problem if all data available is used for calculating the sample size (trade-off, taking only neighbours or taking all...)
            switch(method,
                   ## linear model
                   "lm" = {
                     power.borders <- (1 + c(-1, 1) * lm.range) * power.example
                     ## to allow transform transformation on power.borders:
                     power.borders[power.borders > 1] <- 0.99
                     power.borders[power.borders < 0] <- 0
                                        # a vector of length to indicating the border for the power range used for fitting a linear model and estimating the sample size
                     dat.example.range <- dat.example[dat.example$power > power.borders[1] & dat.example$power < power.borders[2], ]
                     ##
                     m.lm.range <- lm(sample.size ~ transform(power), data = dat.example.range)
                     p.lm <- predict(m.lm.range, interval = "confidence", newdata = data.frame(power = c(power.example, power.borders)))
                     sample.size <- ceiling(p.lm[1, 1])
                     message(paste("estimator: ", sample.size,
                               "\n95%CI: [", paste(round(p.lm[1, c("lwr", "upr")]), collapse = "; " ), "]\n"))
                     ## forceDivisor handling
                     if ( forceDivisor ){
                       if ( is.na(divisor) ){
                         ## find out the greatest common divisor
                         divisor <- 1
                         for ( i in 2:min(n.vec) ) {
                           divisor <- ifelse(all(n.vec %% i == 0), i, divisor)
                         }
                       }
                       if ( sample.size%%divisor ) {
                         sample.size <- sample.size + (divisor - sample.size%%divisor)
                         message(paste("Returning ", sample.size, " instead of the estimator to achieve a divisibility with the divisor ", divisor,".", "\n", sep = ""))
                       }
                     }
                     mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                       suppressWarnings(panel.loess(x, y, span = 0.75, degree = 2, family = "gaussian", col = "blue", ...))
                       ## panel.abline(m.lm.range$coef, col = "red")
                       panel.lines(x = c(transform(power.borders)), y = p.lm[2:3, 1], col = "red", lty = 1, lwd = 1.5)
                       panel.abline(m.lm$coef, col = "blue", lty = 2)
                       ## showing the chosen power
                       panel.text(x = min(transform(power.example)), y = min(dat.example$sample.size), labels = paste(" power = ", round(power.example, 2), sep = ""), adj = c(-0.05, -0.05), srt = 90, col = "grey")
                       panel.abline(v = transform(power.example), col = "gray")
                       ## showing the chosen sample size
                       panel.abline(h = sample.size, col = "gray")
                       panel.text(x = min(transform(dat.example$power)), y = sample.size, labels = paste(" sample size = ", sample.size, sep = ""), adj = c(-0.05, -0.05), col = "grey")
                       ## showing the method
                       panel.text(x = min(transform(dat.example$power)), y = max(y), labels = paste(" method: ", method, " (lm.range = ", lm.range, ")", sep = ""), adj = c(-0.05, 1.1), col = "grey")
                     }
                   },
                   "step" = {
                     element <- tail(which(dat.example$power < power.example), 1) + 1
                     sample.size <- dat.example$sample.size[element]
                     ## for the inspection plot we need a line (based on all data)
                     m.lm <- lm(sample.size ~ transform(power), data = dat.example)
                     library(grid)
                     mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                       ## suppressWarnings because this is for visualisation only.
                       suppressWarnings(panel.loess(x, y, span = 0.75, degree = 2, family = "gaussian", col = "blue", ...))
                       panel.abline(m.lm$coef, col = "blue", lty = 2)
                       panel.abline(v = transform(power.example), col = "gray")
#                       grid.text(label = paste("power = ", round(power.example, 2), sep = ""), x = transform(power.example), y = unit(0.15, "npc"), just = c(-0.01, 0.5))
                       ## showing the chosen power
                       panel.text(x = min(transform(power.example)), y = min(dat.example$sample.size), labels = paste(" power: ", round(power.example, 2), sep = ""), adj = c(-0.05, -0.05), srt = 90, col = "grey")
                       panel.points(x = transform(dat.example$power[element]), y = dat.example$sample.size[element], col = "red", pch = "*", cex = 4)
                       ## showing the chosen sample size
                       panel.abline(h = sample.size, col = "gray")
                       panel.text(x = min(transform(dat.example$power)), y = sample.size, labels = paste(" sample size: ", sample.size, sep = ""), adj = c(-0.05, -0.05), col = "grey")
                       ## showing the method
                       panel.text(x = min(transform(dat.example$power)), y = max(y), labels = paste(" method: ", method, sep = ""), adj = c(-0.05, 1.1), col = "grey")
                     }
                   },
                   "na" = {
                     sample.size <- NA
                      mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                     }
                   }
                   )
### loess
###############
### this part is not used and only here fore historical reasons
            ## if (type == "loess"){
            ##   span = 0.05
            ##   m.loess <- loess(sample.size ~ transform(power), span = span,
            ##                    data = dat.example)
            ##   p.loess <- predict(m.loess, newdata = data.frame(power = power.example))
            ##   sample.size <- ceiling(p.loess)
            ##   cat(paste("estimator: ", sample.size,  "\n"))
            ## }
            if (inspect) {
              print(xyplot(sample.size ~ transform(power), data = dat.example,
                           xlab = "power (transformed)",
                           ylab = "sample size",
                           panel = mypanel
                           ))
            }
            return(new("SampleSize", estimate = as.integer(sample.size)))
          })



### ------------------------------------------------------------------ METHODS
### only for backard compatibility only!
## setMethod("merge",
##           signature = c(x = "powCalc", y = "powEx"),
##           definition = function(x, y){
##             warning("This function is only available for backward compatibility. Use the function example instead of powEx and merge.")
##               ## do powCalc and powex fit together?
##               ## if powCalc has no xi, powEx should neither
##               if (all(is.na(x@xi)) & all(!is.na(y@xi.example))){
##                   warning("The powCalc-object does not make use of xi, but powEx provides an example for xi. The example for xi provided will be ignored.")
##                   y@xi.example <- as.numeric(NA)
##             }
##               ## do powCalc has the same endpoint names as powEx?
##               if(!is.na(y@endpoint.example) && all(x@return.names != y@endpoint.example))stop(paste("You have chosen an endpoint name for the example that is not available, please select one of the following list: ", paste(x@return.names, collapse = ", ")), call. = FALSE)
##               ## ---
##               new("power", x, y)
##           })



### ------------------------------------------------------------------ FUNCTIONS
### only for internal usage - not exported

### ---------------------------------
construct.powEx <- function(theta, xi = NA, endpoint = NA, power = 0.9, drop = 0, method = c("default", "lm", "step"), lm.range = NA, forceDivisor = FALSE) {
  ## -----
  ## This function generats an object of class powEx.

  
  ## -----
  ## testing and preparing the input we get
  ## - theta: is not tested 
  ## - xi:
  ## - endpoint:
  ## - power:
  ## - drop:
  ## - method: not tested but handled by match.arg (other tests depend on method -> handled first)
  ## - lm.range:
  ## - forceDivisor:

  
  ## --- method
  method <- match.arg(method)
  ## --- theta
  if (length(theta) > 1){
    warning("The argument <theta> should have a length of one. Only the first element is used")
    theta <- theta[1]
  }
  ## --- xi
  if (length(xi) > 1){
    warning("The argument <xi> should have a length of one. Only the first element is used")
    xi <- xi[1]
  }
  ## --- endpoint
  if (length(endpoint) > 1){
    warning("The argument <endpoint> should have a length of one. Only the first element is used")
    endpoint <- endpoint[1]
  }
  ## --- power
  if (length(power) > 1){
    warning("The argument <power> should have a length of one. Only the first element is used")
    power <- power[1]
  }
  if (power <= 0 | power >=1){
    stop("The argument <power> should be between 0 and 1")
  }
  ## --- drop
  if (length(drop) > 1){
    warning("The argument <drop> should have a length of one. Only the first element is used")
    drop <- drop[1]
  }
  if (drop < 0 | drop >=1){
    stop("The argument <drop> should be between 0 and 1")
  }
  ## --- lm.range
  if (method == "step"){
    if (!is.na(lm.range)){
      warning("If the method 'step' is selecte, the argument <lm.range> will not be use. ")
    }
    lm.range <- as.numeric(NA)
  } else {
    ## for method "default" or "lm" we set the lm.range to 0.2 or if provided test it.
    if (is.na(lm.range)) {
      lm.range <- 0.2
    } else {
      if (length(lm.range) > 1){
        warning("The argument <lm.range> should have a length of one. Only the first element is used")
        lm.range <- lm.range[1]
      }
      if (lm.range < 0 | lm.range > 1){
        stop("The argument <lm.range> should be in the range [0; 1]")
      }
    }
  }
  ## --- forceDivisor 
  ## handle forceDivisor and generate divisor
  divisor <- as.integer(NA)
  if (is.logical(forceDivisor)) {
    if (forceDivisor){
      ## if forceDivisor is TRUE we set the divisor to the default 2
      divisor <- as.integer(2)
    }
  } else {
    ## if forceDivior is numeric we set forceDivisor to TRUE
    if (is.numeric(forceDivisor)) {
      if (forceDivisor <= 0) {
        stop("If a divisor should be used the argument <forceDivisor> should get a positive number or TRUE (which uses the default divisor of 2).")
      }
      divisor <- as.integer(forceDivisor)
      if (forceDivisor != divisor) {
        warning(paste("The argument <forceDivisor> got ", forceDivisor, " but the integer", divisor, " will be used"))
      }
      forceDivisor <- TRUE
    } else {
      stop("The arguemnt <forceDivisor> should get an integer number or TRUE.")
    }
  }

    
  ## -----
  ## populating a new powEx object
  new("powEx",
      theta.example = theta,
      xi.example = as.numeric(xi),
      endpoint.example = as.character(endpoint),
      power.example = power,
      drop = drop,
      method = method,
      lm.range = lm.range,
      forceDivisor = forceDivisor,
      divisor = divisor)
}



### --------------------------------- FIXME (no changes done when 
plot.power <- function(x, at = c(0.8, 0.85, 0.9, 0.95), smooth = FALSE, example = TRUE, label.pos = 0.75, reflines = TRUE, symbol = bquote(theta), ...){ # the argument label.pos is not used yet
  object <-  x
  
  ## -----
  ## some tests
  ## only plot if dimenstion are ok
  if (any(dim(object)[1:2] < 2)) {
    ## sample.size <- sampleSize(object) # this allows the programmer to see the estimated sample size
    stop("For creating a power plot the vectors of theta and n (defined in the powPar-object) should have at least a length of two.", call. = FALSE)
  }

  
  ##
  dat <- exDat(object)

  
  # ---
  ## handling the smooth-argument
  if (is.logical(smooth) & smooth) {
    span <- 0.75
    dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, "power"] <- fitted(loess(power ~ theta + sample.size, data=dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, ], span=span))
  }
  if (is.numeric(smooth)) {
    if (length(smooth) > 1) {
      warning("Only the first element of <smooth> is used.")
      smooth <- smooth[1]
    }
    if(smooth <= 0) {
      stop("The argument <smooth> has to be larger than 0, have a look at the argument span in ?loess")
    }
    span <- smooth
    transform <- function(x) {(qnorm(x) + qnorm(1 - 0.05))^2}
    untransform <- function(y) {pnorm(sqrt(y) - qnorm(1 - 0.05))} #(exp(2 * y) -1)/(1 + exp(2 * y))
    fitted.transform <- fitted(loess(transform(power) ~ theta + sample.size,
                                  data = dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, ],
                                  span = span))
    dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, "power"] <- untransform(fitted.transform)
  } 

  
  ## ---
  ## handling the example
  if (example){
    sample.size <- sampleSize(object)@estimate
    if (is.na(sample.size)) {
      ## this is TRUE if the range of power for the theta.example does not include power.example
      example <- FALSE
    }
    power.example <- object@power.example
    theta.example <- object@theta.example
  }

  
  ## ---
  ## handling the at-argument
  if(!is.numeric(at)){
    stop("The argument \"at\" has to be numeric")
  }
  if(any(at <= 0) | any(at >= 1)){
    stop("The argument \"at\" has to be between of 0 and 1")
  }
  if (example){
    ## sorting at that the power.example is the first element
    at <- at[c(which(round(at,10) == object@power.example), which(round(at,10) != object@power.example))]
  }
  at.main <- c(at[1], at[1]*1.00000000001)
  at.second <- at[-1]
  if(length(at.second==1)){ at.second=c(at.second, at.second*1.0000000001)}
  rm(at)

  
  ## ---
  ## handling the xlim and ylim-argument
  ## (the default handling does not allow a nice control of the labeling and of the example)
  dots.eval <- list(...)
  if( any(names(dots.eval) == "xlim") ){
    xlim <- dots.eval[["xlim"]]
    dat <- dat[dat$theta >= xlim[1] & dat$theta <= xlim[2], ]
    if (example) {
      if (theta.example < xlim[1] | theta.example > xlim[2]) {
        warning("The example is outsite the range the ploting range and will not be used.")
        example <- FALSE
      }
    }
  }
 if( any(names(dots.eval) == "ylim") ){
    ylim <- dots.eval[["ylim"]]
    dat <- dat[dat$sample.size >= ylim[1] & dat$sample.size <= ylim[2], ]
    if (example) {
      if (sample.size < ylim[1] | sample.size > ylim[2]) {
        warning("The example is outsite the range the ploting range and will not be used.")
        example <- FALSE
      }
    }
  }


  
  ##
  ## xscale.components.theta <- function(lim, ...){
  ##     ans <- xscale.components.default(lim = lim, ...)
  ##     tick.at <- round(ans$bottom$ticks$at,10)
  ##     cat(tick.at)
  ##     cat("\n")
  ##     theta.tick.at <- round(object@theta,10)
  ##     cat(theta.tick.at)
  ##           cat("\n")
  ##     major <- theta.tick.at %in%  tick.at
  ##     cat(major)
  ##           cat("\n")
  ##     ans$bottom$ticks$at <- theta.tick.at
  ##     tck <- rep(0.75, length.out = length(theta.tick.at))
  ##     tck[major] <- 1.5
  ##     ans$bottom$ticks$tck <- tck# ifelse(major, 1.5, 0.75)
  ##     ans$bottom$labels$at <- tick.at
  ##     ans$bottom$labels$labels <- as.character(tick.at)
  ##     print(ans)
  ##     ans
  ## }

  ## -----
  ## creating the power plot
  cp <- contourplot(power ~ theta + sample.size,
                    data = dat,
                    ...,
                    ## xscale.components = xscale.components.theta,
                    ## scales = list(x = list(tick.number = 10)),
                    ##
                    panel = function(x,y,z,at,labels, ...){
                        ##
                        if (reflines){
                            panel.fill(col = grey(0.9))
                            ##
                            panel.refline(v = object@theta, col = grey(1))
                            panel.refline(h = object@n, col = grey(1))
                        }
                      ## drawing the thick line
                      panel.contourplot(x, y, z,
                                        at = at.main,
                                        cut = 2,
                                        lwd = 4,
                                        col = grey(0.6),
                                        label.style = "align",
#                                        label.style = "free", label.pos = label.pos, # perhaps for the next version instead of previous line
                                        labels = list(cex = 1,
                                                      labels = paste("Power = ", round(at.main,3), sep = ""),
                                                      col = grey(0),
                                                      adj = c(0.5, -0.5)),
                                        ...)
                      ## label.style causes often a conflict with the arrows ... experience shows that "align" here and "mixed" for the others is probably best, perhaps the user choice would make sense
                      if(length(at.second) >= 1){
                        ## drawing the thin line(s)
                        panel.contourplot(x, y, z,
                                          at = at.second,
                                          cut=2,
                                          lwd = 1,
                                          col = grey(0.4),
                                          label.style = "align",
                                          ##  label.style = "free", label.pos = label.pos,
                                          labels = list(labels = as.character(round(at.second,3)),
                                                        col = grey(0),
                                                        adj = c(0.5, -0.25)),
                                          ...)
                      }
                      ## adding the example
                      if (example){
                        grid.lines(x = unit(c(theta.example, theta.example), "native"),
                                   y = unit(c(0, sample.size), c("npc","native")) + unit(c(0.02, -0.02), c("npc", "npc")),
                                   arrow = arrow(length=unit(0.02, "npc")))
                        grid.lines(x = unit(c(theta.example, 0), c("native","npc")) + unit(c(-0.02, 0.02), c("npc", "npc")),
                                   y = unit(c(sample.size, sample.size), "native"),
                                   arrow = arrow(length = unit(0.02, "npc")))
                        grid.text(label = paste("N=", sample.size, sep = ""),
                                  x = unit(0.05, "npc"),
                                  y = unit(sample.size, "native"),
                                  just = c(0, -0.2))
                        grid.text(label = bquote(paste(.(symbol), "=", .(theta.example), sep = "")),
                                  y = unit(0.1, "npc"),
                                  x = unit(theta.example, "native"),
                                  just = c(0.5, -0.2),
                                  rot = 90)
                        grid.points(x = unit(theta.example, "native"),
                                    y = unit(sample.size, "native"),
                                    pch = 20,
                                    size = unit(0.7,"char"))
                      }
                    }
                    )

  ##
  class(cp) <- "trellis"
  print(cp)
}
