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
                        method = "character",
                        lm.range = "numeric",
                        drop = "numeric",
                        forceDivisor = "logical",
                        divisor = "integer"))
##
setClass("Resample",
         representation(n.iter = "integer"))
##
setClass("powCalc",
         representation(core="array",
                        endpoint.name = "character",
                        statistic = "function"),
         contains = c("Resample", "powPar"))
##
setClass("SampleSize",
         representation(estimate = "integer"))
##
setClass("power",
         contains = c("powCalc", "powEx"))

## validity=function(object){
##   if(length(object@n)!=dim(object@core)[1]) return("the slot \"n\" does not correspond to the dimention of the core")
##   if(length(object@theta)!=dim(object@core)[2]) return("the slot \"theta\" does not correspond to the dimention of the core")
##   if(length(object@xi)!=dim(object@core)[3]) return("the slot \"xi\" does not correspond to the dimention of the core")
##   if(length(object@endpoint)!=dim(object@core)[4]) return("the slot \"endpoint\" does not correspond to the dimention of the core")



## ------------------------------------------------------------------ CONSTRUCTORS
powPar <- function(n, theta = NA, xi = NA, ...){
  dots <- as.list(substitute(list(...)))[-1]
  dots.eval <- sapply(dots, eval)
  dots.eval <- as.list(dots.eval)
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
      stop("If the argument theta is NA, you should provide the argument theta.name with the name of the argument that provides the vector of values to evaluate.")
    }
  }else{
    if( any(names(dots.eval) == "theta.name") ){
      theta.name <- dots.eval[["theta.name"]]
      dots.eval[["theta.name"]] <- NULL
    }else{
      theta.name <- as.character(NA)
    }
  }
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
  ##
  powPar <- new("powPar",
                list = dots.eval,
                theta = round(theta, 20), # there is for sure a nicer solution than rounding here ... the same is true for xi
                theta.name = theta.name,
                xi = round(xi, 20),
                xi.name = xi.name,
                n = as.integer(n),
                n.act = as.integer(NA),
                theta.act = as.numeric(NA),
                xi.act = as.numeric(NA))
  powPar@theta.act <- powPar@theta[1]
  powPar@xi.act <- powPar@xi[1]
  powPar@n.act <- powPar@n[1]
  return(powPar)
}

powEx <- function (theta, xi = NA, endpoint = NA, power = 0.9, drop = 0, method = c("default", "lm", "step"), lm.range = 0.2, forceDivisor = FALSE) {
  method <- match.arg(method)
  ## handle the divisor
  divisor <- as.integer(NA)
  if ( is.numeric(forceDivisor) ){
    if ( forceDivisor <= 0 ) warning("If a divisor should be used the argument forceDivisor should get a positive number")
    divisor <- as.integer(forceDivisor)
    forceDivisor <- TRUE
  }
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

### ---------------------------------

setMethod("sampleSize",
          signature = c(x = "power"),
          definition = function(x, inspect = FALSE, ...){
            ##
            power.example <- x@power.example
            theta.example <- x@theta.example
            method <- x@method
            lm.range <- x@lm.range
            n.vec <- x@n
            n.iter <- x@n.iter
            forceDivisor <- x@forceDivisor
            divisor <- x@divisor
            ##
            ## handling the default method
            if ( method == "default" ){
              if ( is.na(n.iter) ){method <- "step"}else{method <- "lm"}
            }
            ##
            dat <- exDat(x)
            dat.example <- dat[dat$theta == theta.example & dat$power > 0& dat$power < 1, ]
            ##
            if (max(dat$power, na.rm=TRUE) < min(power.example, na.rm=TRUE) | min(dat$power, na.rm=TRUE)>max(power.example, na.rm=TRUE)) {
              stop(paste("The power of the example is outside of the power range observed. The range is: ", round(min(dat$power, na.rm=TRUE), 2), "to" , round(max(dat$power, na.rm=TRUE),2), ". There will be no example." , sep = ""))
              example <- FALSE
            }
            ##
            fisher <- function(x) 0.5 * log((1 + x) / (1 - x))
            unfisher <- function(y) (exp(2 * y) -1)/(1 + exp(2 * y))
            ##
            m.lm <- lm(sample.size ~ fisher(power), data = dat.example)
            ##
### it is a problem if all data available is used for calculating the sample size (trade-off, taking only neighbours or taking all...)
            switch(method,
                   ## linear model
                   "lm" = {
                     power.borders <- (1 + c(-1, 1) * lm.range) * power.example
                     ## to allow fisher transformation on power.borders:
                     power.borders[power.borders > 1] <- 0.99
                     power.borders[power.borders < 0] <- 0
                                        # a vector of length to indicating the border for the power range used for fitting a linear model and estimating the sample size
                     dat.example.range <- dat.example[dat.example$power > power.borders[1] & dat.example$power < power.borders[2], ]
                     ##
                     m.lm.range <- lm(sample.size ~ fisher(power), data = dat.example.range)
                     p.lm <- predict(m.lm.range, interval = "confidence", newdata = data.frame(power = c(power.example, power.borders)))
                     sample.size <- ceiling(p.lm[1, 1])
                     cat(paste("estimator: ", sample.size,
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
                         cat(paste("Returning ", sample.size, " instead of the estimator to achieve a divisibility with the divisor ", divisor,".", "\n", sep = ""))
                       }
                     }
                     
                     mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                       panel.loess(x, y, span = 0.75, degree = 2, family = "gaussian", col = "blue", ...)
                       ## panel.abline(m.lm.range$coef, col = "red")
                       panel.lines(x = c(fisher(power.borders)), y = p.lm[2:3, 1], col = "red", lty = 1, lwd = 1.5)
                       panel.abline(m.lm$coef, col = "blue", lty = 2)
                       ## showing the chosen power
                       panel.text(x = fisher(power.example), y = mean(dat.example$sample.size), labels = paste(" power = ", round(power.example, 2), sep = ""), adj = c(0.5, 0), srt = 90, col = "grey")
                       panel.abline(v = fisher(power.example), col = "gray")
                       ## showing the chosen sample size
                       panel.abline(h = sample.size, col = "gray")
                       panel.text(x = mean(fisher(dat.example$power)), y = sample.size, labels = paste(" sample size = ", sample.size, sep = ""), adj = c(0, -0.1), col = "grey")
                     }
                   },
                   "step" = {
                     element <- tail(which(dat.example$power < power.example), 1) + 1
                     sample.size <- dat.example$sample.size[element]
                     ## for the inspection plot we need a line (based on all data)
                     m.lm <- lm(sample.size ~ fisher(power), data = dat.example)
                     library(grid)
                     mypanel <- function(x, y, ...) {
                       panel.xyplot(x, y, col = "blue", ...)
                       panel.loess(x, y, span = 0.75, degree = 2, family = "gaussian", col = "blue", ...)
                       panel.abline(m.lm$coef, col = "blue", lty = 2)
                       panel.abline(v = fisher(power.example), col = "gray")
#                       grid.text(label = paste("power = ", round(power.example, 2), sep = ""), x = fisher(power.example), y = unit(0.15, "npc"), just = c(-0.01, 0.5))
                       ## showing the chosen power
                       panel.text(x = fisher(power.example), y = mean(dat.example$sample.size), labels = paste(" power: ", round(power.example, 2), sep = ""), adj = c(0.5, 0), srt = 90, col = "grey")
                       panel.points(x = fisher(dat.example$power[element]), y = dat.example$sample.size[element], col = "red")
                       ## showing the chosen sample size
                       panel.abline(h = sample.size, col = "gray")
                       panel.text(x = mean(fisher(dat.example$power)), y = sample.size, labels = paste(" sample size: ", sample.size, sep = ""), adj = c(0, -0.1), col = "grey")
                     }
                   })
### loess
###############
### this part is not used and only here fore historical reasons
            ## if (type == "loess"){
            ##   span = 0.05
            ##   m.loess <- loess(sample.size ~ fisher(power), span = span,
            ##                    data = dat.example)
            ##   p.loess <- predict(m.loess, newdata = data.frame(power = power.example))
            ##   sample.size <- ceiling(p.loess)
            ##   cat(paste("estimator: ", sample.size,  "\n"))
            ## }
            if (inspect) {
              print(xyplot(sample.size ~ fisher(power), data = dat.example,
                           xlab = "power (transformed)",
                           ylab = "sample size",
                           panel = mypanel
                           ))
            }
            return(new("SampleSize", estimate = as.integer(sample.size)))
          })

setMethod("inspect",
          signature = c(object = "power"),
          definition = function(object){
            invisible(sampleSize(x = object, inspect = TRUE))
          })

###---------------------------------------------------------------------------------------------------PLOT
plot.power <- function(x, at = c(0.9, 0.8, 0.85, 0.95), smooth = FALSE, example = TRUE, ...){
  object <-  x
  dat <- exDat(object)
  if (smooth) {
    span <- 0.5
    dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, "power"] <- fitted(loess(power ~ theta + sample.size, data=dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, ], span=span))
  }
  if (is.numeric(smooth)) {
    span <- smooth
    fisher <- function(x) 0.5 * log((1 + x) / (1 - x))
    unfisher <- function(y) (exp(2 * y) -1)/(1 + exp(2 * y))
    fitted.fisher <- fitted(loess(fisher(power) ~ theta + sample.size, data=dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, ], span = span))
    cat(summary(fitted.fisher))
    dat[!is.na(dat$power) & dat$power > 0 & dat$power < 1, "power"] <- unfisher(fitted.fisher)
  }else{
    span <- 0.75
  }
  if (example){
    sample.size <- sampleSize(object)@estimate
    power.example <- object@power.example
    theta.example <- object@theta.example
  }
### at argument
  if(!is.numeric(at)){stop("The argument \"at\" has to be numeric")}
  if(any(at<=0) | any(at>=1)){stop("The argument \"at\" has to be between of 0 and 1")}
  at.main<- c(at[1], at[1]*1.00000000001)
  at.second <- at[-1]
  if(length(at.second==1)){ at.second=c(at.second, at.second*1.0000000001)}
  rm(at)
###
  cp <- contourplot(power ~ theta + sample.size,
                    data = dat,
                    ...,
###
                    panel = function(x,y,z,at,labels,...){
                      panel.contourplot(x, y, z, at = at.main, cut = 2, lwd = 4, col = grey(0.6), label.style = "align", 
                                        labels = list(cex = 1.1, labels = paste("Power = ", round(at.main,3), sep = ""), col = grey(0.1)), ...)
                                        # label.syle causes often a conflict with the arrows ... experience shows that "align" here and "mixed" for the others is probably best, perhaps the user choice would make sense
                      if(length(at.second) >= 1){
                        panel.contourplot(x, y, z, at = at.second, cut=2, lwd = 1, col = grey(0.7),
                                          label.style = "align", labels = list(label.style = "align", labels = as.character(round(at.second,3)), col = grey(0.1)), ...)
                      }
                      if (example){
                        grid.lines(x=unit(c(theta.example,theta.example), "native"), y=unit(c(0,sample.size),c("npc","native"))+unit(c(0.02,-0.02),c("npc","npc")), arrow=arrow(length=unit(0.02, "npc")))
                        grid.lines(x=unit(c(theta.example,0),c("native","npc"))+ unit(c(-0.02,0.02),c("npc","npc")), y=unit(c(sample.size,sample.size), "native"), arrow=arrow(length=unit(0.02, "npc")))
                        grid.text(label=paste("N=",sample.size, sep=""),x=unit(0.05, "npc"), y=unit(sample.size,"native"), just=c(0,-0.2))
                        grid.text(label=bquote(paste(theta,"=", .(theta.example), sep="")),y=unit(0.1, "npc"), x=unit(theta.example,"native"), just=c(0.5,1),rot=-90)
                        grid.points(x=unit(theta.example, "native"), y=unit(sample.size,"native"), pch=20, size=unit(0.7,"char"))
                      }}
                    )
    class(cp) <- "trellis"
    print(cp)
}





## ------------------------------------------------------------------ METHODS
### for internal usage only:


### for users usage also:
setMethod("exDat",
          signature = c(x = "power"),
          definition = function(x, y, ...){
            xi.example <- x@xi.example
            if (is.na(xi.example)) {
              xi.example.integer <- 1
            } else {
              xi.example.integer <- which(xi.example == x@xi)
            }
            endpoint.example <- x@endpoint.example
            if (is.na(endpoint.example)) {
              endpoint.example.integer <-  1
            } else {
              endpoint.example.integer <- which(endpoint.example == x@endpoint.name)
            }
            dat <- data.frame(sample.size = rep(pp(x, "n"), times = dim(x)[2]),
                              theta = rep(pp(x, "theta"), each = dim(x)[1]),
                              power = c(pp(x, "core")[,,xi.example.integer,endpoint.example.integer]))
            return(dat)
          })
###



setMethod("merge",
          signature = c(x = "powCalc", y = "powEx"),
          definition = function(x, y, ...){
            ## do powCalc and powex fit together?
            ## if powCalc has no xi, powEx should neither
            if (all(is.na(x@xi)) & all(!is.na(y@xi.example))){
              warning("The powCalc-object does not make use of xi, but powEx provides an example for xi. The example for xi provided will be ignored.")
              y@xi.example <- as.numeric(NA)
            }
            new("power", x, y)
          })

setMethod("tex", signature(x="power", type = "character"),
          function(x, type = c("drop", "nRec", "nEval", "sampling", "theta", "xi", "n.iter", "power"), ...){
            type <- match.arg(type)
            switch(type,
                   sampling = {paste("$n_{i=1,...,", length(x@n), "} = ",min(x@n), ", ..., ",max(x@n),"$", sep = "")},
                   theta = {x@theta.example},
                   xi = {x@xi.example},
                   n.iter = {x@n.iter},
                   power = {x@power.example},
                   drop = {paste(round(100*x@drop), "~\\\\%", sep = "")},
                   nRec = {ceiling(sampleSize(x)@estimate/(1-x@drop))},
                   nEval = {ceiling(sampleSize(x)@estimate)},
                   {"undefinded sting provided for the argument type"}) # not used as long as match.arg is used because mach.arg returns with an error if it does not match!
          })


setMethod("pp", signature(object="powPar"), function(object, name){
  ##
  if (name %in% slotNames("power")) {
    slot(object, name)
  } else {
    eval(object@list[[name]])
  }
})

setMethod("n", signature(object="powPar"), function(object, ...){
  ##
  object@n.act
  ##
})

setMethod("theta", signature(object="powPar"), function(object, ...){
  ##
  object@theta.act
  ##
})
setMethod("xi", signature(object="powPar"), function(object, ...){
  ##
  object@xi.act
  ##
})

setMethod("dim", signature(x="powPar"), function(x){
  c(length(x@n), length(x@theta), length(x@xi))
})

setMethod("plot",
          signature(x = "power", y = "missing"),
          definition = function(x, ...) {
            plot.power(x, ...)
            })

setMethod("powCalc",
          signature(object="powPar"),
          definition = function(object, statistic, n.iter = NA, cluster = NULL, ...){
  # arg: n.iter
  n.iter <- as.integer(n.iter)
  ##
  if ( !is.na(n.iter) ) {
    power.fun <- function(statistic, object, ...){
      sig <- replicate(n.iter,
                       statistic(object)
                       )
      if(is.vector(sig)){ # this is the case if there is only one endpoint
        sig <- matrix(sig, ncol = length(sig), dimnames = list(names(sig[1])))
      }
      return(apply(sig, 1, function(x) sum(x, na.rm = TRUE) / length(x[!is.na(x)])))
    }
    empirical.endpoints <- power.fun(statistic, object, n.iter = 1)
  }
  if ( is.na(n.iter) ) {
    power.fun <- function(statistic, object){
      return(statistic(object))
    }
    empirical.endpoints <- power.fun(statistic, object)
  }
  
  ## to find out the number of endpoints and their name(s) we run the statistics function once
  n.endpoint <- length(empirical.endpoints)
  endpoint.name <- names(empirical.endpoints)
  if (is.null(endpoint.name)) {endpoint.name <- paste("ep", seq(1, n.endpoint), sep = "")}

  ## 
  power.array <- array(NA, dim=c(dim(object), n.endpoint))

  ## enable parallel computing (zumbrunnt)
  if (is.null(cluster) | is.na(n.iter) ) {

    ## original version (fabbrot)
    for (n.i in seq(along = object@n)){
      for (theta.i in seq(along = object@theta)){
        for (xi.i in seq(along = object@xi)){
          object@n.act <- object@n[n.i]
          object@theta.act <- object@theta[theta.i]
          object@xi.act <- object@xi[xi.i]
          if ( !is.na(n.iter) ) {
            power.array[n.i, theta.i, xi.i, ] <- power.fun(statistic, object, n.iter)
          }
          if ( is.na(n.iter) ) {
            power.array[n.i, theta.i, xi.i, ] <- power.fun(statistic, object)
          }
                                        #        cat(paste(object@n, object@theta, object@xi, "\n"))
        }                               # xi.loop
      }                                 # theta.loop
      cat(paste("n:", object@n.act, Sys.time(), "\n"))
    }                                   # n.loop
    
  } else {

    ## parallelised version (zumbrunnt)
    library(snow)
    clusterEvalQ(cluster, library(power))
    for (theta.i in seq(along = object@theta)) {
      for (xi.i in seq(along = object@xi)) {
        object@theta.act <- object@theta[theta.i]
        object@xi.act <- object@xi[xi.i]
        objects <- list()
        for (n.i in seq(along = object@n)) {
          object@n.act <- object@n[n.i]
          objects[[n.i]] <- object
        }
        power.array[seq(along = object@n), theta.i, xi.i, ] <-
          parSapply(cluster,
                    objects,
                    function(x) power.fun(statistic, x, n.iter))
        cat(paste("theta:", object@theta.act, Sys.time(), "\n"))
        ## temporary hack to have non-null endpoint.name (zumbrunnt)
        endpoint.name[xi.i] <- letters[xi.i]
      }
    }

  }
  
  ## seting the xy.act to NA, to avoid later use (instead of xy.example)
  object@n.act <- as.integer(NA)
  object@theta.act <- as.numeric(NA)
  object@xi.act <- as.numeric(NA)
  new("powCalc",
      object,
      core=power.array,
      statistic = statistic,
      endpoint.name = endpoint.name,
      n.iter = n.iter)
  ##
})



setMethod("show",
          signature(object = "power"),
          definition = function(object) {
            cat("*** Class power                      ***\n")
            cat(paste("The parameter \"", object@theta.name, "\" is used as theta.\n", sep = ""))
            cat("Additional parameters defined:\n")
            for (i in names(object@list)) {
              if (i != object@theta.name) {
              cat(paste("   ",i, ": ", eval(pp(object, i)), "\n", sep = ""))
            }
            }
            cat("\n")
            cat("Range and dimensions of the power array:\n")
            cat(paste("       n: from ", range(object@n)[1], " to ", range(object@n)[2], " (dim: ", dim(object)[1], ")\n", sep = ""))
            cat(paste("   theta: from ", range(object@theta)[1], " to ", range(object@theta)[2], " (dim: ", dim(object)[2], ")\n", sep = ""))
            if (all(!is.na(object@xi))){
            cat(paste("      xi: from ", range(object@xi)[1], " to ", range(object@xi)[2], " (dim: ", dim(object)[3], ")\n", sep = ""))
          }
            cat("\n")
            cat("Range of power observed:\n")
            cat(paste("   Min. ", round(range(object@core, na.rm = TRUE)[1],2), "\n   Max. ",round(range(object@core, na.rm = TRUE)[2],2), "\n"))
            cat("\n")
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
            cat("***                                  ***\n")
          })
