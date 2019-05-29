context("Standard use of sse")

library(testthat)
library(sse)
## as used in the vignette(examples)
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 2))
##
powFun.power <- function(psi){
  return(power.t.test(n = n(psi) / 2,
                      delta = theta(psi),
                      sig.level = 0.05)$power)
}
powFun.resample <- function(psi){
  x <- rnorm(n(psi) / 2)
  y <- rnorm(n(psi) / 2) + theta(psi)
  return(wilcox.test(x = x, y = y)$p.value < 0.05)
}

##
calc.power <- powCalc(psi, statistic = powFun.power)
calc.resample <- powCalc(psi, statistic = powFun.resample, n.iter = 33)

#
calc.resample2 <- update(calc.resample, theta = 1, n.iter = 99)

##
pow.power <- powEx(calc.power, theta = 1, power = 0.9)
pow.resample <- powEx(calc.resample, theta = 1, power = 0.9)
pow.resample <- powEx(pow.resample, theta = 0.8, power = 0.9)
pow.resample2 <- powEx(calc.resample2, theta = 1, power = 0.9)
##
inspect(pow.resample)
inspect(pow.resample2)
##

plot(pow.resample)

pow.refined <- refine(pow.resample)


inspect(pow.refined)
plot(pow.refined)
#pow.refined@iter <- as.integer(33)
update(pow.refined, n = seq(from = 30, to = 60, by = 5))

pow.refined.newEx <- powEx(pow.refined, theta = 1, power = 0.9)

pow.refined2 <- refine(pow.resample, n.iter = 99)
inspect(pow.refined2)
plot(pow.refined2)
### --------------------------------- TEST PLOTS  -->>  CHECK THEM VISUALLY
##



plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     )

## default is without smooting ?
plot(pow.resample,
     xlab = "Effect size",
     ylab = "Total sample size")

## sequence of "at" elements
## is the right line emph (power = 0.9)?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     at = seq(from = 0.8, to = 0.95, by = 0.05))

## is the right line emph (power = 0.8)?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     at = seq(from = 0.8, to = 0.95, by = 0.05),
     example = FALSE)

## xlim, correct range and overall look of labels etc?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     xlim = c(0.79, 1.21))

## ylim, correct range and overall look of labels etc?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     ylim = c(30, 50))

## xlim and ylim, correct range and overall look of labels etc?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     xlim = c(0.79, 1.21),
     ylim = c(30, 50))

## default method for resampling is "lm" -> check: is method lm is used?
inspect(pow.resample)
inspect(pow.resample2)


## method = "step" is possible for resampling -> check: is method step is used?
inspect(powEx(calc.resample, theta = 1, power = 0.9, method = "step"))
inspect(powEx(calc.resample2, theta = 1, power = 0.9, method = "step"))



### --------------------------------- TESTS
test_that("powPar", {
  # and not an object of class powCalc
  expect_error( # no theta
      powPar(n = seq(from = 20, to = 60, by = 2))  ## READ ERROR MESSAGE
  )
  expect_error( # an n that can not be coerced to an integer
      powPar(n  = calc.power, theta = seq(from = 0.5, to = 1.5, by = 0.05))
      ## READ ERROR MESSAGE
  )
  ## n as integer is dangerous but allowed
  ## expect_error(
  ##    powPar(n = c(TRUE, FALSE), theta = seq(from = 0.5, to = 1.5, by = 0.05))
  ## )
  expect_error(
      powPar(used.as.theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 2))
  )
    expect_error(
        powPar(used.as.theta = seq(from = 0.5, to = 1.5, by = 0.05),
               n = seq(from = 20, to = 60, by = 2),
               theta.name = "as.theta")
    )
})



test_that("powCalc", {
  ## expects an object of class powPar and not an object of class powCalc
  expect_error(
      powCalc(seq(from = 0, to = 100, by = 10), powFun1)
  )
  ## ## default is using cluster
  ## expect_message(
  ##     calc.resample <- powCalc(psi,
  ##               statistic = powFun.resample, n.iter = 3, cluster = TRUE)
  ##    , "using cluster"
  ## )
  ## ## if cluster = FALSE
  ## expect_message(
  ##     calc.resample <- powCalc(psi,
  ##              statistic = powFun.resample, n.iter = 3, cluster = FALSE)
  ##     , "not using cluster"
  ## )
})



test_that("powEx", {

  ## a wrong method, e.g. linear, is not allowed
  expect_error(
      powEx(calc.power, theta = 1, method = "linear")  ## READ ERROR MESSAGE
  )

  ## theta is not in the range used
  expect_error(
      powEx(calc.power, theta = 2)  ## READ ERROR MESSAGE
  )
  ## 
  expect_error(
      powEx(calc.power, theta = 2, forceDivisor = -2)  ## READ ERROR MESSAGE
  )
  expect_error(
      powEx(calc.power, theta = 2, endpoint = "ep2")  ## READ ERROR MESSAGE
  )
  ## calc object does not use xi but example for xi provided
  expect_warning(
      powEx(calc.power, theta = 1, xi = 7)  ## READ WARNING MESSAGE
  )
})


test_that("refine", {

  ## only if
  expect_error(
      refine(pow.power),
      strwrap(
          "Additional iterations for the chosen example are only
            meaningful if the object was created using resampling.",
          prefix = " ", initial = ""))
  expect_error(
      refine(calc.resample, n.iter = 0.5)  ## READ WARNING MESSAGE
  )
})


test_that("plot", {

  ##
  expect_warning(
      plot(pow.power, ylim = c(30, 40))  ## READ WARNING MESSAGE
  )
})


## a helper function for latex strings
prep.tex <- function(string){
  gsub("\\", "", string, fixed = TRUE)
  }

test_that("tex methods", {
  expect_match(prep.tex(tex(pow.power, "drop")), "0~%")
#  expect_match(prep.tex(tex(pow.power, "sampling")),
#    "$n_{i=1,...,21} = 20, ..., 60$") FIXME
  expect_equal(tex(pow.power, "nRec"), 46)
  expect_equal(tex(pow.power, "nEval"), 46)
  expect_equal(tex(pow.power, "theta"), 1)
  expect_equal(tex(pow.power, "xi"), as.numeric(NA))
  expect_equal(tex(pow.power, "power"), 0.9)
  expect_equal(tex(pow.power, "n.iter"), as.numeric(NA))
})
