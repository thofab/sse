context("Advanced applications")

library(sse)
library(testthat)
### ------------------------------------------------------------------
### with pilot data and several endpoints
pilot.data <- rnorm(1000)
#
psi <- powPar(F.hat = pilot.data,
              delta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 50, by = 2),
              theta.name = "delta")

powFun <- function(psi){
   a <- sample(pp(psi, "F.hat"), size = n(psi)/2, replace = TRUE)
   b <- sample(pp(psi, "F.hat"), size = n(psi)/2, replace = TRUE) + theta(psi)
   w <- wilcox.test(a, b)$p.value < 0.05
   t <- t.test(a, b)$p.value < 0.05
   return(c(w = w, t = t))
   }

calc <- powCalc(psi, statistic = powFun, n.iter = 99)

pow.w <- powEx(calc, theta = 1, drop = 0.1, endpoint = "w")

plot(pow.w, smooth = 0.5,
     xlab = expression(paste("Delta, ", delta)),
     ylab = "Total sample size",
     main = "Wilcoxon Test")

pow.t <- powEx(calc, theta = 1, drop = 0.1, endpoint = "t")

plot(pow.t, smooth = 0.5,
     xlab = expression(paste("Delta, ", delta)),
     ylab = "Total sample size",
     main = "T- Test",
     ylim = c(30,40))

### --------------------------------- tests
test_that("endpoints", {
#
  ## selecting an endpoint that does not exist
  expect_error(
      powEx(calc, theta = 1, drop = 0.1, endpoint = "T")  ## READ ERROR MESSAGE
  )
})


test_that("powPar with data", {
#
  ## data extracted from psi is like the original
  expect_equal(pp(psi, "F.hat"), pilot.data)
})
