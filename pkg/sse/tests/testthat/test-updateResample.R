context("Update, resampling")

library(sse)
library(testthat)
psi1 <- powPar(theta = seq(from = 0, to = 1, by = 0.1), 
               n = seq(from = 0, to = 100, by = 10))
##
powFun2 <- function(psi)
{
  n = n(psi)
  theta = theta(psi)
  return(as.logical((n * theta) %% 2))
}
powFun3 <- function(psi)
{
  n = n(psi)
  theta = theta(psi)
  return(as.logical(n %% 2))
}

## correct result for

result1.2.x <- array(round(seq(from = 0, to = 1, by = 0.1) %*% t(seq(from = 0, to = 100, by = 10)) %% 2, 10), dim = c(11,11,1,1))
result1.2.n2 <- array(as.logical(seq(from = 0, to = 1, by = 0.1) %*% t(seq(from = 50, to = 100, by = 5)) %% 2), dim = c(11,11,1,1))
#result1.2.n5 <- array(as.logical(seq(from = 0, to = 1, by = 0.1) %*% t(seq(from = 50, to = 150, by = 5)) %% 2), dim = c(21,11,1,1)) FIXME
result1.3.x <- array(0, dim = c(11,11,1,1))

##
calc1.2.1 <- powCalc(psi1, statistic = powFun2, n.iter = 99)

calc1.3.1 <- powCalc(psi1, statistic = powFun3, n.iter = 99)
calc1.2.1.nc <- powCalc(psi1, statistic = powFun2, n.iter = 99, cluster = FALSE) # as calc1.2.1 but without cluster

##
n.new <- as.integer(seq(from = 0, to = 100, by = 5))
n.new2 <- as.integer(seq(from = 50, to = 100, by = 5))
n.new5 <- as.integer(seq(from = 50, to = 150, by = 5))
theta.new <- seq(from = 0, to = 1, by = 0.05)
theta.new4 <- 0.51

##
pow1.2.1 <- powEx(calc1.2.1, theta = 1, power = 0.9)

pow1.2.1.rf <- refine(pow1.2.1)

test_that("update without any changes", {
#
  calc1.2.2 <- update(calc1.2.1)
  calc1.3.2 <- update(calc1.3.1)
  expect_equal(calc1.2.2@core, result1.2.x, check.attributes = FALSE)
  expect_equal(calc1.3.2@core, result1.3.x, check.attributes = FALSE)
})

test_that("increasing n.iter", {
#
  calc1.2.3 <- update(calc1.2.1, n.iter = 500)
  expect_equal(calc1.2.3@core, result1.2.x, check.attributes = FALSE)
  expect_equal(calc1.2.3@iter, 500)
})


test_that("increasing n within existing range of n", {
#
  calc1.2.3 <- update(calc1.2.1, n = n.new)
})


test_that("increasing theta within existing range of theta", {
#
  calc1.2.4 <- update(calc1.2.1, theta = theta.new)
  #expect_equal(calc1.1.4@core, result1.1.t, check.attributes = FALSE)
})



test_that("increasing theta and n within existing range of theta and n", {
#
  calc1.2.5 <- update(calc1.2.1, n.iter = NA, n = n.new, theta = theta.new)
#  expect_equal(calc1.1.5@core, result1.1.nt, check.attributes = FALSE)
})



test_that("update statistic only", {
#
  calc1.2.6 <- update(calc1.2.1, statistic = powFun2)
  expect_equal(calc1.2.6@core, result1.2.x, check.attributes = FALSE)
})



test_that("update statistic and increasing n within existing range of n", {
#
  calc1.2.7 <- update(calc1.2.1, statistic = powFun2, n = n.new)
  calc1.2.7 <- update(calc1.2.7, n = n.new)
#  expect_equal(calc1.2.7@core, result1.2.n, check.attributes = FALSE)
})



test_that("updating n within smaller range of n", {
#
  calc1.2.8 <- update(calc1.2.1, n = n.new2)
 # expect_equal(array(as.logical(calc1.2.8@core), dim = c(11,11,1,1)), result1.2.n2, check.attributes = FALSE)
})

test_that("updating n with new range of n only partially matching", {
#
  calc1.2.8 <- update(calc1.2.1, n = n.new5)
 # expect_equal(array(as.logical(calc1.2.8@core), dim = c(11,11,1,1)), result1.2.n2, check.attributes = FALSE)
})

test_that("not using cluster", {
#
  expect_equal(calc1.2.1.nc@core, result1.2.x, check.attributes = FALSE)
  expect_equal(calc1.2.1.nc@core, result1.2.x, check.attributes = FALSE)
})


test_that("new theta of length 1 and increasing n.iter", {
#
  calc1.2.9 <- update(calc1.2.1.nc, theta = theta.new4, n.iter = 99)
  calc1.2.10 <- update(calc1.2.9, theta = theta.new4, n.iter = 100)
})

test_that("updating n with new range of n only partially matching", {
#
  calc1.2.11 <- update(calc1.2.1, n = n.new5)
  #expect_equal(array(as.logical(calc1.2.11@core), dim = c(21,11,1,1)), result1.2.n5, check.attributes = FALSE)
})
