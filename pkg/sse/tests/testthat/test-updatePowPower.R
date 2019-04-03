context("Update pow-object, statistic returns a power")
## same test as in updatePower but not updating the calc-object but the pow-object.

library(sse)
library(testthat)
psi1 <- powPar(theta = seq(from = 0, to = 1, by = 0.1), 
               n = seq(from = 0, to = 100, by = 10))
##
powFun1 <- function(psi)
{
  n = n(psi)
  theta = theta(psi)
  return((n * theta)/100)
  }

## correct result for
result1.1.x <- array(seq(from = 0, to = 100, by = 10) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(11, 11, 1,1))
## n
result1.1.n <- array(seq(from = 0, to = 100, by = 5) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(21, 11, 1,1))
result1.1.n2 <- array(seq(from = 50, to = 100, by = 5) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(11, 11, 1,1))
result1.1.n3 <- array(seq(from = 110, to = 150, by = 10) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(5, 11, 1,1))
result1.1.n4 <- array(55 %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(1, 11, 1,1))
## theta 
result1.1.t <- array(seq(from = 0, to = 100, by = 10) %*% t(seq(from = 0, to = 1, by = 0.05)) /100, dim = c(11, 21, 1,1))
result1.1.t4 <- array(seq(from = 0, to = 100, by = 10) %*% t(0.51) /100, dim = c(11, 1, 1,1))
## thata and n
result1.1.nt <- array(seq(from = 0, to = 100, by = 5) %*% t(seq(from = 0, to = 1, by = 0.05)) /100, dim = c(21, 21, 1,1))


##
calc1.1.1 <- powCalc(psi1, statistic = powFun1)
pow1.1.1 <- powEx(calc1.1.1, theta = 0.5, power = 0.4)
##
n.new <- seq(from = 0, to = 100, by = 5)
n.new2 <- seq(from = 50, to = 100, by = 5)
n.new3 <- seq(from = 110, to = 150, by = 10)
n.new4 <- 55
theta.new <- seq(from = 0, to = 1, by = 0.05)
theta.new4 <- 0.51


test_that("update without any changes", {
#
  pow1.1.2 <- update(pow1.1.1)
  expect_equal(pow1.1.2@core, result1.1.x, check.attributes = FALSE)
})

test_that("increasing n within existing range of n", {
#
  pow1.1.3 <- update(pow1.1.1, n = n.new)
  expect_equal(pow1.1.3@core, result1.1.n, check.attributes = FALSE)
})

test_that("increasing theta within existing range of theta", {
#
  pow1.1.4 <- update(pow1.1.1, theta = theta.new)
  expect_equal(pow1.1.4@core, result1.1.t, check.attributes = FALSE)
})

test_that("increasing theta and n within existing range of theta and n", {
#
  pow1.1.5 <- update(pow1.1.1, n.iter = NA, n = n.new, theta = theta.new)
  expect_equal(pow1.1.5@core, result1.1.nt, check.attributes = FALSE)
})

test_that("update statistic only", {
#
  pow1.1.6 <- update(pow1.1.1, statistic = powFun1)
  expect_equal(pow1.1.6@core, result1.1.x, check.attributes = FALSE)
})

test_that("update statistic and increasing n within existing range of n", {
#
  pow1.1.7 <- update(pow1.1.1, statistic = powFun1, n.iter = NA, n = n.new)
  expect_equal(pow1.1.7@core, result1.1.n, check.attributes = FALSE)
})

test_that("updating n within smaller range of n", {
#
  pow1.1.8 <- update(pow1.1.1, n.iter = NA, n = n.new2)
  expect_equal(pow1.1.8@core, result1.1.n2, check.attributes = FALSE)
})

test_that("new n of length 1", {
#
  pow1.1.9 <- update(pow1.1.1, n = 55)
  expect_equal(pow1.1.9@core, result1.1.n4, check.attributes = FALSE)
})


test_that("completely new n (not within the old range)", {
#
  pow1.1.10 <- update(pow1.1.1, n = n.new3)
  expect_equal(pow1.1.10@core, result1.1.n3, check.attributes = FALSE)
})


test_that("new theta of length 1", {
#
  pow1.1.11 <- update(pow1.1.1, theta = theta.new4)
  expect_equal(pow1.1.11@core, result1.1.t4, check.attributes = FALSE)
})
