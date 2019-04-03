context("Update, statistic returns a power")

library(sse)
library(testthat)
psi1 <- powPar(theta = seq(from = 0, to = 1, by = 0.1), 
               n = seq(from = 0, to = 100, by = 10))
psi2 <- powPar(theta = seq(from = 0, to = 1, by = 0.1), 
               n = seq(from = 50, to = 100, by = 10))
##
powFun1 <- function(psi)
{
  n = n(psi)
  theta = theta(psi)
  return((n * theta)/100)
  }


##
n.new <- as.integer(seq(from = 0, to = 100, by = 5))
n.new2 <- as.integer(seq(from = 50, to = 100, by = 5))
n.new3 <- as.integer(c(seq(from = 50, to = 75, by = 2), seq(from = 75, to = 100, by = 5)))
n.new5 <- seq(from = 50, to = 150, by = 5)
theta.new <- seq(from = 0, to = 1, by = 0.05)


## correct result for
result1.1.x <- array(seq(from = 0, to = 100, by = 10) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(11, 11, 1,1))
result1.1.n <- array(seq(from = 0, to = 100, by = 5) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(21, 11, 1,1))
result1.1.t <- array(seq(from = 0, to = 100, by = 10) %*% t(seq(from = 0, to = 1, by = 0.05)) /100, dim = c(11, 21, 1,1))
result1.1.nt <- array(seq(from = 0, to = 100, by = 5) %*% t(seq(from = 0, to = 1, by = 0.05)) /100, dim = c(21, 21, 1,1))
result1.1.n2 <- array(seq(from = 50, to = 100, by = 5) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(11, 11, 1,1))
result1.1.n3 <- array(n.new3 %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(length(n.new3), 11, 1,1))
result1.1.n5 <- array(n.new5 %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(length(n.new5), 11, 1,1))

result2.1.x <- array(seq(from = 0, to = 100, by = 10) %*% t(seq(from = 0, to = 1, by = 0.1)) /100, dim = c(11, 11, 1,1))


##
calc1.1.1 <- powCalc(psi1, statistic = powFun1)

pow1.1.1 <- powEx(calc1.1.1, theta = 0.5, power = 0.4)

plot(pow1.1.1, at = c(0.4, seq(from = 0.5, to = 0.9, by = 0.1)))

test_that("update without any changes", {
#
  calc1.1.2 <- update(calc1.1.1)
  expect_equal(calc1.1.2@core, result1.1.x, check.attributes = FALSE)
})



test_that("increasing n within existing range of n", {
#
  calc1.1.3 <- update(calc1.1.1, n = n.new)
  expect_equal(calc1.1.3@core, result1.1.n, check.attributes = FALSE)
})



test_that("increasing theta within existing range of theta", {
#
  calc1.1.4 <- update(calc1.1.1, theta = theta.new)
  expect_equal(calc1.1.4@core, result1.1.t, check.attributes = FALSE)
})



test_that("increasing theta and n within existing range of theta and n", {
#
  calc1.1.5 <- update(calc1.1.1, n.iter = NA, n = n.new, theta = theta.new)
  expect_equal(calc1.1.5@core, result1.1.nt, check.attributes = FALSE)
})



test_that("update statistic only", {
#
  calc1.1.6 <- update(calc1.1.1, statistic = powFun1)
  expect_equal(calc1.1.6@core, result1.1.x, check.attributes = FALSE)
})



test_that("update statistic and increasing n within existing range of n", {
#
  calc1.1.7 <- update(calc1.1.1, statistic = powFun1, n.iter = NA, n = n.new)
  expect_equal(calc1.1.7@core, result1.1.n, check.attributes = FALSE)
})


test_that("updating n within smaller range of n", {
#
  calc1.1.8 <- update(calc1.1.1, n.iter = NA, n = n.new2)
  calc1.1.9 <- update(calc1.1.1, n.iter = NA, n = n.new3)
  expect_equal(calc1.1.8@core, result1.1.n2, check.attributes = FALSE)
  expect_equal(calc1.1.9@core, result1.1.n3, check.attributes = FALSE)
})


test_that("updating n partially new range of n", {
#
  calc1.1.10 <- update(calc1.1.1, n.iter = NA, n = n.new5)
  expect_equal(calc1.1.10@core, result1.1.n5, check.attributes = FALSE)
  calc1.1.10 <- update(calc1.1.1, n.iter = NA, n = n.new5, keep = TRUE)
})


### ------------------------------------------------------------------ CHECK MANUALLY
calc1.1.9 <- update(calc1.1.1, n.iter = NA, n = n.new3)
plot(powEx(calc1.1.9, theta = 1), at = c(0.4, seq(from = 0.5, to = 0.9, by = 0.1)))


