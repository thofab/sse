# Sample Size Estimation
```{r teaser}
install.packages("sse")
library(sse)


psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 10))
powFun <- function(psi) {
  return(power.t.test(n = n(psi)/2, 
                      delta = theta(psi),
                      sig.level = 0.05)$power)
}
calc <- powCalc(psi, statistic = powFun)
pow <- powEx(x = calc, theta = 1, power = 0.9)
plot(pow)
```
