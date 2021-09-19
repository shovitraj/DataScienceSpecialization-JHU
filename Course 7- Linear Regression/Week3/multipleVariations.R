## Plot of $R^2$ versus $n$
n <- 100
plot(c(1, n), 0 : 1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
r <- sapply(1 : n, function(p)
{
        y <- rnorm(n); x <- matrix(rnorm(n * p), n, p)
        summary(lm(y ~ x))$r.squared 
}
)
lines(1 : n, r, lwd = 2)
abline(h = 1)


## Variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)


## Variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

##doesn't depend on which y you use,
y <- x1 + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2]
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2],
  summary(lm(y~ x1 + x2 + x3))$cov.unscaled[2,2]) / a
temp <- apply(betas, 1, var); temp[2 : 3] / temp[1]


## Swiss data
data(swiss); 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) 


## Swiss data VIFs, 
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #I prefer sd 

## How to do nested model testing in R
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)



