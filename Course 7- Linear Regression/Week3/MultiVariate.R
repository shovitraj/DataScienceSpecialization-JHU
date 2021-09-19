## Demonstration that it works using an example
n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd=0.1)
ey = resid(lm(y~x2+x3))
ex = resid(lm(x~x2+x3) )
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey~ex-1))
coef(lm(y~x+x2+x3))

### Linear model with two variables and an intercept

n <- 100; x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- x + x2 + x3 + rnorm(n, sd = .1)
e <- function(a, b) a -  sum( a * b ) / sum( b ^ 2) * b
ey <- e(e(y, x2), e(x3, x2))
ex <- e(e(x, x2), e(x3, x2))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term

## Showing that order doesn't matter
ey <- e(e(y, x3), e(x2, x3))
ex <- e(e(x, x3), e(x2, x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term

## Residuals again
ey <- resid(lm(y ~ x2 + x3 - 1))
ex <- resid(lm(x ~ x2 + x3 - 1))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(y ~ x + x2 + x3 - 1)) #the -1 removes the intercept term

