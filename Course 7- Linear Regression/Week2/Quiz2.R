#1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)
coefTable <- coef(summary(fit))
coefTable
(pval <- coefTable[2, 4])

#We can also sompute the P-value using the definitions and formulas as follows. The P-value will be the same as above.

n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e ^ 2) / (n - 2)) 
ssx <- sum((x - mean(x)) ^ 2)
seBeta1 <- sigma / sqrt(ssx)
tBeta1 <- beta1 / seBeta1
(pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE))

#2 Consider the previous problem, give the estimate of the residual standard deviation.

summary(fit)$sigma
(sigma <- sqrt(sum(e ^ 2) / (n - 2)))

#3
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
predict(fit_car, newdata = data.frame(x = mean(x)), interval = ("confidence"))
yhat <- fit_car$coef[1] + fit_car$coef[2] * mean(x)
yhat + c(-1, 1) * qt(.975, df = fit_car$df) * summary(fit_car)$sigma / sqrt(length(y))

#4
#4 Since variable wt has unit (lb/1000), the coefficient is interpreted as the estimated expected change in mpg per 1,000 lb increase in weight.
#The estimated expected change in mpg per 1,000 lb increase in weight
#5
predict(fit_car, newdata = data.frame(x = 3), interval = ("prediction"))
yhat <- fit_car$coef[1] + fit_car$coef[2] * 3
yhat + c(-1, 1) * qt(.975, df = fit_car$df) * summary(fit_car)$sigma * sqrt(1 + (1/length(y)) + ((3 - mean(x)) ^ 2 / sum((x - mean(x)) ^ 2)))
# the upper point is 27.57.
#6
fit_car2 <- lm(y ~ I(x/2))
sumCoef2 <- coef(summary(fit_car2))
(sumCoef2[2,1] + c(-1, 1) * qt(.975, df = fit_car2$df) * sumCoef2[2, 2])
#The lower endpoint of the interval is -12.973.
#7
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
fit$coef[2]
x_meter <- x/100
fit_meter <- lm(y ~ x_meter)
fit_meter$coef[2]


#8
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
fit$coef
x_c <- x + 10
fit_c <- lm(y ~ x_c)
fit_c$coef
fit$coef[1] - 10 * fit$coef[2]


#9
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
sum(resid(fit_car)^2) / sum((y - mean(y)) ^ 2)
fit4 <- lm(mpg~wt,data=mtcars)
fit5 <- lm(mpg~1,data=mtcars)

num <- sum((predict(fit4)-mtcars$mpg)^2)
den <- sum((predict(fit5)-mtcars$mpg)^2)
num/den
#10
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
sum(resid(fit_car))
fit_car_noic <- lm(y ~ x - 1)
sum(resid(fit_car_noic))
fit_car_ic <- lm(y ~ rep(1, length(y)))
sum(resid(fit_car_ic))
#The residuals always sum to 0 if an intercept is defined.