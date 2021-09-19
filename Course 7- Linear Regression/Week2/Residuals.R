library(UsingR)
data(diamond)
library(ggplot2)
diamond
table(diamond$carat)
g=ggplot(diamond, aes(x=carat, y=price))
g= g + xlab("Mass(carats)")
g= g + ylab("Price(SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y~x)
## The easiest way to get the residuals
e <- resid(fit)
e
## Obtain the residuals manually, get the predicted Ys first
yhat <- predict(fit)
yhat
## The residuals are y - yhat. Let's check by comparing this
## with R's build in resid function
max(abs(e-(y-yhat)))

## Let's do it again hard coding the calculation of Yhat
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

sum(e)
sum(e*x)

## Residuals are the signed length of the red lines

plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n) {
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)
}

## Residuals versus X

plot(diamond$carat, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) {
        lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)
}
## Non-linear data

x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2); 
plot(x, y); abline(lm(y ~ x))

plot(x, resid(lm(y ~ x))); 
abline(h=0)

## Heteroskedasticity

x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
plot(x, y); abline(lm(y ~ x))

## Getting rid of the blank space can be helpful

plot(x, resid(lm(y ~ x))); 
abline(h = 0)







## Diamond example

y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
sqrt(sum(resid(fit)^2) / (n - 2))



y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
## the estimate from lm
summary(fit)$sigma

## directly calculating from the residuals
sqrt(sum(resid(fit)^2) / (n - 2))





#Non-Linear Data

x = runif(100, -3,3)
y = x + sin(x)+rnorm(100, sd =.2)
g = ggplot(data.frame(x=x, y=y),aes(x=x, y=y))
g=g + geom_smooth(method="lm", color="black")
g= g+geom_point(size=7, color="black", alpha=0.4)
g= g+geom_point(size=5, color="red", alpha=0.4)
g


#Residual Plot
x = runif(100, -3,3)
y = x + sin(x)+rnorm(100, sd =.2)
g = ggplot(data.frame(x=x, y=resid(lm(y~x))),aes(x=x, y=y))
g=g + geom_hline(yintercept=0, size=2)
g= g+geom_point(size=7, color="black", alpha=0.4)
g= g+geom_point(size=5, color="red", alpha=0.4)
g = g+ xlab("x") + ylab("Residual")
g


#redidual plot diamond

diamond$e <- resid(lm(price~carat, data=diamond))
g <- ggplot(diamond, aes(x=carat, y=e))
g <- g + xlab("Mass (carats)")
g <- g + ylab("Residual price (SIN $")
g <- g+geom_hline(yintercept = 0, size=2)
g <- g+geom_point(size=7, color="black", alpha=0.5)
g <- g+geom_point(size=5, color="blue", alpha=0.2)
g


#diamond data residual plot

e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g


