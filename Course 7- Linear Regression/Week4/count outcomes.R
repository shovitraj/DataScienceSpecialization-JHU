#Poisson Regression

par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE) 

## Poisson distribution
x <- 0 : 10000; lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)
dev.off()
## Website data

load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")


## Linear regression line
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)

## Poisson regression in R
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
abline(lm1,col="red",lwd=3); lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

## Mean-variance relationship?
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

## Model agnostic standard errors 
library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
        cf <- coef(object); pnames <- names(cf)
        if (missing(parm))
                parm <- pnames
        else if (is.numeric(parm))
                parm <- pnames[parm]
        a <- (1 - level)/2; a <- c(a, 1 - a)
        pct <- stats:::format.perc(a, 3)
        fac <- qnorm(a)
        ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                   pct))
        ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
        ci[] <- cf[parm] + ses %o% fac
        ci
}


## Estimating confidence intervals
confint(glm1)
confint.agnostic(glm1)

## Fitting rates in R 
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)


## Fitting rates in R 
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),col="grey",xlab="Date",
     ylab="Fitted Rates",pch=19)
lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)


