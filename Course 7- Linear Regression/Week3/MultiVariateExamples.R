
## Swiss fertility data
library(datasets); data(swiss); require(stats); require(graphics); library(ggplot2);require(GGally)
#pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))
g <- ggpairs(swiss, lower=list(continuous="smooth"), params=c(method="loess"))
g
## Example interpretation

summary(lm(Fertility ~ . , data = swiss))$coefficients


#How can adjustment reverse the sign of an effect? Let's try a simulation.

n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

par(mfrow = c(1, 2))
plot(x1, y, pch=21,col="black",bg=topo.colors(n)[x2], frame = FALSE, cex = 1.5)
title('Unadjusted, color is X2')
abline(lm(y ~ x1), lwd = 2)
plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), pch = 21, col = "black", bg = "lightblue", frame = FALSE, cex = 1.5)
title('Adjusted')
abline(0, coef(lm(y ~ x1 + x2))[2], lwd = 2)

## What if we include an unnecessary variable?
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)

## Insect Sprays

require(datasets);data(InsectSprays)
require(stats); require(graphics)
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")

## Linear model fit, group A is the reference
summary(lm(count ~ spray, data = InsectSprays))$coef

## What if we include all 6?

lm(count ~ 
           I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
           I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
           I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays)

## Reordering the levels
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

## Doing it manually


      
fit <- lm(count ~ spray, data = InsectSprays) #A is ref
bbmbc <- coef(fit)[2] - coef(fit)[3] #B - C
temp <- summary(fit) 
se <- temp$sigma * sqrt(temp$cov.unscaled[2, 2] + temp$cov.unscaled[3,3] - 2 *temp$cov.unscaled[2,3])
t <- (bbmbc) / se
p <- pt(-abs(t), df = fit$df)
out <- c(bbmbc, se, t, p)
names(out) <- c("B - C", "SE", "T", "P")
round(out, 3)


## WHO childhood hunger data
#download.file("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv?profile=text&filter=COUNTRY:*;SEX:*","hunger.csv",method="curl")
hunger <- read.csv("hunger.csv")
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)


## Plot percent hungry versus time
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")

## Add the linear model
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
lines(hunger$Year,lm1$fitted,lwd=3,col="darkgrey")

## Color by male/female

plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))


## Color by male/female


lmM <- lm(hunger$Numeric[hunger$Sex=="Male"] ~ hunger$Year[hunger$Sex=="Male"])
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"] ~ hunger$Year[hunger$Sex=="Female"])
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
lines(hunger$Year[hunger$Sex=="Male"],lmM$fitted,col="black",lwd=3)
lines(hunger$Year[hunger$Sex=="Female"],lmF$fitted,col="red",lwd=3)


dev.off()

