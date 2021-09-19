#1

library(MASS)
data(shuttle)
head(shuttle)
unique(shuttle$use)
unique(shuttle$wind)
table(shuttle$use)
table(shuttle$wind)

# Auto lander predicted by wind sign
fit1<-glm(shuttle$use~shuttle$wind, family="binomial")
summary(fit1)
# Coefficient 2 is the windtail refereced to windhead
# coef[2] is the ration in the log scale so we must exponentiate it to go back to the original scale
exp(summary(fit1$coef[2]))
exp(coef(fit1))



#2

fit2<-glm(shuttle$use~shuttle$wind+shuttle$magn, family="binomial")
summary(fit2)
exp(summary(fit2$coef[2]))
exp(coef(fit2))

#3
# Let's use relevel to noauto as the reference value
fit3<-glm(relevel((shuttle$use),ref="noauto")~shuttle$wind, family="binomial")
summary(fit3)
exp(summary(fit3$coef[2]))

#4

data("InsectSprays")
head(InsectSprays)
#using relevel we can obtais the coefficients relatively to B
fitArelB<-glm(InsectSprays$count~relevel(InsectSprays$spray,ref="B"),family="poisson")
summary(fitArelB)
#the outcome is the result of e exponentiated with coef[2], A relative to B
exp(summary(fitArelB)$coef[2])

#5
#The coefficient estimate is unchanged


#6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots<-0

splineTerms<-sapply(knots, function(knot)(x>knot)*(x-knot))
(xMat<-cbind(1,x,splineTerms))

fit<-lm(y~ xMat-1)
yhat<-predict(fit)
plot(x,y,frame=FALSE, pch=21, bg="lightblue", cex=2)
lines(x,yhat,col="red", lwd=2)
#the slope is calculated as the difference of yhat at 11. position and 6.
#position divided by the difference of the x valuas art same positions 

yhat

slope = (yhat[11]-yhat[6])/(x[11]-x[6])
slope







