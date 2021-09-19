#1
#Loading and examining the data
data(mtcars)
head(mtcars)
#fitting the model
fit <- lm(mpg~factor(cyl) + wt, data=mtcars)
fit
coeff<- summary(fit)$coefficient
coeff
#selecting coefficient
coeff[3,1]

#2
fit2 <- lm(mpg~factor(cyl), data=mtcars)
fit22 <- lm(mpg~factor(cyl) + wt, data=mtcars)
coeff2<- summary(fit2)$coef
coeff22<- summary(fit22)$coef
coeff2
coeff22
#Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.

#3
fit3 <-lm(mpg~factor(cyl) + wt, data=mtcars)
fit33 <- lm(mpg~factor(cyl)*wt, data=mtcars)
anova(fit3, fit33)
#The P-value is larger than 0.05. So, according to our criterion, 
#we would fail to reject, which suggests that the interaction terms may not be necessary.

#4
fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
fit4
summary(fit)
summary(fit4)
#The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).

#5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit5 <- lm(y~x)
hatvalues(fit5) #0.9946
plot(fit5, which=4)

#6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6<- lm(y~x)
dfbetas(fit6) #-134

#7
#It is possible for the coefficient to reverse sign after adjustment.
#For example, it can be strongly significant and positive before adjustment
#and strongly significant and negative after adjustment.
data(swiss)
summary(lm(Fertility~Agriculture,data=swiss))
summary(lm(Fertility~., swiss))


