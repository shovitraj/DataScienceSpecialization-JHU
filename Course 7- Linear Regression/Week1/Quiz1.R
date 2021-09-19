#1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mu<- sum(w*x)/sum(w)
mu

#2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit2 <- lm(y~x)
fit2$coefficients
fit1<-lm(y ~ x - 1)
fit1$coefficients
#3

data(mtcars)
summary(lm(mpg ~ wt, data = mtcars))
fit3 <- lm(mpg~wt,mtcars)
slope <- fit3$coefficients[2]
slope
s<- cor(mtcars$mpg, mtcars$wt) * sd(mtcars$mpg)/sd(mtcars$wt)
s

#4
#1

#5
#0.6

#6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
y <- (x-mean(x))/sd(x)
y
y[1]

#7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit7 <- lm(y~x)
fit7$coefficients[1]
fit7$coefficients[2]
coef(lm(y ~ x))[1]

#8
#Intercept 0

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
#10
