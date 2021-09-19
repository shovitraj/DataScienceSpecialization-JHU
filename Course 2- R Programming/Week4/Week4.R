#str function

str(str)
str(lm)
str(ls)
x <- rnorm(100, 2,4)
summary(x)
str(x)
str(rnorm)
f <- gl(40, 10)
f
str(f)
summary(f)
library(datasets)
head(airquality)
str(airquality)

m<- matrix(rnorm(100),10,10)
str(m)

s <- split(airquality, airquality$Month)
str(s)

#Simulation of Random Number

#rnorm by default mean 0, sd 1
#dnorm by default mean 0, sd 1
#pnorm by default mean 0, sd 1
#rpois by default mean 0, sd 1

x <- rnorm(10)
x
x <- rnorm(10,20,2)
x
summary(x)

set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5)

rpois(10,1) #rate of 1
rpois(10,2) #rate of 2 mean 2
rpois(10,20)
ppois(2,2) #cumulative distrubition #Pr(<x <= 2, if rate is 2)
ppois(4,2) #cumulative distrubition #Pr(<x <= 4)
ppois(6,2) #cumulative distrubition #Pr(<x <= 6)

#Simulaiton- Simulating a Linear Model

set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2* x + e
summary(y)
plot(x, y)

#binomial dist
set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2* x + e
summary(y)
plot(x, y)

#pisson dist

set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x,y)

#sample function

set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10)
sample(1:10)
sample(1:10, replace=TRUE)




#R profiler
system.time(readLines("http://www.jhsph.edu"))

hilbert <- functin(n) {
  i <- 1:n
  1 / outer(i-1, i , "+")
}
  
