## simulate nosim averages of 10 standard normals
nosim <- 1000
n <- 10
## simulate nosim averages of 10 standard normals
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))

#Simulating means of random normals
nosim <- 1000
n <- 10
x <-rnorm(nosim * n)
mat <- matrix(x, nosim)
app <- apply(mat, 1, mean)
sd(app)



## Let's check to make sure that this is sigma / sqrt(n)
1 / sqrt(n)

#Simulating means of uniforms
nosim <- 1000
n <- 10

sd(apply(matrix(runif(nosim * n), nosim), 1, mean))

1 / sqrt(12 * n)


#Simulating means of Poisson variates
nosim <- 1000
n <- 10
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))


2 / sqrt(n)


#Simulating means of coin flips
nosim <- 1000
n <- 10
sd(apply(matrix(sample(0 : 1, nosim * n, replace = TRUE),
                nosim), 1, mean))

1 / (2 * sqrt(n))

#Data Example


library(UsingR); data(father.son);

x <- father.son$sheight
n<-length(x)
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)



p <- c(.1,.2,.3,.4)
x <- 2:5
sum(p)
rbind(x,p)
sum(x^2*p)-sum(x*p)^2

#Quiz 1:Question 5:
x <- 1:4
p <- x/sum(x)
temp <- rbind(x,p)
rownames(temp) <- c("X","prob")
temp
sum(p*x)
