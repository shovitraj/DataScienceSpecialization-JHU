x <- runif(1, 0, 10)
if(x > 3) {
  y <- 10
} else {
  y <- 0
}


y <- if(x > 3) {
  10
} else {
  0
}

for(i in 1:10) {
  print(i)
}

x <- c("a", "b", "c", "d")
for(i in 1:4) {
  ## Print out each element of 'x'
  print(x[i])
}

for(i in seq_along(x)) {
  print(x[i])
}

for(letter in x) {
  print(letter)
}

for(i in 1:4) print(x[i])


x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }
}


count <- 0
while(count <= 10) {
  print(count)
  count <- count + 1
}

z <- 5
set.seed(1)

while(z >= 3 && z <= 10) {
  coin <- rbinom(1, 1, 0.5)
  
  if(coin == 1) { ## random walk
    z <- z + 1
  } else {
    z <- z - 1
  }
}

z





x0 <- 1
tol <- 1e-8
repeat {
  x1 <- 10
  if(abs(x1 - x0) < tol) { ## Close enough?
    break
  } else {
    x0 <- x1
  }
}

for(i in 1:100) {
  if(i <= 20) {
    ## Skip the first 20 iterations
    next
  }
  ## Do something here
}

for(i in 1:100) {
  print(i)
  if(i > 20) {
    ## Stop loop after 20 iterations
    break
  }
}

add2 <- function(x, y) {
  x+y
}

add2(2,3)

above10 <-funciton(x) {
  use <- x >10
  x[use]
}

above(x,12)

above(10)

above <- function(x,n){
  use <- x>n
  x[use]
}


columnmean <- function(y, removeNA=TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] < mean(y[,i])
  }
  means
}
columnmean(airquality)

f <- function() {
  cat("Hello, world!\n")
}
f()


f <- function(num) {
  for(i in seq_len(num)) {
    cat("Hello, world!\n")
  }
}
f(3)  

f <- function(num) {
  hello <- "Hello, world!\n"
  for(i in seq_len(num)) {
    cat(hello)
  }
  chars <- nchar(hello) * num
  chars
}
meaningoflife <- f(3)
print(meaningoflife)


f <- function(num = 1) {
  hello <- "Hello, world!\n"
  for(i in seq_len(num)) {
    cat(hello)
  }
  chars <- nchar(hello) * num
  chars
}
f()

x <- as.Date("1970-01-01")
x

unclass(x)
unclass("1970-01-02")

unclass(as.Date("1970-01-02"))
unclass(as.Date("1970-01-03"))

unclass(as.Date("1969-12-30"))

x<- Sys.time()
y<- Sys.Date()
x

y

p<- as.POSIXlt(x)
p

names(unclass(p))

> p$sec

p$yday
p$yday

p$min

p$hour

p$mday
p$mon

p$year

p$wday

p$yday

p$isdst

p$zone

p$gmtoff

p$gmtoff

q<- as.POSIXlt(y)
q

p$isdst

r<-as.POSIXct.Date(x)
r

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x

class(x)

x <- as.POSIXlt(x)
x-y

x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
x-y

x <- as.POSIXlt(x)
x-y

x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28")
x-y

x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT")
y-x
#Time difference of -2 hours
#Time difference of 356.3095 days
#The nice thing about the date/time classes is that they keep track of all the annoying things about
#dates and times, like leap years, leap seconds, daylight savings, and time zones.
#Here’s an example where a leap year gets involved.
x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28")
x-y

#Functions (Part1)

mydata <- rnorm(100)
sd(mydata)
sd(x=mydata)
sd(x=mydata, na.rm=FALSE)
sd(na.rm=FALSE, x=mydata)
sd(na.rm=FALSE,mydata)


args(lm)

f <- function(a, b=1, c, d=NULL){
  
}

f <- function(a,b){
  a^2
}

f(2)
f(2,3)

f <- function(a,b){
  print(a)
  print(b)
}

f(45)

myplot <- function(x, y, type = "l", ...) {
  plot(x, y, type = type, ...) ## Pass '...' to 'plot' function
}

paste("a", "b", sep = ":")
paste("a", "b", se = ":")


#Scoping Symbol

lm <- function(x){x*x}
lm
lm(2)


search()

f <- function(x, y) {
  x^2 + y / z
}

make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}

cube <- make.power(3)
square <- make.power(2)
cube(3)

square(3)

ls(environment(cube))

ls(environment(square))

cube
function(x) {
  x^n
}

y <- 10
f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x){
  x^y
}

g <- function(x) {
  a <- 3
  x+a+y
  ## 'y' is a free variable
}
g(2)

y <- 3
g(2)


cube <- function(x,n){
  x^3
}

cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}


h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}


