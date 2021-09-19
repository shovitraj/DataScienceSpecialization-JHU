 x <- c(0.5, 0.6) ## numeric
 x <- c(TRUE, FALSE) ## logical
 x <- c(T, F) ## logical
 x <- c("a", "b", "c") ## character
 x <- 9:29 ## integer
 x <- c(1+0i, 2+4i)
 #Mixing
 x <- vector("numeric", length = 10)
 x
 
 
 y <- c(1.7, "a") ## character
 y

 y <- c(TRUE, 2)## numeric
 y
 
 y <- c("a", TRUE) ## character
 y
 #Explicit Coercion
  x <- 0:6
  class(x)
  
  as.numeric(x)

  as.logical(x)

  as.character(x)
  as.complex(x)
  x <- c("a", "b", "c")
  as.numeric(x)
  
  as.logical(x)
  
  as.character(x)
  #List
  x <- list(1, "a", TRUE, 1 + 4i)
  x
  
  x <- vector("list", length = 5)
  x
  #Matrix
  m  <- matrix(nrow = 2, ncol = 3)
  m
  dim(m)
  attributes(m)
  
  m <- matrix(1:6, nrow=2, ncol=3)
  m
  
  m <- 1:10
  dim(m) <-  c(2,5)
  m
  
  x <-1:3
  y<- 10:12
  cbind(x,y)
  rbind(x,y)
  
  m<- 1:10
  m
  
  #Factors
  x <- factor(c("yes", "yes", "no", "yes", "no"))
  x
  
  table(x)
  
  unclass(x)
  
  attr(,"levels")

  
   x <- factor(c("yes", "yes", "no", "yes", "no"))
   x ## Levels are put in alphabetical order
   
 x <- factor(c("yes", "yes", "no", "yes", "no"),
                        levels = c("yes", "no"))
 x
 
 
 #Missing Values
 x <- c(1,2,NA, 10,3)
 is.na(x)
 is.nan(x)
 
 y <- c(1,2,NA, NaN,3)
 is.na(y)
 is.nan(y)
 
 
 #Data Frames
 
 x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
 x 
 nrow(x)
 ncol(x)
 
 #Data Frame Name Attribute
 
 x<- 1:3
 names(x)
 
 names(x) <- c("New York", "Seattle", "Los Angeles")
 x
 names(x)
 
 x <- list("Los Angeles" = 1, Boston = 2, London = 3)
 x
 
 x <- list(a=1, b=2, c=3)
 x
 
 
 
 x <- 1:4; y <- 6:9
 x+y
 z <- x+y
 z
 x>2
 y == 8
 x*y
 x/y
 
 class(x/y)
 
 x <- matrix(1:4, 2,2)
 x
 
 y <- matrix(rep(10,4),2,2)
 y
 
 x*y
 x/y
 
 x %*% y
 
 x <- c("a", "b", "c", "c", "d","a")
 x[1]
 x[2]
 
 x[1:4]
 
 x[c(1, 3, 4)]
 
 u <- x > "a"
 u
 
 x[u]
 
 x[x>"a"]
 
 
 x <- matrix(1:6, 2, 3)
 x
 
 x[1, 2]
 
 x[2, 1]
 
 
 x[1,]
 x[,2]
 x <- matrix(1:6, 2, 3)
 
 x[1,2]
 x[1, 2, drop = FALSE]
 
 x <- matrix(1:6, 2, 3)
 
 x[1, ]
 
 x[1, , drop = FALSE]
 
 x <- list(foo = 1:4, bar = 0.6)
 x
 x[1]
 x[[1]]
 x$foo
 
 x$bar
 
 x[["bar"]]
 x["bar"]
 
 x[[1]]
 
 x[c(1,2)]
 
 x <- list(foo = 1:4, bar = 0.6, baz = "hello")
 
 name <-"foo"
 
 x[[name]]
 
 x$name
 
 x$foo
 
 
x <- list(a = list(10, 12, 14), b = c(3.14, 2.81))
x
## Get the 3rd element of the 1st element
x[[c(1, 3)]]

## Same as above
x[[1]][[3]]
## 1st element of the 2nd element
x[[c(2, 1)]]
 

x <- list(aardvark = 1:5)
x

x$a

x[["a"]]

x[["a", exact = FALSE]]

x <- c(1, 2, NA, 4, NA, 5)
bad<- is.na(x)
print(bad)

x[!bad]

x <- a(1,2,NA,4,NA,5)
y <- c("a", "b", NA, "d", NA, "f")
x
y
good <- complete.cases(x,y)
good
x[good]
y[good]
airquality
head(airquality)
airquality[1:6,]
good <- complete.cases(airquality)
head(airquality[good,][1:6,])


str(file)

con <- file("foo.txt")
open(con, "r")


data <- read.table("foo.txt")

initial <- read.table("datatable.txt", nrows = 100)

y <- data.frame(a = 1, b = "a")
dput(y)
structure(list(a = 1, b = structure(1L, .Label = "a", class = "factor")), .Names =c("a","b"), row.names = c(NA, -1L), class = "data.frame")t(y)
      

dput(y, file = "y.R")
new.y <- dget("y.R")
new.y

x <- "foo"
y <- data.frame(a = 1L, b = "a")
#We can dump() R objects to a file by passing a character vector of their names.
dump(c("x", "y"), file = "data.R")
rm(x, y)
#The inverse of dump() is source().
source("data.R")
str(y)
?class

#Quiz1

quiz1<- read.csv("hw1_data.csv")

quiz1[1:2,]
row<-nrow(quiz1)
quiz1[152:153,]
quiz1[47, ]
quiz1$Ozone[47]
sum(is.na(quiz1$Ozone))
mean(quiz1$Ozone, na.rm=TRUE)
quiz1


mean(quiz1)
mean(quiz1[which(quiz1$Ozone > 31 & quiz1$Temp>90),]$Solar.R)
mean(quiz1[which(quiz1$Month==6),]$Temp)
max(quiz1[which(quiz1$Month==5),]$Ozone, na.rm=TRUE)
