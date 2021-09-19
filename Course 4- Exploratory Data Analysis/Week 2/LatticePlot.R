#Lattice Plot

library(lattice)
library(datasets)
##simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)

##Convert 'Month' to a factor variable
airquality <- transform(airquality, Month= factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(5,1))

p <- xyplot(Ozone~Wind, data = airquality) ##Nothing happens
print(p)
xyplot(Ozone~Wind, data = airquality) ##Auto printing

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each=50)
y <- x+f -f *x + rnorm(100, sd=0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y~x | f, layout= c(2,1)) ##Plot with 2 panels

## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)  ## First call default panel function
        panel.lmline(x, y, col = 2)  ## Overlay a simple linear regression line
})

