#base plot

library(datasets)
data(cars)
with(cars, plot(speed, dist))

#lattice plot
library(lattice)
state.x77

state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

#ggplot2 plot
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

#Base Plotting System
library(datasets)
hist(airquality$Ozone)  ## Draw a new plot
help(package="datasets")

library(datasets)
with(airquality, plot(Wind, Ozone))

library(datasets)
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")
