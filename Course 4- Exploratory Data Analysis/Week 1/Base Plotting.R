
#Chapter 7
#Base Plotting System

data(airquality)
with(airquality, plot(Temp, Ozone), lines(loess.smooth(Temp, Ozone)))


data(cars)
## Create the plot / draw canvas
with(cars, plot(speed, dist))
 
## Add annotation
title("Speed vs. Stopping distance")

#Lattice Plots
library(lattice)
state <- data.frame(state.x77, region = state.region)

xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

#ggplot2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)



#Chapter 8: Graphic Devices
## Make plot appear on screen device
with(faithful, plot(eruptions, waiting)) 
 
## Annotate with a title
title(main = "Old Faithful Geyser data") 


## Open PDF device; create 'myplot.pdf' in my working directory
pdf(file = "myplot.pdf")  
 
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))  
 
## Annotate plot; still nothing on screen
title(main = "Old Faithful Geyser data")  

## Close the PDF file device
dev.off()  
 
## Now you can view the file 'myplot.pdf' on your computer

qplot(displ, hwy, data = mpg)


library(datasets)
 
## Create plot on screen device
with(faithful, plot(eruptions, waiting))  
 
## Add a main title
title(main = "Old Faithful Geyser data")  

## Copy my plot to a PNG file
dev.copy(png, file = "geyserplot.png")  
 
## Don't forget to close the PNG device!
dev.off()  

#Chapter 9: Base Plotting Systems
library(datasets)
 
## Draw a new plot on the screen device
hist(airquality$Ozone) 

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

with(airquality, plot(Wind, Ozone))
?par
par("lty")

par("col")
par("pch")

par("bg")

par("mar")

par("mfrow")

library(datasets)
airquality

## Make the initial plot
with(airquality, plot(Wind, Ozone))

## Add a title
title(main = "Ozone and Wind in New York City")  

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
## Fit a simple linear regression mode
model <- lm(Ozone ~ Wind, airquality)

## Draw regression line on plot
 abline(model, lwd = 2)
        
        
 par(mfrow = c(1, 2))
 with(airquality, plot(Wind, Ozone, main = "Ozone and Wind"), plot(Solar.R, Ozone, main = "Ozone and Solar Radiation"))
        
        
  par(mfrow = c(1, 2))
  with(airquality, {
          plot(Wind, Ozone, main = "Ozone and Wind")
          plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
          })
  
  
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
          plot(Wind, Ozone, main = "Ozone and Wind")
          plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
          plot(Temp, Ozone, main = "Ozone and Temperature")
          mtext("Ozone and Weather in New York City", outer = TRUE)
})





