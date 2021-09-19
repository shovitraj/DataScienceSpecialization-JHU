
pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character", 
                                                         "factor", "numeric", "numeric"))
head(pollution)




summary(pollution$pm25)
    

boxplot(pollution$pm25, col = "blue")

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


        
     
hist(pollution$pm25, col = "green")


![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



 
hist(pollution$pm25, col = "green")
rug(pollution$pm25)


![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



        

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)


![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



boxplot(pollution$pm25, col = "blue")
abline(h = 12)


![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)


![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 



barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")


![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



        
        ## Simple Summaries of Data
        

boxplot(pm25 ~ region, data = pollution, col = "red")


![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 



par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



        

        
        

with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)


![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)


![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 



       
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 



        
      









