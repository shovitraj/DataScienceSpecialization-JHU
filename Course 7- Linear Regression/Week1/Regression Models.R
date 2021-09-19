#Normalizing the data and setting plotting parameters
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
myPlot <- function(x, y) {
        plot(x, y, 
             xlab = "Father's height, normalized",
             ylab = "Son's height, normalized",
             xlim = c(-3, 3), ylim = c(-3, 3),
             bg = "lightblue", col = "black", cex = 1.1, pch = 21, 
             frame = FALSE)
}

#Plot the data, code
myPlot(x, y)
abline(0, 1) # if there were perfect correlation
abline(0, rho, lwd = 2) # father predicts son
abline(0, 1 / rho, lwd = 2) # son predicts father, son on vertical axis
abline(h = 0); abline(v = 0) # reference lines for no relathionship
