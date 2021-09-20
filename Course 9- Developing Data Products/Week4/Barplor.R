income<- c(57.4, 55.5, 54.9, 53.6, 52.8, 52.7, 52.9, 53.7, 56.6)
year<- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
value <- paste("$", income, "K")

png("plot1.png")
bar <- barplot(income,
        main="Median House Hold Income",
        xlab = "Year",
        ylab="Median Income (1000's of USD)",
        col= "lightblue",
        border=FALSE, 
        names.arg = year,
        ylim=c(0,60)
        )
text(x=bar,y=income+2, labels=value, cex=0.8)
dev.off()
