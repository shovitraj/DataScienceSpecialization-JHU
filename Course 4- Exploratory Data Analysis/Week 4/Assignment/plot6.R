#Question 6: Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California
#(\color{red}{\verb|fips == "06037"|}fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

#Unzip the dataset
unzip(zipfile = "./data/exdata_data_NEI_data.zip", exdir = "./data")

library(ggplot2)

#Reading data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")


#subsetting Los Angeles and Baltimore data
LA <- subset(NEI, fips == "06037")
Baltimore <- subset(NEI, fips=="24510")

# subsetting SCC with vehicle values
vehicle  <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
subsetSCC <- SCC[vehicle, ]

# merging Baltimore and LA data with  SCC vehicles 
data_Balt <- merge(Baltimore, subsetSCC, by="SCC")
data_LA <- merge(LA, subsetSCC, by="SCC")

#adding city variable
data_Balt$city <- make.names("Baltimore City")
data_LA$city <- make.names("Los Angeles County")

#merging Baltimore and Los Angeles data
data_merged <- rbind(data_Balt, data_LA)

# summing emission data per year by city
data_LA_Balt<-aggregate(Emissions ~ year + city, data_merged, sum)

# plotting
png("plot6.png", width=560, height=480)
g <- ggplot(data_LA_Balt, aes(year, Emissions, color = city))
g + geom_line() +
        xlab("Year") +
        ylab(expression("Total PM"[2.5]*" Emissions")) +
        ggtitle("Total Emissions from motor sources in Baltimore,MD and Los Angeles, CA")
dev.off()
