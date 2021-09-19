#question 3: Of the four types of sources indicated by the \color{red}
#{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of
#these four sources have seen decreases in emissions from 1999–2008 for 
#Baltimore City? Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

#Unzip the dataset
unzip(zipfile = "./data/exdata_data_NEI_data.zip", exdir = "./data")

library(ggplot2)

#Reading data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

#subsetting data for the city of Baltimore
Baltimore <- subset(NEI, fips=="24510")
head(Baltimore)
table(Baltimore$year)

# takng sum of emission data per year by type(point, nonpoint, onroad, nonroad) 
data_Balt <- aggregate(Emissions ~ year + type, Baltimore, sum)
#head(data_Balt)

#plotting data Emission per year by type(point, nonpoint, onroad, nonroad) 
png("plot3.png", width=480, height)
g <- ggplot(data_Balt, aes(year, Emissions, color = type))
g + geom_line() +
        xlab("Year") +
        ylab(expression("Total PM"[2.5]*" Emissions (tons)")) +
        ggtitle("Total Emissions per type in Baltimore, Maryland")


dev.off()