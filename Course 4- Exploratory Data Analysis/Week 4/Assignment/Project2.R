if(!file.exists("./data")){dir.create("./data")}

#Unzip the dataset
unzip(zipfile = "./data/exdata_data_NEI_data.zip", exdir = "./data")

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

head(NEI)
head(SCC)

names(NEI)
names(SCC)

dim(NEI)
dim(SCC)

head(NEI)

#Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using 
#the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of 
#the years 1999, 2002, 2005, and 2008.

table(NEI$year)
netEmission <-tapply(NEI$Emissions, NEI$year, sum)
png("plot1.png")
barplot(netEmission, xlab="year", ylab="Total Emissions (Tons)", main="Total Emissions Per Year")

dev.off()


#Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}
#{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? Use the base plotting system to make a plot
#answering this question.
baltimore <- subset(NEI, fips="24510")
head(baltimore)
table(baltimore$year)
netEmission <-tapply(baltimore$Emissions, baltimore$year, sum)
png("plot2.png")
barplot(netEmission, xlab="year", ylab="Total Emissions (Tons)", main="Total Emissions Per Year")

dev.off()


#question 3: Of the four types of sources indicated by the \color{red}
#{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of
#these four sources have seen decreases in emissions from 1999–2008 for 
#Baltimore City? Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

baltimore <- subset(NEI, fips="24510")
head(baltimore)
table(baltimore$year)
data <- aggregate(Emissions ~ year + type, baltimore, sum)
head(data)
png("plot3.png")
g <- ggplot(data, aes(year, Emissions, color = type))
g + geom_line() +
        xlab("Year") +
        ylab(expression("Total PM"[2.5]*" Emissions")) +
        ggtitle("Total Emissions per type in Baltimore")


dev.off()

#Question 4:Across the United States, how have emissions from coal 
#combustion-related sources changed from 1999–2008?

# subsetting SCC with coal values
#coal <- grepl("coal", SCC$Short.Name, ignore.case=TRUE)
subsetSCC_coal <- SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE), ]
coal
head(subsetSCC_coal)
dim(NEI)
dim(SCC)

# merging dataframes
NEISCC <- merge(NEI, subsetSCC_coal, by="SCC")
NEISCC
head(NEISCC)
names(NEISCC)
names(NEI)
SCC$SCC
NEI$SCC



# summing emission data per year
totalEmissions <- tapply(NEISCC$Emissions, NEISCC$year, sum)

# plotting
png("plot4.png")
barplot(totalEmissions, xlab = "Year", ylab = "Total Emission (ton)", 
        main = "Total Emission from coal sources")

dev.off()

#Question 5:How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
baltimore <- subset(NEI, fips="24510")

subsetSCC_vehicle <- SCC[grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE), ]


# merging dataframes
NEISCC <- merge(baltimore, subsetSCC_vehicle, by="SCC")
NEISCC
head(NEISCC)
names(NEISCC)
names(NEI)
SCC$SCC
NEI$SCC



# summing emission data per year
totalEmissions <- tapply(NEISCC$Emissions, NEISCC$year, sum)

# plotting
png("plot5.png")
barplot(totalEmissions, xlab = "Year", ylab = "Total Emission (ton)", 
        main = "Total Emission from motor sources in Baltimore")

dev.off()

#Question 6: Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California
#(\color{red}{\verb|fips == "06037"|}fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

LA <- subset(NEI, fips="06037")
baltimore <- subset(NEI, fips="24510")
subsetSCC_vehicle <- SCC[grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE), ]
data_balt<-merge(baltimore, subsetSCC_vehicle, by="SCC")
data_LA<-merge(LA, subsetSCC_vehicle, by="SCC")
head(data_balt)
dim(data_balt)
dim(data_LA)
names(data_balt)
names(data_balt)
data_balt$city <- "Baltimore"
data_LA$city <- "Los Angeles"
data <- rbind(data_balt, data_LA)
data_final <- aggregate(Emissions ~ year + city, data, sum)
png("plot6.png")
g <- ggplot(data_final, aes(year, Emissions, color = city))
g + geom_line() +
        xlab("Year") +
        ylab(expression("Total PM"[2.5]*" Emissions")) +
        ggtitle("Total Emissions per type in Baltimore and Los Angeles")


dev.off()


