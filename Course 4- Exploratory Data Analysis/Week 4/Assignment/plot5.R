#Question 5:How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

#Unzip the dataset
unzip(zipfile = "./data/exdata_data_NEI_data.zip", exdir = "./data")

#Reading data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")



#subsetting city of Baltimore data
Baltimore <- subset(NEI, fips=="24510")

# subsetting SCC with Vehicle sources value
vehicle_source <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
subsetSCC_vehicle <- SCC[vehicle_source, ]


# merging dataframes for Baltimore city with vehicle source by SCC
NEISCC <- merge(Baltimore, subsetSCC_vehicle, by="SCC")




# summing emission data per year
total_Emissions_BV <- tapply(NEISCC$Emissions, NEISCC$year, sum)

# plotting
png("plot5.png", width=480, height=480)
barplot(total_Emissions_BV, 
        col=rainbow(4), xlab = "Year",
        ylab=expression("Total PM"[2.5]*" Emissions (tons)"),
        main = "Total Emission from motor sources in Baltimore, Maryland")

dev.off()
