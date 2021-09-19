#Question 4:Across the United States, how have emissions from coal 
#combustion-related sources changed from 1999–2008?


#Unzip the dataset
unzip(zipfile = "./data/exdata_data_NEI_data.zip", exdir = "./data")

#Reading data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

total_Emission <-tapply(NEI$Emissions, NEI$year, sum)

# subsetting SCC with coal source values
coal <- grepl("coal", SCC$Short.Name, ignore.case=TRUE)
subsetSCC_coal <- SCC[coal, ]

#head(subsetSCC_coal)


# merging dataframes by "SCC"
NEISCC <- merge(NEI, subsetSCC_coal, by="SCC")


# summing emission data per year
total_Emissions_coal <- tapply(NEISCC$Emissions, NEISCC$year, sum)
total_Emissions_coal

# plotting
png("plot4.png", width=480, height=480)
barplot(total_Emissions_coal, col=rainbow(4),
        xlab = "Year", 
        ylab = expression("Total PM"[2.5]*" Emissions (tons)"), 
        main = "Total Emission from coal sources")

dev.off()


