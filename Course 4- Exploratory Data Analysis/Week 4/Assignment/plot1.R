#Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using 
#the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of 
#the years 1999, 2002, 2005, and 2008.


#unzip downloaded file
unzip(zipfile = "./data/exdata_data_NEI_data.zip", exdir = "./data")

#reading data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
dim(NEI)
table(NEI$year)

#taking the sum of emission data per year
total_Emission <-tapply(NEI$Emissions, NEI$year, sum)
dim

#plotting Total Emission per year
png("plot1.png", width=480, height=480)
plot(total_Emission, 
        col="green", xlab="year", 
        type="l",
        ylab=expression("Total PM"[2.5]*" Emissions (tons)"), 
        main="Total Emissions Per Year")

dev.off()