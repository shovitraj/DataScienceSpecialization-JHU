#Question 1
fileurl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
question1 <- read.csv(fileurl1)
question1
head(question1)
question1_cols <- names(Q1)
question1_cols
strsplit(question1_cols, "^wgtp")[[123]]

#Question 2
fileurl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
question2 <- "./data/Q3GDP.csv"
download.file(fileurl2, question2, method = "curl")
head(question2)
q2_file <- read.csv(Q2_Path, nrow = 190, skip = 4)
head(q2_file)
q2_file <- q2_file[,c(1, 2, 4, 5)]
colnames(q2_file) <- c("CountryCode", "Rank", "Country", "Total")
q2_file
head(q2_file)

q2_file$Total <- as.integer(gsub(",", "", q2_file$Total))
mean(q2_file$Total, na.rm = T)

#Question 3

q2_file$Country <- as.character(q2_file$Country)
q2_file$Country[99] <- "Côte d’Ivoire"
q2_file$Country[186] <- "São Tomé and Príncipe"

q2_file$Country[grep("^United", q2_file$Country)]


#Question 4

library(dplyr)
library(data.table)
GDPUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
GDPPath <- "./data/Q3GDP.csv"
EduUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
EduPath <- "./data/Q3Edu.csv"

download.file(GDPUrl, GDPPath, method = "curl")
download.file(EduUrl, EduPath, method = "curl")

GDP <- fread(GDPPath, skip = 5, nrows = 190, select = c(1, 2, 4, 5), col.names = c("CountryCode", "Rank", "Economy", "Total"))
Edu <- fread(EduPath)
GDP
Edu
Merge <- merge(Q3GDP, Q3Edu, by = 'CountryCode')
names(Merge)
Merge$'Special Notes'
FiscalJune <- grep("Fiscal year end: June", Merge$`Special Notes`)
FiscalJune
NROW(FiscalJune)

#Question 5
library(quantmod)
amzn = getSymbols("AMZN", auto.assign=FALSE)
sampleTimes = index(amzn)
amzn2012 <- sampleTimes[grep("^2012", sampleTimes)]
NROW(amzn2012)
NROW(amzn2012[weekdays(amzn2012) == "Monday"])




