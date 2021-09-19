#Quiz1

fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl1, destfile = "./data/Quiz1-01.csv", method = "curl")
quiz1Data <- read.csv("./data/Quiz1-01.csv")
head(quiz1Data)
sum(quiz1Data$VAL == 24, na.rm = TRUE)


#Question 3

#Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0.jdk/Contents/Home/jre")
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl1, destfile = "./data/Quiz1-03.xlsx", method = "curl")
dateDownloaded <- date()
dateDownloaded
library(openxlsx)
col <- 7:15
row <- 18:23
dat <- read.xlsx("./data/Quiz1-03.xlsx", sheetIndex=1, colIndex = col, rowIndex = row)
dat
sum(dat$Zip*dat$Ext, na.rm=T)


#Question 4
library(XML)
library(httr)
fileUrl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
#BalResto <- xmlTreeParse(sub("s", "", fileUrl3), useInternal=TRUE) use this with library(XML)
BalResto <- xmlTreeParse(GET(fileUrl3), useInternal=TRUE) #use this with library(XML) & library(httr)
rootNode <- xmlRoot(BalResto)
zip <- xpathSApply(rootNode, "//zipcode", xmlValue)
sum(zip == 21231)

#Question 5
fileUrl4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl4, destfile = "./data/Quiz1-05.csv", method = "curl")
library(data.table)
DT <- read.csv("./data/Quiz1-05.csv")
DT

system.time(rowMeans(DT[DT$SEX==1]), rowMeans(DT[DT$SEX==2]))
system.time(mean(DT[DT$SEX==1,]$pwgtp15), mean(DT[DT$SEX==2,]$pwgtp15))
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(mean(DT$pwgtp15,by=DT$SEX))
