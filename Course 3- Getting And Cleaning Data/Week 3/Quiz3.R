#Quiz 3

#Question 1

#Download File

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
question1 <- read.csv(fileUrl)
question1
head(question1)
names(question1)
agricultureLogical <- question1$ACR ==3 & question1$AGS == 6
which(agricultureLogical)
#Answer: 125 238 262

#Question 2
library(jpeg)
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
path = './data/2Fjeff.jpg'
download.file(fileUrl1, path, mode = 'wb')
question2 <- readJPEG(path, native = TRUE)
quantile(question2, probs = c(0.3, 0.8))
#Answer:       30%       80% 
#           -15259150 -10575416 

#Question 3
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
head(Merge)
Mergedesc <- Merge %>% arrange(desc(Rank))
head(Mergedesc)

paste(nrow(Mergedesc), " matches, 13th country is ", Q3_Merge$Economy[13])

#Answer: "189  matches, 13th country is  St. Kitts and Nevis"

#Question 4
Merge %>% group_by(`Income Group`) %>%
        filter("High income: OECD" %in% `Income Group` | "High income: nonOECD" %in% `Income Group`) %>%
        summarize(Average = mean(Rank, na.rm = T)) 
        
#Question 5
?cut
MergeRankGroups <- cut(Merge$Rank, breaks = 5)
vs <- table(MergeRankGroups, Merge$`Income Group`)
vs
vs[1, "Lower middle income"]

#Answer: 5
