#2 Quiz 2 #Answe 2013-11-07T13:25:07Z
#Question 1
library(httr)
#Find OAuth settings for github: http://developer.github.com/v3/oauth/


oauth_endpoints("github")
#Register an application at https://github.com/settings/applications
gitapp <- oauth_app("Shovit_Github",
                    key = "c62c71dd1fb7d6fd64fa",
                    secret = "1783ffe17c788a39bb79b683b84d0051f599be03")

#Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), gitapp)
#Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))
stop_for_status(req)
output <- content(req)

#Find “datasharing”
datashare <- which(sapply(output, FUN=function(X) "datasharing" %in% X))
datashare
#Find the time that the datasharing repo was created.
list(output[[15]]$name, output[[15]]$created_at)

#library(jsonlite)

#json1 = content(req)
#json2 = jsonlite::fromJSON(toJSON(json1))
#repo <- json2[5]
#names(repo)

#repo$created_at

#Question 2 #Answer: sqldf(“select pwgtp1 from acs where AGEP < 50”)
library(sqldf)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", destfile = "quiz2data.csv")

acs <- read.csv("quiz2data.csv")

sqldf("select pwgtp1 from acs where AGEP < 50")

#Question 3 Answer: sqldf(“select distinct AGEP from acs”)



sqldf("select distinct AGEP from acs")


#Question 4 Answer: 45 31 7 25

require(httr);require(XML)
URL <- url("http://biostat.jhsph.edu/~jleek/contact.html")
lines <- readLines(URL)
close(URL)
c(nchar(lines[10]), nchar(lines[20]), nchar(lines[30]), nchar(lines[100]))

#Question 5 #Answer: 32426.7
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
widths <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)

fixed <- read.fwf(url, widths, header = FALSE, skip = 4)
head(fixed)
sum(fixed$V8)





