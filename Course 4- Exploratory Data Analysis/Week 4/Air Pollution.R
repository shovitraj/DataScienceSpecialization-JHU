#EPA Air Polluton Case Study



filepath1<- file.path("./data/pm25_data/RD_501_88101_1999-0.txt")
filepath2<- file.path("./data/pm25_data/RD_501_88101_2012-0.txt")
#Reading 1999 data
pm0<- read.table(filepath1, comment.char="#", header=FALSE, sep="|", na.strings="")
dim(pm0)
head(pm0)
cnames <- readLines(filepath1,1)
cnames <- strsplit(cnames, "|", fixed = TRUE)
cnames
## Ensure names are properly formatted
names(pm0) <- make.names(cnames[[1]])  
head(pm0[, 1:13])


x0 <- pm0$Sample.Value
class(x0)
summary(x0)
mean(is.na(x0))

#Reading 2012 data
pm1<- read.table(filepath2, comment.char="#", header=FALSE, sep="|", na.strings="")
dim(pm1)
head(pm1)
cnames1 <- readLines(filepath2, 1)
cnames1 <- strsplit(cnames1, "|", fixed = TRUE)
names(pm1) <- make.names(cnames1[[1]])  
head(pm1[,1:13])
head(pm1[,1:28])
x1 <- pm1$Sample.Value
class(x1)


## Five number summaries for both periods
summary(x1)
summary(x0)
mean(is.na(x1))



## Make a boxplot of both 1999 and 2012
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))


## Check negative values in 'x1'
negative <- x1 <0
str(negative)
sum(negative, na.rm=TRUE)
mean(negative, na.rm=TRUE)
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month")
hist(dates[negative], "month")

## Plot a subset for one monitor at both times

## Find a monitor for New York State that exists in both datasets

site0 <- unique(subset(pm0, State.Code==36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code==36, c(County.Code, Site.ID)))
#new variable with county code and site.ID pasted together
site0 <- paste(site0[,1], site0[,2], sep=".")
site1 <- paste(site1[,1], site1[,2], sep=".")
str(site0)
str(site1)

both <- intersect(site0, site1)
both

## Find how many observations available at each monitor
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep="."))
pm0$county.site
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep="."))
pm1$county.site 
cnt0 <- subset(pm0, State.Code==36 & county.site %in% both)
cnt0
cnt1 <- subset(pm1, State.Code==36 & county.site %in% both)
cnt1
head(cnt0)
head(cnt1)

split(cnt0, cnt0$county.site)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

## Choose county 63 and side ID 2008
pm1sub <- subset(pm1, State.Code == 36 & County.Code==63 & Site.ID==2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code==63 & Site.ID==2008)
dim(pm1sub)
dim(pm0sub)


## Plot data for 1999
dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

## Plot data for 2012
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
str(dates1)
plot(dates1, x1sub)

## Plot data for both years in same panel
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20)  ## Whoa! Different ranges
abline(h = median(x1sub, na.rm = T))

## Find global range
rng <- range(x0sub, x1sub, na.rm = T)
rng
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

## Show state-wide means and make a plot showing trend
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)

## Make separate data frames for states / years
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

## Connect lines
par(mfrow = c(1, 1))
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
with(mrg, points(rep(2, 52), mrg[, 3]))
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])


#how many observations available


                            

