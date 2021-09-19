
if(!file.exists("./data")){dir.create("./data")}

url <-("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
filepath <- "./data/activity.zip"
download.file (url, filepath)
unzip(zipfile = "./data/activity.zip", exdir = "./data")

#### Loading and preprocessing the data
library(ggplot2)
library(dplyr)
activity <- read.csv("./data/activity.csv")
str(activity)

head(activity)
dim(activity)
class(activity)
names(activity)
activity

##### What is mean total number of steps taken per day?

###### Calculate the total number of steps taken per day

Steps_Daily <- aggregate(activity$steps,list(activity$date), sum)
colnames(Steps_Daily) <- c("Date", "Steps")
str(Steps_Daily)




###### If you do not understand the difference between a histogram and a barplot, 
###### research the difference between them. Make a histogram of the total number 
###### of steps taken each day

g <- ggplot(Steps_Daily, aes(Steps))
g+geom_histogram(boundary=2, binwidth=2200, 
                 col="turquoise3", fill="turquoise2")+
                 ggtitle("Histogram of steps per day")+
                 xlab("Steps")+ylab("Frequency")
#hist(dailysteps, col=rainbow(5), xlab="Number of Steps", main="Steps per Day")

###### Calculate and report the mean and median of the total number of steps taken per day
mean_StepsDaily <- mean(Steps_Daily$Steps, na.rm=TRUE)
mean_StepsDaily
median_StepsDaily<- median(Steps_Daily$Steps, na.rm=TRUE)
median_StepsDaily

##### What is the average daily activity pattern?

Steps_Interval <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
interval<- as.numeric(names(Steps_Interval))
Steps_Interval

g <- ggplot(Steps_Interval, aes(interval, steps))
g+geom_line(col="red")+
        ggtitle("Average steps per time interval")+
        xlab("Interval")+ylab("Steps")+
        theme(plot.title = element_text(face="bold", size=12))


#plot(interval, steps_interval,
#     xlab="Interval",
#     ylab="Average Steps",
#     main="Average Daily Activity Pattern",
#     type="l")

###### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
summary(Steps_Interval)
str(Steps_Interval)
class(Steps_Interval)
maxsteps <- max(Steps_Interval)
maxsteps
#maxinterval <- steps_interval[Steps_Interval==maxsteps]
#maxinterval
maxinterval <- filter(Steps_Interval, steps == max(steps))
maxinterval
class(maxinterval)
max_interval <- maxinterval[1]
max_steps <- maxinterval[2]

##### Inputing missing values
###### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


missing_values <- sum(is.na(activity$steps))
missing_values
activity$Steps

replaced_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
imputedActivity <- activity %>% group_by(interval) %>% mutate(steps= replaced_mean(steps))
head(imputedActivity)
imputedSteps
str(imputedActivity)

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of
#the total daily number of steps?

total_Steps_Daily <- aggregate(imputedActivity$steps,list(imputedActivity$date), sum)
colnames(total_Steps_Daily) <- c("Date", "Steps")
g <- ggplot(total_Steps_Daily, aes(Steps))
g+geom_histogram(boundary=2, binwidth=2200, 
                 col="turquoise3", fill="turquoise2")+
        ggtitle("Histogram of steps per day")+
        xlab("Steps")+ylab("Frequency")

summary(total_Steps_Daily)
class(total_Steps_Daily)
newmean<-mean(total_Steps_Daily$Steps)
newmean
mean_StepsDaily
newmedian<- median(total_Steps_Daily$Steps)
newmedian
median_StepsDaily


#### Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


imputedActivity$date <- as.Date(imputedActivity$date)
imputedActivity$weekday <- weekdays(imputedActivity$date)
imputedActivity$weekday
imputedActivity$weekend <- ifelse(imputedActivity$weekday=="Saturday" | imputedActivity$weekday=="Sunday", "Weekend", "Weekday" )
imputedActivity$weekend
head(imputedActivity$weekend, 10)



Activity_Week_Day_End <- aggregate(imputedActivity$steps , by= list(imputedActivity$weekend, imputedActivity$interval), na.omit(mean))
names(Activity_Week_Day_End) <- c("weekend", "interval", "steps")


g<-ggplot(Activity_Week_Day_End, aes(x=interval, y=steps, color=weekend)) 
g+ geom_line()+
        facet_grid(weekend ~.) + xlab("Interval") + ylab("Number of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval") +
        theme_light()+
        theme(legend.position = "bottom", legend.title = element_blank()) 




