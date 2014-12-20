---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data
- set working directory

```r
setwd("/Users/liwenlong/Documents/Coursera/RepDataAndStatInference/RepData_PeerAssessment1")
getwd()
```

```
## [1] "/Users/liwenlong/Documents/Coursera/RepDataAndStatInference/RepData_PeerAssessment1"
```
- Since the data is in a zip file, need to unzip it first

```r
csvfile<-"activity.csv"
if(!exists(csvfile)){
     unzip(zipfile = "activity.zip")   
}
```
- Read the data to the object Activity  
  before this ,roughly review the data and found there is column headers and has NA value in the "steps" column

```r
Activity<-read.csv(csvfile,header = T)
str(Activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
- Transform date column to Date format

```r
library(lubridate)
attach(Activity)
date<<-ymd(as.character(Activity$date))
Activity$time<-hm(paste(floor(interval/100),interval%%100,sep=" "))
detach(Activity)
str(Activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ time    :Formal class 'Period' [package "lubridate"] with 6 slots
##   .. ..@ .Data : num  0 0 0 0 0 0 0 0 0 0 ...
##   .. ..@ year  : num  0 0 0 0 0 0 0 0 0 0 ...
##   .. ..@ month : num  0 0 0 0 0 0 0 0 0 0 ...
##   .. ..@ day   : num  0 0 0 0 0 0 0 0 0 0 ...
##   .. ..@ hour  : num  0 0 0 0 0 0 0 0 0 0 ...
##   .. ..@ minute: num  0 5 10 15 20 25 30 35 40 45 ...
```
- I tried to convert the interval to time,but meet a problem when using [lubritime], have no idea how to use it

```r
head(Activity$time)
```

```
## [1] "0S"     "5M 0S"  "10M 0S" "15M 0S" "20M 0S" "25M 0S"
```

```r
unique(Activity$time)
```

```
## [1] 0
```

## Task1, What is mean total number of steps taken per day?
  - Ignore the missing values in the dataset. (If the data of a day is all missed, this day will be removed)
  - Make a histogram of the total number of steps taken each day
  - Calculate and report the mean and median total number of steps taken per day

```r
StepsPerDay<-aggregate(steps~date,data=Activity,sum,na.rm=T)
# Notice result is different with the follow statement. 
#the day with all NA values are kept with 0 steps. 
#StepsPerDay<-aggregate(Activity$steps, by = list(date=Activity$date), FUN=sum, na.rm=T)
dim(StepsPerDay)
```

```
## [1] 53  2
```

```r
DailySteps<-StepsPerDay$steps
hist(x = DailySteps,breaks = 20)
```

![plot of chunk Task1](figure/Task1-1.png) 

```r
options(digits = 2)
meanSteps<-mean(DailySteps)
medianSteps<-median(DailySteps)
```
The mean of the total number of steps taken per day is : 1.08 &times; 10<sup>4</sup>, while the median is : 10765. 

## Task2, What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
AverageIntervalSteps<-aggregate(steps~interval, data=Activity, mean, na.rm=T)
dim(AverageIntervalSteps)
```

```
## [1] 288   2
```

```r
with(AverageIntervalSteps,{
        plot(interval,steps,
             type="l",
             main = "Average number of steps taken",
             xlab="Interval, Time(hmm/hhmm)",
             ylab="average steps cross all days")
                
})
```

![plot of chunk Task2](figure/Task2-1.png) 

```r
maxInterval<-AverageIntervalSteps[which.max(AverageIntervalSteps$steps),"interval"]
start<-paste(floor(maxInterval/100),maxInterval%%100,sep = ":")
end<-paste(floor(maxInterval/100),maxInterval%%100+5,sep = ":")
```
The interval contains max average steps is **835**, which represent from **8:35**  to **8:40**;  
It should be the time that people goes to work. 

## Task3, Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
table(is.na(Activity))
```

```
## 
## FALSE  TRUE 
## 67968  2304
```

```r
sum(is.na(Activity$steps))
```

```
## [1] 2304
```
- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
NewActivity<-Activity

for(i in 1:length(NewActivity$steps)){
        if(is.na(NewActivity$steps[i])){
                NewActivity$steps[i]<-
                        AverageIntervalSteps$steps[AverageIntervalSteps$interval==NewActivity$interval[i]]
        }
}

sum(is.na(NewActivity$steps))
```

```
## [1] 0
```

```r
sum(is.na(NewActivity$steps))
```

```
## [1] 0
```
- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
StepsPerDay_New<-aggregate(steps~date,data=NewActivity,sum,na.rm=T)
DailySteps_New<-StepsPerDay_New$steps
opar<-par()
par(mfcol = c(1,2))
hist(DailySteps,20,col = "grey",ylim = c(0,15))
hist(DailySteps_New,20,col="blue",ylim =c(0,15))
```

![plot of chunk Task3](figure/Task3-1.png) 

```r
par<-opar
options(digits = 2)
mean(DailySteps_New)
```

```
## [1] 10766
```

```r
median(DailySteps_New)
```

```
## [1] 10766
```
Since in the original data, if the data of a day is all NA, the day will be removed from the dataset. 
So In the imputing histgram, the fix data with average interval steps are added into the most popular bucket.  
  
Keep the wrong plot I had made (which seems interesting ), be carefull next time.

```r
with(AverageIntervalSteps,{
        plot(interval,steps,
             type="l",
             col="red",
             main = "Average number of steps taken",
             xlab="Interval, Time(hmm/hhmm)",
             ylab="average steps cross all days")
                
})
AverageIntervalStepsNew<-aggregate(steps~interval,data=NewActivity, mean, na.rm=T)
with(AverageIntervalStepsNew,{
        lines(x=interval,y=steps, col = "blue", lty ="twodash" )
})
legend("topright", 
       legend = c("Original","Fix Missing Value"), 
       col = c("red","blue"), 
       lty = c("solid","twodash"),cex=0.8)
```

![plot of chunk Task3.backup](figure/Task3.backup-1.png) 

```r
maxInterval<-AverageIntervalStepsNew[which.max(AverageIntervalStepsNew$steps),"interval"]
start<-paste(floor(maxInterval/100),maxInterval%%100,sep = ":")
end<-paste(floor(maxInterval/100),maxInterval%%100+5,sep = ":")
```
The interval contains max average steps is **835**, which represent from **8:35**  to **8:40**;  
Which is the same as the values estimated with missing values and shows that imputing missing data on the estimates of the total daily number of steps has no impact

## Task4, Are there differences in activity patterns between weekdays and weekends?
- Firstly, plot the data of workdays and weekends together

```r
NewActivity$weekday<-wday(NewActivity$date,label=T)
NewActivity$DayType<-ifelse(NewActivity$weekday %in% c("Sun","Sat"),"weekends","workday")
AverageIntervalStepsWeekends<-
        aggregate(NewActivity$steps,
                  by=list(DayType=NewActivity$DayType,interval=NewActivity$interval),
                  FUN = mean,
                  na.rm=T)
library(ggplot2)
qplot(data=AverageIntervalStepsWeekends,
      x = interval,
      y = x,
      geom="line",
      col=DayType,
      xlab="Interval, Time(hmm/hhmm)",
      ylab="average steps cross all days")+theme(legend.position = "top")
```

![plot of chunk Task4.Main](figure/Task4.Main-1.png) 

- Secondly, calculate the compare the mean/median between weekdays and weekends.

```r
attach(AverageIntervalStepsWeekends)
aggregate(x,FUN = mean,by=list(dayType=DayType))
```

```
##    dayType  x
## 1 weekends 42
## 2  workday 36
```

```r
aggregate(x,FUN = median,by=list(dayType=DayType))
```

```
##    dayType  x
## 1 weekends 32
## 2  workday 26
```

```r
detach(AverageIntervalStepsWeekends)
```

We can see that the average steps of weekends is more than weekdays;  
And the time series pattern is also different:  
 a. In the morning(5~10 o' clock), weekend < workdays;  
 b. In the daylight(10~20 o' clock), weekend > workdays;

- Finally, plot the data by weekdays to further understand.

```r
AverageIntervalStepsPerWeekday<-
        aggregate(NewActivity$steps,
                  by=list(weekday=NewActivity$weekday,interval=NewActivity$interval),
                  FUN = mean,
                  na.rm=T)

qplot(data=AverageIntervalStepsPerWeekday,
      x = interval,
      y = x, 
      geom="line",
      facets =.~ weekday, 
      ylab="average steps",
      xlab="Interval, Time(hmm/hhmm)")
```

![plot of chunk Task4.Extra](figure/Task4.Extra-1.png) 

