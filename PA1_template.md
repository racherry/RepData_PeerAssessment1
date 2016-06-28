# Reproducible Research Assignment 1
RAC  
June 26, 2016  
### Loading and preprocessing the data  
If the assignment.zip file is not already downloaded, it is accessed from https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip and unzipped.  No further processing is carried out.  

```r
setwd("~/Desktop/2016 Coursera Data Science/05_Reproducible Research")
if(!file.exists("./data")){dir.create("./data",showWarnings = TRUE)}
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("./data/assignment.zip")){download.file(URL,"./data/assignment",method = "curl" )}
unzip("./data/assignment")
rawdata <- read.csv("activity.csv")
```

### What is mean total number of steps taken per day?  

```r
library(dplyr)
library(tidyr)
options("scipen"=100, "digits"=1) # to supress scientific notation in the output

dailysteps <- rawdata %>%
  group_by(date) %>%
  summarise(total=sum(steps))

hist(dailysteps$total,main = "Histogram -  Total Steps Taken Each Day",xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
average <- mean(dailysteps$total,na.rm = TRUE)
med <- median (dailysteps$total, na.rm = TRUE)
```
The **mean** of the total number of steps taken per day is **10766.2**.  
The **median** of the total number of steps taken per day is **10765**.  

### What is the average daily activity pattern?    

```r
intervalsteps <- rawdata %>%
  group_by(interval) %>%
  summarise(aveinterval=mean(steps,na.rm=TRUE))

intervalmax <- intervalsteps[which.max(intervalsteps$aveinterval),1]

plot(intervalsteps$interval,intervalsteps$aveinterval, type="l", main="Average Total Steps by Interval",xlab = "5-minute interval", ylab = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

The 5-minute interval on average across all the days in the dataset that contains the maximum number of steps is interval **835**.   

### Imputing missing values  

```r
totalna <- sum(is.na(rawdata$steps))
```
The **total number of NA steps** in the dataset is **2304**.  


```r
impdata <- rawdata %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = T), steps))
```
I chose to impute the NA values with the mean of the total steps in that 5 minute interval.  
**impdata** is the new dataset which contains the imputed data. As confirmation, the total number of NAs in **impdata** is **0**.  


```r
impdailysteps <- impdata %>%
  group_by(date) %>%
  summarise(total=sum(steps))
hist(impdailysteps$total,main = "Histogram -  Total Steps Taken Each Day",xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

```r
impaverage <- mean(impdailysteps$total,na.rm = TRUE)
impmed <- median (impdailysteps$total, na.rm = TRUE)
```
In the imputed data, the **mean** of the total number of steps taken per day is **10766.2**.  
For the imputed data, the **median** of the total number of steps taken per day is **10766.2**.  
Imputing the missing data using the method described made next to no change to the mean or the median compared to the raw data set.  

### Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
library(ggplot2)
wddata <- mutate(impdata,DOW = wday(date)) %>%
            mutate(WD = ifelse(DOW == 7 | DOW == 1, "weekend", ifelse(DOW >1 & DOW <7,"weekday",0)))
wdintervalsteps <- wddata %>%
  group_by(interval,WD) %>%
  summarise(aveinterval=mean(steps,na.rm=TRUE))
```
The package **lubridate** was used to handle the parsing of the dates in the dataset.  


```r
ggplot(wdintervalsteps,aes(x=interval,y=aveinterval))+geom_line(color="blue")+facet_wrap(~WD,nrow=2,ncol=1)+labs(x="Interval",y="Number of Steps")+theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

Examining the activity patterns reveals some differences between weekdays and weekends.  On weekdays activity begins earlier rise shows a higher peak in the total steps. Weekends show a delayed rise but a more sustained elevation across the intervals that extends later in the day.
