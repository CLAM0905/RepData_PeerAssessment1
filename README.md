---
title: 'Reproducible Research: Course Assignment 1'
author: "CLAM0905"
date: "October 28, 2019"
output:
  html_document: default
  keep_md: true
  output: md_document
---
Summary:
1.    Code for reading in the dataset and/or processing the data
        Activity <- read.csv("C:/Users/klein337/Documents/RProgWork/activity.csv")
        Activity$date <- as.Date(Activity$date,"%Y-%m-%d")
        View(Activity)
        
2.    Histogram of the total number of steps taken each day
        See "Totalstepsbyday"
        
3.    Mean and median number of steps taken each day
        mean: The mean number of steps taken each day is 10766 steps
        median: The median number of steps taken each day is 10765 steps
        
4.    Time series plot of the average number of steps taken
        See "Totalstepsnona"
        
5.    The 5-minute interval that, on average, contains the maximum number of steps
        Interval: The interval that contains the maximum number of steps is interval 835
        
6.    Code to describe and show a strategy for imputing missing data
        Used the mean total of the day to replace NA values, and 0 if there were no steps taken at all.
        code:
          ActivityLong <- dcast(Activity, interval ~ date, value.var = "steps") #dcast from long to wide
          ActivityLongNA <- na_mean(ActivityLong, option = "mean") #replace NA with mean for the day
          ActivityLongNA[is.na(ActivityLongNA)] <- 0 #replace remaining NA with 0, no steps that day
          View(ActivityLongNA)
  
7.    Histogram of the total number of steps taken each day after missing values are imputed
        See "Totalstepsyesna"

8.    Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
        See "intervalbysteps5min"
        
9.    All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
        See "PA1_template.Rmd"



Full code:

# **Course 5 Reproducible Research, Project 1**
This assignment analyzes the amount of  steps taken during distinct 5-minute time intervals over the 24 hour day. Variables included in the original dataset are:
   ** Steps: number of steps taken in a 5 minute interval, with missing values coded as NA **
   ** Date: The date on which the measurement was taken in YYYY-MM-DD format **
   ** Interval: Identifier for the 5-minute interval in which measurement was taken **

Below is the analysis on this data: 

##Step 1. Loading and preprocessing the data
```{r}
Activity <- read.csv("C:/Users/klein337/Documents/RProgWork/activity.csv")
Activity$date <- as.Date(Activity$date,"%Y-%m-%d")
View(Activity)
```
##Step 2. What is the mean total number of steps taken per day?
###2a. Total Number of Steps taken Per day 
**The following code and table shows the total number of steps taken per day, omitting the NA values:**
```{r}
TotalSteps <- aggregate(steps ~ date, data = Activity, sum, na.action = na.omit)
TotalSteps
```

###2b. Histogram of the total number of steps taken each day
```{r totalstepsbyday}
hist(TotalSteps$steps, breaks=30, col="red", xlab="Steps", main="Total number of Steps Taken Each Day")
```

###2c. Mean and median number of steps taken each day
####The mean number of steps taken each day is 10766 steps. The below code also includes a table for the mean number of steps taken by day:
```{r eval = TRUE}
meanStepsPerDay <- aggregate(steps ~ date, Activity, mean, na.action = na.omit)
meanStepsPerDay
meanStepsTotal <- mean(TotalSteps$steps)
```

#####The median number of steps taken each day is 10765 steps. The below code also includes a table of the median number of steps taken by day:
```{r eval = FALSE}
medianStepsPerDay <- aggregate(steps ~ date, Activity, median, na.action = na.omit)
medianStepsPerDay
medianStepsTotal <- median(TotalSteps$steps)
```

##Step3. What is the average daily pattern? 
###3a. Time series plot of the 5-minute interval and average number of steps taken, averaged across all days:
```{r}
meanStepsPerInterval <- aggregate(steps ~ interval, Activity, mean, na.action = na.omit)
```

```{r totalstepsyesna}
library(ggplot2)
TimeInterval <- ggplot(meanStepsPerInterval, aes(interval, steps)) + geom_line()
print(TimeInterval + ggtitle("Time Series Plot of Average Number of Steps per 5 Minute Interval over 61 day period"))
```

###3b. Which 5-minute interval that, on average, contains the maximum number of steps
#### The interval that contains the maximum number of steps is interval 835
```{r}
max(meanStepsPerInterval$steps)
```

##4. Imputing missing values
###4a. The total number of missing values in dataset are: 2304 Intervals/Rows are missing values, which is 13.1% of dataset
```{r warning = FALSE, message = FALSE}
library(imputeTS)
```

```{r}
statsNA(Activity$steps, bins = 61, printOnly = TRUE)
```
####4b. Fill NA's with Mean of the entire day, along with Code to describe and show a strategy for imputing missing data
```{r warning = FALSE, message = FALSE}
library(reshape2)
```

```{r warning = FALSE}
ActivityLong <- dcast(Activity, interval ~ date, value.var = "steps") #dcast from long to wide
ActivityLongNA <- na_mean(ActivityLong, option = "mean") #replace NA with mean for the day
ActivityLongNA[is.na(ActivityLongNA)] <- 0 #replace remaining NA with 0, no steps that day
View(ActivityLongNA)
```
####4c. Create a new Dataset with the missing values filled in 
```{r}
ActivityWide <- melt(ActivityLongNA, id.vars = c("interval")) #transform back to long data set, next two lines change column name
colnames(ActivityWide)[colnames(ActivityWide)=="variable"] <- "date"
colnames(ActivityWide)[colnames(ActivityWide)=="value"] <- "steps"
View(ActivityWide)
```
####4d. Create a Histogram of the total number of steps taken each day after missing values are imputed
```{r totalstepsnona}
TotalStepsAll <- aggregate(steps ~ date, ActivityWide, sum)
TotalStepsAll 
TotalStepsAll$date <- as.Date(TotalStepsAll$date,"%Y-%m-%d")
hist(TotalStepsAll$steps, breaks=30, col="blue", xlab="Steps", main="Total number of Steps Taken Each Day With Missing Imputation")
```

####4da. What is the mean total number of steps per day, with no missing data: 9354.23 steps
```{r eval = FALSE}
meanStepsPerDayAll <- aggregate(steps ~ date, ActivityWide, mean, na.action = na.omit)
meanStepsPerDayAll
meanStepsTotalAll <- mean(TotalStepsAll$steps)
meanStepsTotalAll    
```

####4db. What is the median total number of steps per day, with no missing data: 10395 steps
```{r eval = FALSE}
medianStepsPerDayAll <- aggregate(steps ~ date, ActivityWide, median, na.action = na.omit)
medianStepsPerDayAll
medianStepsTotalAll <- median(TotalStepsAll$steps)
medianStepsTotalAll   
```




###The mean and medium total number of steps taken per day is lower with the missing values imputed, than when omitted. 

##5. Are there differences in activity patterns between weekdays and weekends?
###5a. New Factor variable in dataset with two levels- weekday or weekend
```{r}
ActivityWide2 <- ActivityWide
ActivityWide2$date <- as.Date(ActivityWide$date,"%Y-%m-%d")
ActivityWide2$date <- weekdays(as.Date(ActivityWide$date))
ActivityWide2$WeekDayType <- ifelse(ActivityWide2$date == "Saturday" | ActivityWide2$date == 
                                 "Sunday", "Weekend", "Weekday")
View(ActivityWide2)
```
###5b. Panel Plot of 5- minute interval and average number of steps taken, averaged across all weekend or weekday days
```{r include = FALSE, warning = FALSE, message = FALSE}
library(dplyr)
```
```{r}
ActivityWeekend <- subset(ActivityWide2, WeekDayType == "Weekend", select = c("interval", "date", "steps", "WeekDayType"))
ActivityWeekendMean <- ActivityWeekend %>% group_by(interval) %>% mutate(steps.new = mean(steps))
ActivityWeekday <- subset(ActivityWide2, WeekDayType == "Weekday", select = c("interval", "date", "steps", "WeekDayType"))
ActivityWeekdayMean <- ActivityWeekday %>% group_by(interval)  %>% mutate(steps.new = mean(steps))
ActivityAll <- rbind(ActivityWeekendMean, ActivityWeekdayMean)
```

```{r intervalbysteps5min}
library(lattice)
xyplot(steps.new~interval | WeekDayType, ActivityAll, type = "l", xlab = "Interval", ylab = "Steps", layout=c(1,2))
```




