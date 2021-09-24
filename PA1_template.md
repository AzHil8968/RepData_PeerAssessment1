---
title: "Reproducible Research: Peer Assessment 1"
author: "Hilda"
date: "23/09/2021"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data

Show any code that is needed to

1.Load the data (i.e. read.csv())


```r
Data <- read.csv("activity.csv")
```
2.Process/transform the data (if necessary) into a format suitable for your analysis


```r
#Change date format
Data$date <- as.Date(Data$date, format = "%Y-%m-%d")
```
## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day.


```r
library(tidyverse) 
```


```r
steps_per_day <- Data %>%
  group_by(date) %>%
  summarize(steps_per_day = sum(steps))
```
2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
with(steps_per_day,hist(steps_per_day,main = "Histogram of the Total Number of Steps Taken Each Day", xlab="Steps per Day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day


```r
#MEAN: 10766.19
mean_steps_per_day <- mean(steps_per_day$steps_per_day,na.rm = TRUE)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
#Median: 10765
med_steps_per_day <- median(steps_per_day$steps_per_day,na.rm = TRUE)
med_steps_per_day
```

```
## [1] 10765
```
## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#Remove all NAs as input of steps
Data_NoNa <- subset(Data, !is.na(Data$steps))
Data_NoNa$interval <- as.factor(Data_NoNa$interval)

steps_per_interval <- Data_NoNa %>%
  group_by(interval) %>%
  summarize(SPI = sum(steps))

steps_per_interval$interval <- as.character(steps_per_interval$interval)

with(steps_per_interval,plot(interval,SPI,type = "l",
                             main = "Average Daily Activity Pattern",
                             xlab = "5-minute Interval",
                             ylab = "Average Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#MAX: 835
Max_5minute_interval <- steps_per_interval[steps_per_interval$SPI == max(steps_per_interval$SPI),"interval"]
Max_5minute_interval
```

```
## # A tibble: 1 x 1
##   interval
##   <chr>   
## 1 835
```
## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
#total number of rows with NAs: 2304
Missing_Values <- sum(apply(Data, 1, anyNA))
Missing_Values
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#Filled_Data is the new dataset that is equal to the original dataset but with the missing data filled in.
Filled_Data <- Data %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Filled_steps_per_day <- Filled_Data %>%
  group_by(date) %>%
  summarize(steps_per_day = sum(steps))

par(mfrow=c(1,2))

with(steps_per_day,hist(steps_per_day,main = "", xlab="Steps per Day (with NAs)"))

with(Filled_steps_per_day,hist(steps_per_day,main = "", xlab="Steps per Day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#MEAN: 10766.19
mean_steps_per_day <- mean(Filled_steps_per_day$steps_per_day,na.rm = TRUE)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
#Median: 10766.19 (previously it was 10765!)
med_steps_per_day <- median(Filled_steps_per_day$steps_per_day,na.rm = TRUE)
med_steps_per_day
```

```
## [1] 10766.19
```
The is a difference in median of two data before and after filling the NAs. Also, the histogram of these two data is different comparing the frequency axis.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Filled_Data$day <- weekdays(Filled_Data$date)
weekdayOnly <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
Filled_Data$day <- factor((Filled_Data$day %in% weekdayOnly), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
Filled_Data$interval <- as.factor(Filled_Data$interval)

steps_per_interval <- Filled_Data %>%
  group_by(interval,day) %>%
  summarize(SPI = sum(steps))

steps_per_interval$interval <- as.numeric(as.character(steps_per_interval$interval))

library(ggplot2)
ggplot(steps_per_interval,aes(x=interval,y=SPI))+
  geom_line()+
  facet_grid(rows=vars(day)) +
  theme(strip.background = element_rect(colour="black", fill="#FEDFC1")) +
  labs(x = "Interval",y="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
