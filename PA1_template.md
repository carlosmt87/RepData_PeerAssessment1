# Reproducible Research: Peer Assessment 1
==========================================
Author: Carlos Trejo

## Loading and preprocessing the data

```r
library(plyr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
data <- read.csv("activity.csv")
#Transform the column into a Date type
data[,'date'] <- as.Date(data[,'date'],"%Y-%m-%d")
```
## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
totalDailySteps <- aggregate(steps ~ date, data = data, FUN = sum)
barplot(totalDailySteps$steps, names.arg = totalDailySteps$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2. Calculate and report the **mean** and **median** total number of
   steps taken per day

```r
mean(totalDailySteps$steps)
```

```
## [1] 10766
```

```r
median(totalDailySteps$steps)
```

```
## [1] 10765
```
   


## What is the average daily activity pattern?


1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
totalIntervalSteps <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(totalIntervalSteps, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
totalIntervalSteps$interval[which.max(totalIntervalSteps$steps)]
```

```
## [1] 835
```




## Imputing missing values
1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

Since it is previously calculated, I will use the mean of the 5-minute interval 


3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
tidyData <- merge(data, totalIntervalSteps, by = "interval",suffixes = c("",".y") )

#Replace null values with the average of the interval
for(i in 1:nrow(tidyData)) {
  if(is.na(tidyData[i, 'steps']))
     tidyData[i, 'steps'] <- tidyData[i, 'steps.y']
}

#Delete the extra column
tidyData <- tidyData[,-4]
```


4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
#Summarize the total steps taken each day with the tidy data set
totalDailySteps <- aggregate(steps ~ date, data = tidyData, FUN = sum)
barplot(totalDailySteps$steps, names.arg = totalDailySteps$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
#Calculate and report the mean and median total number of steps 
#with the new data set
mean(totalDailySteps$steps)
```

```
## [1] 10766
```

```r
median(totalDailySteps$steps)
```

```
## [1] 10766
```

As we may see, there is no significant change after replacing the NAs with 
the mean of the interval. 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
#Classify the days
classifyDay <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
tidyData$dayType <- as.factor(sapply(tidyData$date, classifyDay))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
#Average Daily Pattern with the new data set, grouping by interval and day type 
dailyPattern = ddply(tidyData, c("interval", "dayType"), summarise, meanSteps = mean(steps))
#Make a plot
ggplot(dailyPattern, aes(x = interval, y = meanSteps))+ geom_line() + facet_wrap( ~ dayType, ncol = 1 )
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
As we may see, the data has the same pattern on weekdays and weekends, however, we may see that the maximum mean steps is smaller on weekends 
