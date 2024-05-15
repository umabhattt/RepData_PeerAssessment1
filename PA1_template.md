---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
fullData <- read.csv('activity.csv')
fullData$date <- as.Date(fullData$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
# 1. Calculate the total number of steps taken per day
totalStepsPerDay <- aggregate(steps ~ date, data = fullData, FUN = sum)

# 2. Make a histogram of the total number of steps taken each day
hist(totalStepsPerDay$steps, main = "Histogram of Total Steps Per Day",
     xlab = "Total Steps",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")
```

![unnamed-chunk-3-1.png](/unnamed-chunk-3-1.png)<!-- -->

```r
#3. Calculate and report the mean and median of the total number of steps taken per day
meanTotalSteps <- mean(totalStepsPerDay$steps)
medianTotalSteps <- median(totalStepsPerDay$steps)

meanTotalSteps
```

```
## [1] 10766.19
```

```r
medianTotalSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

meanStepsPerInterval <- aggregate(steps ~ interval, data = fullData, FUN = mean)

plot(meanStepsPerInterval$interval, meanStepsPerInterval$steps, 
     type = "l", 
     xlab = "Interval", ylab = "Steps",
     main = "Average Steps Taken per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxAverageSteps <- max(meanStepsPerInterval$steps, na.rm = TRUE)

maxAverageSteps
```

```
## [1] 206.1698
```
## Imputing missing values


```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs

sum(is.na(fullData))
```

```
## [1] 2304
```

```r
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

naSteps <- fullData[is.na(fullData$steps),]

naStepsFilled <- merge(naSteps, meanStepsPerInterval, by='interval')

naStepsFilled$steps.x <- NULL


#Create a new dataset that is equal to the original dataset but with the missing data filled in.

findMissingValue <- function(x, meanStepsPerInterval){
  missing_indices <- which(is.na(x$steps))  # Find indices of missing values
  
  # Replace missing values with corresponding mean values based on 'interval'
  x$steps[missing_indices] <- meanStepsPerInterval$steps[
    match(x$interval[missing_indices], meanStepsPerInterval$interval)]
  
  return(x)
}

filledData <- findMissingValue(fullData, meanStepsPerInterval)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

newTotalStepsPerDay <- aggregate(steps ~ date, data = filledData, FUN = sum)

hist(newTotalStepsPerDay$steps, main = "New Histogram of Total Steps Per Day",
     xlab = "Total Steps",
     ylab = "Frequency",
     col = "green",
     border = "black")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
newMeanTotalSteps <- mean(newTotalStepsPerDay$steps)
newMedianTotalSteps <- median(newTotalStepsPerDay$steps)

newMeanTotalSteps
```

```
## [1] 10766.19
```

```r
newMedianTotalSteps
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

library(ggplot2)


filledData$nameOfDay <- weekdays(filledData$date)

filledData$dayType <- ifelse(filledData$nameOfDay %in% c("Saturday", "Sunday"), "weekend", "weekday")

filledData$dayType <- factor(filledData$dayType, levels = c("weekday", "weekend"))


#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

newMeanStepsPerInterval <- aggregate(steps ~ interval + dayType, data = filledData, FUN = mean)


p <- ggplot(newMeanStepsPerInterval, aes(x = interval, y = steps)) +
  geom_line() +  
  facet_wrap(~ dayType, scales = "free_y") +  
  labs(x = "Interval", y = "Average Steps Taken", title = "Five-minute Interval vs. Mean Steps Taken") +
  theme_minimal()  

p
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
