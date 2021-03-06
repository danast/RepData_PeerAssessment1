---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

In this section we read the data and do a bit of processing to simplify plotting.


```r
# Options for number formatting, load libraries
options(scipen=2, digits=2)
library(data.table)
library(ggplot2)
# Read the data, unz helps us read the zip-file directly
data <- read.csv(unz("activity.zip", "activity.csv"), colClasses=c("numeric", "Date", "integer"))
DT <- data.table(data)
# Create a time field (in hours) for plotting, since the interval is not suitable for this
# Since this updates in place, wrap in invisible() to suppress output
invisible(DT[, `:=`(ihour=as.numeric((interval%/%100)+(interval%%100)/60.0))])
# Support function for plotting time values (7.75 ->  07:45)
timeFormatter <- function(hours) {
    h <- hours%/%1
    m <- round(60*(hours%%1))
    sprintf("%02d:%02d", h, m)
}
```

## What is mean total number of steps taken per day?

```r
# Create a sum of steps by date
DS <- DT[!is.na(steps),.(stepsum=sum(steps)), by=date]
# Plot a histogram
hist(DS$stepsum, xlab="Number of steps", main="Histogram of total daily steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# Calculate mean and median
meansteps <- mean(DS[,stepsum])
mediansteps <- median(DS[,stepsum])
```

The mean number of steps per day is 10766.19 and the median 10765 steps.


## What is the average daily activity pattern?

```r
# Calculate averages for each interval
AD <- DT[, .(smean=mean(steps, na.rm=T)), .(interval,ihour)]

# plot
qplot(data=AD, x=ihour, y=smean, geom="line", xlab="Time of day",ylab="Mean number of steps") + scale_x_continuous(breaks=c(0,6,12,18,24), label=timeFormatter) + ggtitle("Average daily activity pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# Find the top interval
top_interval <- AD[smean==(AD[, max(smean)]), interval]
thour = top_interval %/% 100
tmin = top_interval %% 100
```

The interval with the highest number of steps on average is the interval 835. That is the interval starting at hour 8, minute 35.


## Imputing missing values

Impute missing values by using the mean for that interval.


```r
# How many missing values are there?
missingvals <- DT[is.na(steps), .("Missing values"=.N)]
missingvals
```

```
##    Missing values
## 1:           2304
```

```r
# Create a new dataset where we'll impute missing values
DTI <- DT
# Add the mean values for each interval to the table
invisible(DTI[,`:=`(imean=mean(steps, na.rm=T)), by=interval])
# Use the mean values for the interval to impute missing values
invisible(DTI[is.na(steps),`:=`(steps=imean)])
# Create a sum of steps by date
DSI <- DTI[,.(stepsum=sum(steps)), date]
hist(DSI$stepsum, xlab="Number of steps", main="Histogram of total daily steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
imeansteps <- mean(DSI[,stepsum])
imediansteps <- median(DSI[,stepsum])
```
There were 2304 missing values before imputing.  
After imputing, the mean number of steps per day is 10766.19 and the median 10766.19 steps. Since we use the mean to impute the missing values, it is unchanged, but the median has changed, the old median was 10765.


## Are there differences in activity patterns between weekdays and weekends?

To separate weekdays from weekends we add weekdays to the table, then apply a custom function to check if it is a weekday or weekend, storing the result as a factor.


```r
# Add weekday names to the table
invisible(DTI[,`:=`(wd=weekdays(strptime(date, format="%Y-%m-%d")))])
# Support function to map days to weekday/weekend
daytype <- function(dayname) {
    weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    if(dayname %in% weekdays) {
        return(factor("weekday", c("weekday", "weekend")))
    }
    factor("weekend", c("weekday", "weekend"))
}
# Apply the function above to the weekday data
invisible(DTI[,we:=sapply(wd, daytype)])
# Calculate means for each interval on weekdays/weekends
WS <- DTI[, .(smean=mean(steps, na.rm=T)), by=.(interval, ihour, we)]
# Plot the result
qplot(data=WS, x=ihour, y=smean, facets=we~., geom="line", xlab="Time of day",ylab="Mean number of steps") + scale_x_continuous(breaks=c(0,6,12,18,24), label=timeFormatter) + ggtitle("Activity patterns weekday/weekend")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

As can be seen in the plot, activity starts later on weekends but goes on for longer in the evening.
