# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
The data for this assignment is contained in a zipped file on the course Github repository.  It can be accessed from there on the fly at the time of analysis.  However, to ensure that the data is available for analysis even in the absence of an internet connection, this implementation has downloaded the file to be accessed locally at any time.  The structure of the loaded dataframe is shown below.

```r
file <- "C:/Users/David/SkyDrive/Private/David/Professional/Technical/Data Science/Coursera JHU DS Certificate/5 Reproducible/PeerAss1/activity.csv"
adata <- read.csv(file)
str(adata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is the total number of steps taken per day?
The following histogram displays the total number of steps taken per day, ignoring missing values.

```r
adata.date <- aggregate(adata[1],by=adata[2],FUN=sum,na.rm=TRUE)
hist(adata.date$steps,
     breaks=20,
     col = "blue",
     main = "Histogram of Total Number of Steps Taken per Day",
     xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
mean(adata.date$steps)   # mean
```

```
## [1] 9354
```

```r
median(adata.date$steps) # median
```

```
## [1] 10395
```

**Mean:** Ignoring missing values, the mean number of steps taken per day is 9,354.

**Median:** Ignoring missing values, the median number of steps taken per day is 10,395.

## What is the average daily activity pattern?
Following is a time-series plot of the average number of steps taken during each 5-minute interval during the day.

```r
# The following data set contains the sample data aggregated by interval.
adata.interval <- aggregate(adata[1],by=adata[3],FUN=mean,na.rm=TRUE)
plot(x=adata.interval$interval,
     y=adata.interval$steps,
     type="l",
     main="Average Steps Per 5-Minute Interval",
     xlab="Interval",
     ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

**Maximum Steps**


```r
max_interval <- adata.interval[adata.interval$steps==max(adata.interval$steps),]

max_interval[1] # maximum interval
```

```
##     interval
## 104      835
```

```r
round(max_interval[2],1) # maximum steps per interval
```

```
##     steps
## 104 206.2
```

The maximum average steps per period occur at interval 835, with an average of 206.2 steps per 5 minutes (calculated inline).

## Imputing missing values
**Item 1 - Number of Missing Values**

```r
# total records
nrow(adata)
```

```
## [1] 17568
```

```r
# missing records
sum(is.na(adata$steps))
```

```
## [1] 2304
```
The total number of records in the dataset is 17,568.  Of these, 2,304 contain missing values for number of steps taken.

**Items 2 and 3 - Replacing Missing Values and Creating a New Dataset**

Missing values for steps per interval are replaced by the mean number of steps for that interval, calculated on the non-missing rows.  A new dataset is created that contains these imputed values.

```r
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
adata.impute <- ddply(adata, ~interval, transform, steps = impute.mean(steps))
```

**Item 4 - Histogram, Mean and Median**

The following is a histogram of the total number of steps per day using imputed values for missing values.

```r
# The following data set contains the sample data aggregated by date.
adata.impute.date <- aggregate(adata.impute[1],by=adata.impute[2],FUN=sum,na.rm=TRUE)
hist(adata.impute.date$steps,
     breaks=20,
     col = "red",
     main = "Histogram of Total Number of Steps Taken per Day (Imputed Data)",
     xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 

```r
mean(adata.impute.date$steps)   # mean
```

```
## [1] 10766
```

```r
median(adata.impute.date$steps) # median
```

```
## [1] 10766
```

**Mean:** Ignoring missing values, the mean number of steps taken per day, rounded to the nearest step, is 10,766.

**Median:** Ignoring missing values, the median number of steps taken per day, rounded to the nearest step, is 10,766.

This means of value imputation has increased both the mean and the median values from those calculated in the first part of the assignment.  Furthermore, given the number of missing values replaced, the median values contain the mean values imputed to them.  This approach may overweigh the central tendency and underweigh the upper and lower extremes, and another system of imputation may result more reliable results. 



## Are there differences in activity patterns between weekdays and weekends?
**Item 1 - New Factor Variable**

A new factor variable, Day, is created, indicating whether a record pertains to a weekday or to a weekend.

```r
adata.impute$dateP <- as.POSIXlt(adata.impute$date,format="%Y-%m-%d")
adata.impute$day <- "Weekday"
adata.impute$day [weekdays(adata.impute$dateP) %in% c("Saturday","Sunday")] <- "Weekend"
```

**Item 2 - Weekday/Weekend Comparison**

The following time series plots display the weekday and weekend data separately.  


```r
adata.impute.interval <- aggregate(adata.impute[1],
                                   by=adata.impute[c(3,5)],
                                   FUN=mean,
                                   na.rm=TRUE)
library(ggplot2)
plot <- ggplot(data = adata.impute.interval,
               aes(x=interval,y=steps))
plot + geom_line() + facet_wrap(~day,nrow=2)
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 
