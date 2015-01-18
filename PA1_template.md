# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
To read data file we need unzip the archive and then read data with 'read.csv()' function. After reading we will convert the string 'date' variable into Date type:

```r
## Clean the working environment
rm(list = ls())
graphics.off()

## Unzip the archive and read data 
zip_file <- "activity.zip"
file <- "activity.csv"

if(!file.exists(file)) unzip(zip_file)

data <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?
In this section we will summarize the total number of steps taken per each day and create new data frame 'total_steps' with summary values. To do this we will use the 'aggregate()' function. Then we will make a histogram of the total number of steps taken each day and calculate the **mean** and **median** total number of steps taken per day.

```r
total_steps <- aggregate(steps ~ date, data = data, sum)

hist(total_steps$steps, main = "Total Number of Steps Taken per Day", xlab = "Daily Total Steps", breaks = 10)
```

![](figure/meantotalnumber-1.png) 

```r
total_mean <- mean(total_steps$steps)
total_median <- median(total_steps$steps)
```
The **mean** of the total number of steps taken per day is 10766.19.   The **median** of the total number of steps taken per day is 10765.00.


## What is the average daily activity pattern?
To make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis), we will calculate the mean values of steps by each interval across all days.

```r
av_by_interval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)

plot(av_by_interval$interval, av_by_interval$steps, type = "l", main = "Average Number of Steps Taken per 5-minute Interval", xlab = "5-minute Intervals", ylab = "Average Number of Steps")
```

![](figure/timeplot-1.png) 

To find the 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps we will use next filter expression.

```r
max_interval <- av_by_interval[which.max(av_by_interval$steps), ]
```
The 5-minute interval with the maximum number of steps is interval 835, with 206.17 steps.


## Imputing missing values
To calculate the total number of missing values in the dataset (i.e. the total number of rows with `NA` values) we can use the next code:

```r
missing_values <- nrow(data) - sum(complete.cases(data))
```
We can see that the number of missing values is 2304. 

We will fill the missing values in the data set with mean values of the 5-minutes intervals, across of all days.

```r
# Create a new data set
imputed_data <- data

# Fill the missing values with mean of 5-minutes intervals
for(i in 1:nrow(imputed_data)) {
        if(is.na(imputed_data$steps[i])) {
                imputed_data$steps[i] <- av_by_interval$steps[(av_by_interval$interval == imputed_data$interval[i])]
        } 
}
```
We can check imputed data set on existing of the missing values:

```r
nrow(imputed_data) - sum(complete.cases(imputed_data))
```

```
## [1] 0
```
So, now we can make a histogram of the total number of steps taken each day and calculate the **mean** and **median** total number of steps taken per day with imputed data.

```r
total_steps_imputed <- aggregate(steps ~ date, data = imputed_data, sum)

hist(total_steps_imputed$steps, main = "Total Number of Steps Taken per Day", xlab = "Daily Total Steps", breaks = 10)
```

![](figure/histimputeddata-1.png) 

```r
total_mean_imputed <- mean(total_steps_imputed$steps)
total_median_imputed <- median(total_steps_imputed$steps)
```
The **mean** of the total number of steps taken per day is 10766.19. The **median** of the total number of steps taken per day is 10766.19. We can see that the **mean** and **median** values of the imputed data set are equal and they are different for raw data set.   

## Are there differences in activity patterns between weekdays and weekends?
To create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day we will use next code:

```r
imputed_data$day <- weekdays(imputed_data$date, abbreviate = TRUE)
imputed_data$daytype <- ifelse(imputed_data$day == "Sat" | imputed_data$day == "Sun", "weekend", "weekday")
```
Then we can make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) with `lattice` library.

```r
library(lattice)

av_by_daytype <- aggregate(steps ~ interval + daytype, data = imputed_data, mean)
xyplot(steps ~ interval | daytype, data = av_by_daytype, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of Steps")
```

![](figure/paneltimeplot-1.png) 

So, we can see there are some differents in the weekday and weekend activities. For example, the average number of steps in the morning (500 - 1000 interval range) is higher in the weekday but the average number of steps during the day time (1000 - 1800 interval range) is higher in the weekend. It seems this is a pattern of typical behavior of people working in an office. 
