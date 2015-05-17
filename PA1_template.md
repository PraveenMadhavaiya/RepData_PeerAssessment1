# Reproducible Research - Peer Assessment 1
## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
activity<-read.csv("activity.csv", colClasses = c("numeric","character","numeric"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activ_data<-na.omit(activity)
activ_steps <- aggregate(steps ~ date, activ_data, sum)
hist(activ_steps$steps, col="orange", main="Histogram of total number of steps per day", xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/Activ Step Histogram-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day.

The mean number of steps per day is 

```r
mean(activ_steps$steps)
```

```
## [1] 10766.19
```

The median number of steps per day is 

```r
median(activ_steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
tseries <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
plot(row.names(tseries), tseries, type = "l", xlab = "5-min interval", ylab = "Average across all Days", main = "Average number of steps taken", col = "blue")
```

![](PA1_template_files/figure-html/Average Daily Pattern-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval <- which.max(tseries)
names(max_interval)
```

```
## [1] "835"
```

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
activity_missing <- sum(is.na(activity))
activity_missing
```

```
## [1] 2304
```


2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy is - 


```r
avgsteps <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNAs <- numeric()
for (i in 1:nrow(activity))
  {
  obs <- activity[i,]
  if (is.na(obs$steps)) 
    {
    steps <- subset(avgsteps, interval == obs$interval)$steps
    }
  else
    {
      steps <- obs$steps
    }
  fillNAs <- c(fillNAs, steps)
  }
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newactivity <- activity
newactivity$steps <- fillNAs
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepstotal <- aggregate(steps ~ date, data = newactivity, sum, na.rm=TRUE)
hist(stepstotal$steps, main = "Total Steps by day", xlab="day", col="red")
```

![](PA1_template_files/figure-html/New dataset histogram-1.png) 


```r
mean(stepstotal$steps)
```

```
## [1] 10766.19
```


```r
median(stepstotal$steps)
```

```
## [1] 10766.19
```

After imputing NA values the mean is the same but the median is a little bit different.

## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels -'weekday' and 'weekend' indicating whether a given date is a weekday or weekend day.


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
day<-weekdays(activity$date)
daylevel<-vector()
for (i in 1:nrow(activity))
  {
  if (day[i] == "Saturday" || day[i] == "Sunday")
    {
    daylevel[i]<-"Weekend"
    }
  else 
    {
      daylevel[i]<-"Weekday"
    }
  }
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)
stepsperday <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsperday) <- c("interval", "daylevel", "steps")
```

2.Make a panel plot containing a time series plot (i.e. type =???l???) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
xyplot(steps ~ interval | daylevel, stepsperday, type = "l", layout = c(1,2), xlab="Interval", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/Weekend Weekday difference-1.png) 
































