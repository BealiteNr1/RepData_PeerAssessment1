Set working directory and provide necessary packages?!

```r
#setwd("E:/Data/Coursera/Data Specialization/Reproducible_Research/PeerAssessment1")

library(lattice)
```




##Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())


```r
f <- read.csv("activity.csv", sep=",", header = TRUE, na.strings = "NA")
names(f)
```

```
## [1] "steps"    "date"     "interval"
```

2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
f$date <- as.Date(f$date)
```

##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.


1.Calculate the total number of steps taken per day

```r
fOK <- f[!is.na(f$steps),]
fStepsPerDay <- aggregate(steps ~ date, data=fOK, sum)
```

2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
#png(filename="plots/hist1.png")
hist(fStepsPerDay$steps, breaks = 20)
```

![plot of chunk unnamed-chunk-5](plots/unnamed-chunk-5-1.png) 

```r
#dev.off()
```

3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(fStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(fStepsPerDay$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
fStepsPerInterval <- aggregate(steps ~ interval, data=fOK, mean)
plot(fStepsPerInterval$interval, fStepsPerInterval$steps, type="l")
```

![plot of chunk unnamed-chunk-7](plots/unnamed-chunk-7-1.png) 

```r
#dev.copy(png,'plots/interval_avg_ts1.png')
#dev.off()
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
fStepsPerInterval[which.max(fStepsPerInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(f)-sum(complete.cases(f))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Well, strategy I choose is: replace NA-values with the mean value of the relevant interval... and take the lazy version by rounding down.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fCorr <- f
for (iRow in 1:nrow(fCorr)) {
    if (is.na(fCorr$steps[iRow])) {
	  fCorr$steps[iRow] <- floor(fStepsPerInterval$steps[fStepsPerInterval$interval == fCorr$interval[iRow]])
    }

}
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
fCorrStepsPerDay <- aggregate(steps ~ date, data=fCorr, sum)
hist(fCorrStepsPerDay$steps, breaks = 20)
```

![plot of chunk unnamed-chunk-11](plots/unnamed-chunk-11-1.png) 

```r
#dev.copy(png,'plots/hist2.png')
#dev.off()
```

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
f$dayOfWeek <- weekdays(f$date)
week <- data.frame(c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"), c("weekday","weekday","weekday","weekday","weekday","weekend","weekend"))
colnames(week) <- c("dayOfWeek","typeOfWeekday")
f <- merge(f, week, by = "dayOfWeek")
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
fStepsPerInterval <- aggregate(steps ~ interval + typeOfWeekday, data=f, mean)
xyplot(steps ~ interval| fStepsPerInterval $typeOfWeekday, 
           data = fStepsPerInterval ,
           type = "l",
           xlab = "Interval",
           ylab = "Number of steps",
           layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](plots/unnamed-chunk-13-1.png) 

```r
#dev.copy(png,'plots/interval_avg_ts2.png')
#dev.off()
```
