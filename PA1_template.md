Reproducible Research - Peer Assessment 1
========================================================

## 1. Loading and processing the data:

First, dataset is loaded after setting the working directory using the read.csv function. 

```r

data.set = read.csv("activity.csv", header = TRUE)
```


Right after the date is converted to the date format using the as.Date() function.

```r

data.set$date = as.Date(data.set$date, "%Y-%m-%d")
```

## 2. Calculation of mean, median and total steps taken  per day and making a histogram of total steps

The total, mean and median of the steps taken per day are calculated and stored in the data frame named new.data.

```r
tempfunc = function(x) c(total = sum(x), mean = mean(x), median = median(x))
new.data = aggregate(steps ~ date, data.set, tempfunc)
```


Using this data frame, the histogram for total steps taken per day is made.

```r
hist(new.data$steps[, 1], main = "Histogram of number of steps per day", xlab = "Number of steps per day")
```

![plot of chunk hist](figure/hist.png) 

## 3. Average daily pattern

The mean steps taken in all days for each interval is calculated and stored in the data frame named new.data2.

```r
tempfunc2 = function(x) c(mean = mean(x))
new.data2 = aggregate(steps ~ interval, data.set, tempfunc2)
```

The required time series plot is made using the new.data2 data frame.

```r
plot(new.data2$interval, new.data2$steps, type = "l", xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk plot](figure/plot.png) 

The interval that has the maximum value of the mean steps taken is calculated and stored in the variable named max.interval.

```r
max.interval = new.data2[which(new.data2$steps == max(new.data2$steps)), 1]
```

**The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 835**.

## 4. Inputting missing values

The total number of missing values in the given dataset is found and is stored in the variable named nona.

```r
nona = sum(is.na(data.set$steps))
```

**The number of missing values in the dataset is 2304**.

The mssing values are replaced with the mean of that particular interval and this whole data is stored in a new data frame named nonadata.
  This is first done by copying the original data frame to nona and plugging the missing values with     the required mean using the for loop.

```r
nonadata = data.set
i = 1
for (i in 1:nrow(nonadata)) {
    if (is.na(nonadata[i, 1]) == TRUE) {
        nonadata[i, 1] = new.data2[which(new.data2$interval == nonadata[i, "interval"]), 
            2]
    }
}
```

For this new data frame that has no missing values, the total, mean and the median of the steps taken per day is calculated. This is stored in the data frame named mm.nonadata.

```r
mm.nonadata = aggregate(steps ~ date, nonadata, tempfunc)
```

Using this data frame, the histogram of the total steps taken per day is made.

```r
hist(mm.nonadata$steps[, 1], main = "Histogram of number of steps per day", 
    xlab = "Number of steps per day")
```

![plot of chunk hist2](figure/hist2.png) 

## 5. Activity patterns on weekdays and weekends

A new column is created to the data frame containing no missing values named weekend which will have "weekend" if the date corresponds to a weekend and "wwekday" if the date corresponds to a weekday. This is done using the weekdays() function and a for loop.

```r
i = 1
for (i in 1:nrow(nonadata)) {
    if (weekdays(nonadata[i, "date"]) == "Saturday" || weekdays(nonadata[i, 
        "date"]) == "Sunday") {
        nonadata[i, 4] = "Weekend"
    } else nonadata[i, 4] = "weekday"
}
colnames(nonadata) = c("steps", "date", "interval", "weekend")
```

This column is then converted into a factor variable.

```r
nonadata[, 4] = as.factor(nonadata[, 4])
```

Finally, a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) is made.

```r
library(ggplot2)
qplot(interval, steps, data = nonadata, facets = weekend ~ ., geom = "line", 
    ylab = "Number of steps", xlab = "Interval")
```

![plot of chunk plot2](figure/plot2.png) 

It is seen that the activity pattern on weekdays and weekends show some slight difference.

