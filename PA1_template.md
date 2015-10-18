# Reproducible Research: Peer Assessment 1



## Preparation stages
This assigment uses the data.table, sqldf and ggplot2 packages

```r
library(data.table)
library(sqldf)
library(ggplot2)
```

## Loading and preprocessing the data

```r
activity_data <- read.csv("activity.csv")
activity_data <- as.data.table(activity_data)
activity_data
```

```
##        steps       date interval
##     1:    NA 2012-10-01        0
##     2:    NA 2012-10-01        5
##     3:    NA 2012-10-01       10
##     4:    NA 2012-10-01       15
##     5:    NA 2012-10-01       20
##    ---                          
## 17564:    NA 2012-11-30     2335
## 17565:    NA 2012-11-30     2340
## 17566:    NA 2012-11-30     2345
## 17567:    NA 2012-11-30     2350
## 17568:    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?
First sumarize data by date.  Missing values will be ignored later at the mean & median call.

```r
by_date <- activity_data[, .(steps = sum(steps)), by=date]
head(by_date, 5)
```

```
##          date steps
## 1: 2012-10-01    NA
## 2: 2012-10-02   126
## 3: 2012-10-03 11352
## 4: 2012-10-04 12116
## 5: 2012-10-05 13294
```

Here is a histogram of steps by date. 

```r
hist(by_date$steps,
     main="Steps by date Analysis with NAs",
     xlab="Steps",
     ylab="Frequency")
```

![](PA1_template_files/figure-html/hist1-1.png) 

This shows the distribution frequency of total steps for the 61 days in the dataset.  Most days are between 10000 and 15000 steps.

Below are the mean and median for the dataset without considering the missing values. format(md1, 10)`.

```r
mean(by_date$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(by_date$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Summarize the data by interval

```r
by_interval <- activity_data[!is.na(steps), .(steps = mean(steps)), by = interval]
by_interval
```

```
##      interval     steps
##   1:        0 1.7169811
##   2:        5 0.3396226
##   3:       10 0.1320755
##   4:       15 0.1509434
##   5:       20 0.0754717
##  ---                   
## 284:     2335 4.6981132
## 285:     2340 3.3018868
## 286:     2345 0.6415094
## 287:     2350 0.2264151
## 288:     2355 1.0754717
```

Here is a plot of the intervals accross all days.


```r
plot(by_interval$interval, by_interval$steps,
     type="l",
     xlab="Interval",
     ylab="Average steps across all days", main="Average steps by interval across days")
```

![](PA1_template_files/figure-html/intplot1-1.png) 

The following finds the interval with the max steps because it is difficult to see from the line. 
However this is verified by the graph

```r
max_interval <- by_interval[steps == max(by_interval$steps),]
max_interval
```

```
##    interval    steps
## 1:      835 206.1698
```

The inerval with the maximum steps is interval 835 with 206.1698113 steps.


## Imputing missing values

### Counting missing values

```r
sum(complete.cases(activity_data))
```

```
## [1] 15264
```

```r
sum(!is.na(activity_data$steps))
```

```
## [1] 15264
```

In both cases the number is the same which means that the only observation with missing values is "steps"

### Impute the values
The selected methodology to impute the missing values is to take the average across all days for the same interval.  
This makes more sense to me because it assumes that the activity is about the same depending on the time of the day.
To achieve this, I use the already calculated average by interval and use sqldf to join the data.  Note
the case statement which says that if the value is missing from my main dataset I take the value from the avergage.


```r
imputed <- sqldf("select case when a.steps isnull then i.steps else a.steps end as steps, a.date, a.interval
                 from activity_data a inner join by_interval i on a.interval = i.interval")
```

```
## Loading required package: tcltk
```

```r
## convert to data.table
imputed <- as.data.table(imputed)
imputed
```

```
##            steps       date interval
##     1: 1.7169811 2012-10-01        0
##     2: 0.3396226 2012-10-01        5
##     3: 0.1320755 2012-10-01       10
##     4: 0.1509434 2012-10-01       15
##     5: 0.0754717 2012-10-01       20
##    ---                              
## 17564: 4.6981132 2012-11-30     2335
## 17565: 3.3018868 2012-11-30     2340
## 17566: 0.6415094 2012-11-30     2345
## 17567: 0.2264151 2012-11-30     2350
## 17568: 1.0754717 2012-11-30     2355
```

The next step is to summarize the new data by date

```r
## sumarize by date on the new data set
by_date_i <- imputed[, .(steps = sum(steps)), by=date]
head(by_date_i, 5)
```

```
##          date    steps
## 1: 2012-10-01 10766.19
## 2: 2012-10-02   126.00
## 3: 2012-10-03 11352.00
## 4: 2012-10-04 12116.00
## 5: 2012-10-05 13294.00
```

Here is a histogram of the new data with the imputed values.


```r
hist(by_date_i$steps,
     main="Steps by date Analysis with imputed values",
     xlab="Steps",
     ylab="Frequency")
```

![](PA1_template_files/figure-html/hist2-1.png) 

Calculate the mean and median of the new data.

```r
mean(by_date_i$steps)
```

```
## [1] 10766.19
```

```r
median(by_date_i$steps)
```

```
## [1] 10766.19
```

### Effect of imputing missing values
Apparently because when a day has a missing value, **all** the values of that day are missing, 
the method chosen created exact copies of them.  They all become the *average day* which 
which increased the peak of the histogram while the rest remained the same.


## Are there differences in activity patterns between weekdays and weekends?

Here I create a new factor variable with values weekend and weekday.  I use a combination 
of the weekdays and ifelse function


```r
## create a two level factor with weekdays/weekends
imputed[, dow := factor(ifelse(weekdays(as.Date(as.character(date),format="%Y-%m-%d")) %in% c("Saturday", "Sunday"), "weekend", "weekday")) ]
```

```
##            steps       date interval     dow
##     1: 1.7169811 2012-10-01        0 weekday
##     2: 0.3396226 2012-10-01        5 weekday
##     3: 0.1320755 2012-10-01       10 weekday
##     4: 0.1509434 2012-10-01       15 weekday
##     5: 0.0754717 2012-10-01       20 weekday
##    ---                                      
## 17564: 4.6981132 2012-11-30     2335 weekday
## 17565: 3.3018868 2012-11-30     2340 weekday
## 17566: 0.6415094 2012-11-30     2345 weekday
## 17567: 0.2264151 2012-11-30     2350 weekday
## 17568: 1.0754717 2012-11-30     2355 weekday
```

Summarize the new data by taking the average steps by interval, dow.


```r
week_analysis <- imputed[, .(steps=mean(steps)), by=.(interval, dow)]
week_analysis
```

```
##      interval     dow       steps
##   1:        0 weekday  2.25115304
##   2:        5 weekday  0.44528302
##   3:       10 weekday  0.17316562
##   4:       15 weekday  0.19790356
##   5:       20 weekday  0.09895178
##  ---                             
## 572:     2335 weekend 11.58726415
## 573:     2340 weekend  6.28773585
## 574:     2345 weekend  1.70518868
## 575:     2350 weekend  0.02830189
## 576:     2355 weekend  0.13443396
```

Plot the data for comparison.  It is easier to see the results if the panel is 2 rows, 1 column.


```r
ggplot(data = week_analysis, aes(x=interval, y=steps)) + geom_point() + geom_line() +
    facet_grid(dow ~ .) +
    ggtitle("Average steps per interval - Weekend Vs. weekdays") +  
    xlab("Interval") + ylab("Steps") +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
```

![](PA1_template_files/figure-html/wendplot-1.png) 

### Results analysis
There is a shift towards later in the day on weekends compared to weekdays.  In general or weekends the steps are more evenly spread whereas on weekdays there is a peak around the 800th interval.
