# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(tidyr)
df<-read.csv("activity.csv")
sdf<-spread(df, date, steps)
```

## The total number of steps taken per day?
### Histogram of the total number of steps taken per day

```r
daily_total_steps <- as.data.frame(colSums(sdf[,-1]))[,1]
hist(daily_total_steps)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

### What is mean and median total number of steps taken per day?

```r
mean(daily_total_steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(daily_total_steps, na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
library(dplyr)
pattern <- df %>% group_by(interval) %>% summarise(mean(steps, na.rm=TRUE))
names(pattern) <- c("interval", "mean_steps")

pattern<-mutate(pattern, hour=(interval-interval%%100)/100 + (interval%%100)/60)

with(pattern, plot(hour, mean_steps, main = "Average Steps over Daily Intervals", xlab="interval by hour", type = "l"))
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?


```r
max_row <- which.max(pattern$mean_steps)

max_interval <- pattern$interval[max_row]
min1 <- max_interval %% 100
min2 <- min1 + 5
hr <- (max_interval - min1)/100

paste("max step interval:", hr,':', min1, '-', hr,':',min2 )
```

```
## [1] "max step interval: 8 : 35 - 8 : 40"
```

## Imputing missing values
### Number of missing value

```r
length(which(is.na(df$steps)))
```

```
## [1] 2304
```
### Fill the steps with missing values with average value of that interval

```r
df1<-left_join(df, pattern, by="interval")
df1<-mutate(df1, steps=ifelse(is.na(steps), mean_steps, steps))
df1<-df1[,c(1:3)]
```

### After imputing NA, the histogram of the total number of steps taken per day

```r
sdf1<-spread(df1, date, steps)
daily_total_steps1 <- as.data.frame(colSums(sdf1[,-1]))[,1]
hist(daily_total_steps1)
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

### After imputing NA, what is mean and median total number of steps taken per day?

```r
mean(daily_total_steps1, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(daily_total_steps1, na.rm=TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
weekends=c("Saturday", "Sunday")

df2 <- mutate(df1, day_type=ifelse( (weekdays(as.Date(date)) %in% weekends ), "Weekend", "Weekday" ) )

pattern1 <- df2 %>% group_by(interval, day_type) %>% summarise(mean(steps, na.rm=TRUE))

names(pattern1) <- c("interval","day_type", "mean_steps")
pattern1$day_type <- as.factor(pattern1$day_type)
xyplot(mean_steps~interval|day_type, layout = c(1,2), type='l', xlab="Interval", ylab="Number of Steps", data=pattern1)
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

As shown in the the plot diagrams, there are some differences in activity patterns between weekdays and weekends.
