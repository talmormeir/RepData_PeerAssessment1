# Reproducible Research: Peer Assessment 1
####*Libraries and setup used in code*
```
library(rmarkdown)
echo = TRUE
library(ggplot2)
library(lattice)
```

## Loading and preprocessing the data

```r
data <- read.csv("C:/Users/talmo/Documents/activity.csv", stringsAsFactors=FALSE)
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

####*set date column and seperate week versus weekend days*

```r
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
data$weekday<-weekdays(data$date)
    
data$weekend.week <- ""
for (i in 1:nrow(data)){
if (data$weekday[i] == "Saturday" | data$weekday[i] == "Sunday"){
    data$weekend.week[i]<-"weekend"}
    else{data$weekend.week[i]<-"weekday"}}
```
## What is mean total number of steps taken per day?

```r
sum_data <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
names(sum_data) <- c("date", "Step")
```

*plot number of steps each day*

```r
require(ggplot2)
```


```r
p<-ggplot(sum_data, aes(x=Step)) + 
    geom_vline(aes(xintercept=mean(Step)),color="blue", linetype="dashed", size=1)+
    geom_histogram(binwidth=1000,color="darkblue", fill="lightblue")+ggtitle("Steps per day Frequency")
p
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)


```r
mean(sum_data$Step)  #mean of steps per day
```

```
## [1] 9354.23
```

```r
median(sum_data$Step)  #median of steps per day
```

```
## [1] 10395
```

## What is the average daily activity pattern?

*Compute the means per interval*

```r
intervals<- aggregate(data$steps,by=list(data$interval),FUN=mean, na.rm=TRUE)
names(intervals) <- c("interval", "Avg")

plot(intervals$interval, intervals$Avg,type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)


*maximum mean interval*

```r
maximum<- which(intervals$Avg == max(intervals$Avg))
max_interval <- intervals[maximum, 1]
max_interval
```

```
## [1] 835
```

## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
data$steps[is.na(data$steps)]<-mean(data$steps, na.rm=TRUE) #replace NA with total mean
```

*Compute the total number of steps each day (NA values removed)*

```r
sum_data1 <- aggregate(data$steps, by=list(data$date), FUN=sum)
names(sum_data1) <- c("date", "Step")
```
*Compute the histogram of the total number of steps each day*

```r
ggplot(sum_data1, aes(x=Step)) + 
    geom_vline(aes(xintercept=mean(Step)),color="blue", linetype="dashed", size=1)+
    geom_histogram(binwidth=1000,color="red", fill="yellow")+ggtitle("Steps per day Frequency (NA replaced by Avg value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

```r
mean(sum_data1$Step)
```

```
## [1] 10766.19
```

```r
median(sum_data1$Step)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
```
*Compute the average number of steps taken using averaged across all daytype variable*

```r
mean_data <- aggregate(data$steps, 
                       by=list(data$weekend.week, 
                               data$weekday, data$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")
```
*Compute the time serie plot*

```r
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       col='black',
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)

