#### Loading the data in  #####  
data <- read.csv("C:/Users/talmo/Documents/activity.csv", stringsAsFactors=FALSE)
head(data)


# set date column and seperate week versus weekend days:
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
data$weekday<-weekdays(data$date)
    
data$weekend.week <- ""
for (i in 1:nrow(data)){
if (data$weekday[i] == "Saturday" | data$weekday[i] == "Sunday"){
    data$weekend.week[i]<-"weekend"}
    else{data$weekend.week[i]<-"weekday"}}



##### calculate the mean  ######

sum_data <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
names(sum_data) <- c("date", "Step")

# plot number of steps each day
library(ggplot2)
p<-ggplot(sum_data, aes(x=Step)) + 
    geom_vline(aes(xintercept=mean(Step)),color="blue", linetype="dashed", size=1)+
    geom_histogram(binwidth=1000,color="darkblue", fill="lightblue")+ggtitle("Steps per day Frequency")
p


mean(sum_data$Step)
median(sum_data$Step)

####   What is the average daily activity pattern?   #####

# Compute the means per interval
intervals<- aggregate(data$steps,by=list(data$interval),FUN=mean, na.rm=TRUE)
names(intervals) <- c("interval", "Avg")

# plot
plot(intervals$interval, intervals$Avg,type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Average number of steps per intervals")

# maximum mean interval
maximum<- which(intervals$Avg == max(intervals$Avg))
max_interval <- intervals[maximum, 1]
max_interval

########   Inputing the missing values    ##############
sum(is.na(data$steps))
data$steps[is.na(data$steps)]<-mean(data$steps, na.rm=TRUE) #replace NA with total mean


# Compute the total number of steps each day (NA values removed)
sum_data1 <- aggregate(data$steps, by=list(data$date), FUN=sum)
names(sum_data1) <- c("date", "Step")

# Compute the histogram of the total number of steps each day

p1<-ggplot(sum_data1, aes(x=Step)) + 
    geom_vline(aes(xintercept=mean(Step)),color="blue", linetype="dashed", size=1)+
    geom_histogram(binwidth=1000,color="red", fill="yellow")+ggtitle("Steps per day Frequency (NA replaced by Avg value)")
p1

mean(sum_data1$Step)
median(sum_data1$Step)

####Are there differences in activity patterns between weekdays and weekends?

library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(data$steps, 
                       by=list(data$weekend.week, 
                               data$weekday, data$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")

# Compute the time serie plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       col='black',
       layout=c(1,2))


