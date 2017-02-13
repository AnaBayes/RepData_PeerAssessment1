---
title: "PA1_template"
author: "Ana Martincic"
date: "29 January 2017"
output: html_document
---
# Loading and preprocessing the data

Show any code that is needed to 

1. Load the data (i.e. ` r read.csv()`)

2. Process/ transform the data (if necessary) into a format suitable for your analysis

```{r}
library(knitr)
library(dplyr)
library(data.table)
library(lattice)
library(ggplot2)
data<-read.csv("activity.csv",sep=",",header=TRUE)
```

# What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```{r}
total_steps_by_day<- aggregate(steps ~ date, data, sum)
head(total_steps_by_day)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```{r histogram1}
hist(total_steps_by_day$steps,col="blue",main="Total number of steps per day",xlab="Number of steps",ylab="Number of days")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean_total_steps<-mean(total_steps_by_day$steps,na.rm=TRUE)
median_total_steps<-median(total_steps_by_day$steps,na.rm=TRUE)
```

Mean of the total number of steps taken per day is `r mean_total_steps`, and the median is `r median_total_steps`.

#What is the average daily activity pattern?

1. Make a time series plot (i.e. ` r type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r plot}
steps_by_interval <- aggregate(steps ~ interval, data, mean,na.rm=TRUE)
plot(steps_by_interval,type="l",xlab="5-min Interval",ylab="Average number of steps",main="Average number of steps per day by interval")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_interval<-steps_by_interval$interval[steps_by_interval$steps==max(steps_by_interval$steps)]
max_interval
```

Interval 835 contains (on average) the maximum number of steps.


# Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
missing_values<-sum(is.na(data$steps))
missing_values
```

There are 2304 in dataset.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We will use the mean for that day to fill in the missing values:

```{r}
average_steps_by_day<-aggregate(steps ~ date, data, mean)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
new_data<-data
for (i in 1:dim(data)[1]) { 
  if (is.na(data$steps[i])) {
    date_temp<-data$date[i]
    new_data$steps[i]<-floor(average_steps_by_day$steps[date_temp])
    }}
```


4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r histogram2}
imputed_total_steps<- aggregate(steps ~ date, new_data, sum)
hist(imputed_total_steps$steps,col="red",main="Total number of steps per day",xlab="Number of steps",ylab="Number of days")
hist(total_steps_by_day$steps,col="blue",main="Total number of steps per day",xlab="Number of steps",ylab="Number of days",add=T)
legend("topright", c("Original data", "Imputed data"), col=c("blue", "red"), lwd=5)

imputed_mean<-mean(imputed_total_steps$steps)
imputed_median<-median(imputed_total_steps$steps)
imputed_mean
imputed_median

difference_mean <- mean_total_steps-imputed_mean
difference_median <- median_total_steps-imputed_median
total_difference <- sum(total_steps_by_day$steps)-sum(imputed_total_steps$steps)

difference_mean
difference_median
total_difference
```

The mean of imputed data is 334.4553 less than the mean of the original data, while the median is also less (for 179.5). Total daily number of steps is bigger than in the original data (which can also be seen from the plot above).


# Are there differences in activity patterns between weekdays and weekends?

For this part the ` r weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
new_data$week = as.factor(ifelse(is.element(weekdays(as.Date(new_data$date)),weekdays), "Weekday", "Weekend"))
```

2. Make a panel plot containing a time series plot (i.e. ` r type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r xyplot}
imputed_steps <- aggregate(steps ~ interval + week, new_data, mean)
par(mfrow=c(2,1))
xyplot(imputed_steps$steps ~ imputed_steps$interval|imputed_steps$week, main="Average Steps per Day by Interval",xlab="5-min Interval", ylab="Number of steps", type="l")
```

From the two plots we can see that the subject is more active during the weekends.