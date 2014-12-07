# Reproducible Research: Peer Assessment 1
Robert Monné  

This is a markdown document to provide answers to the Peer Assignment for the course Reproducible research in the data science specialisation

## Loading and preprocessing the data
We will first load and preprocess the data from the .csv file. We use the lubridate package to transform the dates to a more usable format

```r
data <- read.csv("activity.csv")
library(lubridate)
data$date <- ymd(data$date)
```

## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day

```r
totalsteps <- tapply(data$steps, data$date, sum)
hist(totalsteps, xlab="Total steps per day", main="Histogram of Total steps per day")
```

![](PA1_template_files/figure-html/histogram1-1.png) 



And compute the mean and median of the total number of steps taken each day. There are NAs in the dataset so we get NAs as a result

```r
mean(totalsteps)
```

```
## [1] NA
```

```r
median(totalsteps)
```

```
## <NA> 
##   NA
```


## What is the average daily activity pattern?

Calculate the average of steps per time interval  
Plot the intervals on the X-axis and the 'average amount of steps' on the Y-axis  

```r
## moet ik hier niet de na.rm weghalen?
averagesteps <- as.data.frame(tapply(data$steps, data$interval, mean, na.rm=T))

plot(rownames(averagesteps), averagesteps[,1], type = "l", xlab="Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/average-1.png) 

In what interval do we obtain the maximum amount of average steps?

```r
names(which.max(averagesteps[,1]))
```

```
## [1] "835"
```

## Imputing missing values
First calculate how many missing values are in the current dataset
Then we create a new data set to be imputed with new values for the NAs
Then we use a for loop to set the NAs with a new value (the mean of the steps on the given interval)



```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
newdata <- data

#for every row in the dataset
for(i in 1:length(newdata[,1])) {
  #check if there is a NA
  if(is.na(newdata[i,1])) {
    #grab the associated interval of the NA
    interval <- newdata[i,3]
    #use the found interval to grab the mean steps of that interval and set is as new value for the NA
    #we grab the first value resulting vector due to overlappings like: 815 & 1815
    newdata[i,1] <- averagesteps[grep(as.character(interval), rownames(averagesteps)),1][1]
  }
}

sum(is.na(newdata))
```

```
## [1] 0
```

```r
totalstepsnew <- tapply(newdata$steps, newdata$date, sum)
```

With the new dataset we create a histogram of the total steps per day

```r
hist(totalstepsnew, xlab="Total steps per day", main="Histogram of Total steps per day")
```

![](PA1_template_files/figure-html/histogram2-1.png) 

Without the NAs we can calculate a better mean and median

```r
mean(totalstepsnew)
```

```
## [1] 10766.19
```

```r
median(totalstepsnew)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

First we create a new variable with the day of the week  
After that we convert all saturdays and sundays to "weekend"
Everything that's not a saturday or sunday will be converted to "weekday"  
At last we 


```r
library(dplyr, quietly = TRUE)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
newdata <- mutate(newdata, weekday = weekdays(data$date))

#since my pc's language is Dutch you'll see the Dutch translation of saturday and sunday in the if statements
for(i in 1:length(newdata$weekday)){
  if(newdata$weekday[i] == "zaterdag")
    newdata$weekday[i] <- "weekend"
  if(newdata$weekday[i] == "zondag")
    newdata$weekday[i] <- "weekend"
  else newdata$weekday[i] <- "weekday"
}
```

We create a 2 plots of the average steps per interval. 1 for the weekdays, and 1 for the weekends


```r
#select rows with  weekday of weekend and create two seperate datasets
weekdays <- newdata[newdata$weekday == "weekday",]
weekends <- newdata[newdata$weekday == "weekend",]

#calculate the average per interval on a weekday or weekend
avgstepsweekday <- as.data.frame(tapply(weekdays$steps, weekdays$interval, mean, na.rm=T))
avgstepsweekend <- as.data.frame(tapply(weekends$steps, weekends$interval, mean, na.rm=T))

par(mfrow = c(2,1))

plot(rownames(avgstepsweekday), avgstepsweekday[,1], 
       type = "l", xlab="Interval", ylab="Average Steps", main = "Weekdays")
plot(rownames(avgstepsweekend), avgstepsweekend[,1], 
       type = "l", xlab="Interval", ylab="Average Steps", main = "Weekends")
```

![](PA1_template_files/figure-html/plots-1.png) 
