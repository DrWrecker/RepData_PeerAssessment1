---
title: "Reproducible Research: Peer Assessment 1"
date: "September 20, 2015"
output: html_document
---

Course Project 1 Overview: 
- Read in file
- Calculate mean number of total steps per day taken with graphic output 
- Calculate and plot average daily activity pattern by interval
- Imput misssing values in dataset and recalculate of mean and median activity
- Calculate and plot average daily activity pattern by interval comparing of activity pattern on weekdays vs. weekend

## Loading and preprocessing the data


```r
file_in <- "activity.csv" 
activity <- read.csv(file_in)
library(lattice)
library(dplyr)
```


## What is mean total number of steps taken per day?

```r
 ## function for later use - mean and median stats
stepstats <- function(activity) {
        # factors to Date class
    activity <- mutate(activity, date = as.Date(as.character(date)))

    #group file by date
    dateact <- group_by(activity,date)
    
    c <-summarise(dateact, total_steps = mean(sum(steps), na.rm =TRUE))
    meanSPD <<- mean(c$total_steps, na.rm =TRUE)
    medianSPD <<- median(c$total_steps, na.rm = TRUE)
    c
    
}
```


```r
    c <- stepstats(activity)

    #plot graph
    hist(c$total_steps, breaks = 10, col = "green", main = "Total Steps Histogram", xlab = "Total Steps per Day")
```

![plot of chunk mean_activity](figure/mean_activity-1.png) 

Mean number of steps per day is 10766.2. Median number of steps per day is   10765.

## What is the average daily activity pattern?

```r
dailypattern <- function(activity,maintitle,x_label,y_label) {
    
    #split data by interval (drop date)
    aveAct <- with(activity, split(steps, interval))
    
    # calculate mean steps over all days at each interval
    meanperday <- sapply(aveAct, mean, na.rm=TRUE)

    # get intervals from named vector
    n <- names(meanperday)
    
    # convert character names to numeric
    interval <- as.numeric(n)
    
    # create data frame  with means and intervals
    ms <- as.vector(meanperday)
    mpd <-data.frame(interval = interval, meansteps = meanperday)
    
    #plot mean steps vs interval

    plot(mpd$interval, mpd$meansteps, type = "l", col = "blue", main = maintitle, xlab = x_label, ylab = y_label)
    
    #determine index of max mean steps
    mval <- which.max(mpd$meansteps)
    
    #determine value of max mean steps
    msteps <- mpd$meansteps[mval]
    
    #determine interval of max mean steps
    minterval <- mpd$interval[mval]
    
    # output mean and interval
    out <- paste0(maintitle, ": The maximum mean of ", formatC(msteps, digits = 6), " steps is at interval ", minterval, ".")
    out
}
```


```r
 dailypattern(activity,"Average daily activity pattern","Interval", "Average Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```
## [1] "Average daily activity pattern: The maximum mean of  206.17 steps is at interval 835."
```


## Imputing missing values

```r
    #determine total number of observations
    a<-dim(activity)
    totobs <- a[1]
    
    # determine if steps = NA in dataset
    missing <- is.na(activity$steps)
    
    # convert logical to count of NAs
    totmissing <- sum(missing)
    complete <- totobs - totmissing
```

The number of NAs in the dataset is 2304 out of 17568 observations.


```r
    # copy dataset
    new_activity <- activity
    
    # make a list of daily means to replace NAs in new data set if needed
    
    temp1 <- with(new_activity, split(steps, date))
    daymeans <- lapply(temp1,mean)
    
    # loop thru TRUE missing entries and assign NA to 0 or daily mean
    
    for (i in 1:length(missing)){
        if(missing[i] == TRUE) {
            #get date as character vector
            dateof <- as.character(activity$date[i])
            #if mean value of that date is NA use mean of zero
            #else use daily mean to replace NA value
            if( is.na(daymeans[dateof]) ) {
                new_activity$steps[i] = 0
                # print(paste(i, dateof))
                } else {
                    new_activity$steps[i] = daymeans[[dateof]]
                    }
        }
    }

    # new_activity = dataset free of NAs
```
    

```r
    c <- stepstats(new_activity)

    #plot graph of dataset with imputed values 
    hist(c$total_steps, breaks = 10, col = "red", main = "Total Steps Histogram - with imputed values", xlab = "Total Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Since entire days had NA values, a step value of 0 replaced the NAs. The only change to the histogram would be an increase in the frequency of the bar for a low number of steps (0-~1250)

Mean number of steps per day is 9354.23. Median number of steps per day is   10395.

The mean and mediam values for steps per day with imputed values are less that the previous calculation. This makes sense since, in the previous calculation,  NA values were removed before the calculation resulting in a data set of 15264 observations for the mean and median calculations. When the missing values are imputed, there are 2304 more observations; both the mean and median values decreased accordingly.

## Are there differences in activity patterns between weekdays and weekends?


```r
    interval_ave <- function(activity,fact) {
    #split data by interval (drop date)
    aveAct <- with(activity, split(steps, interval))
    
    # calculate mean steps over all days at each interval
    meanperday <- sapply(aveAct, mean, na.rm=TRUE)
    
    # get intervals from named vector
    n <- names(meanperday)
    
    # convert character names to numeric
    interval <- as.numeric(n)
    
    # create data frame  with means and intervals
    ms <- as.vector(meanperday)
    mpd <-data.frame(interval = interval, meansteps = meanperday, Factor = fact)
    mpd
}
    
new_activity$Date <- as.Date(as.character(new_activity$date))
    new_activity$weekday <- weekdays(new_activity$Date)

    weekday <- "weekday"
    weekend <- "weekend"
    x <- "Interval"
    y <- "Average steps"
    new_activity$weekday <-sub("Saturday|Sunday","weekend", new_activity$weekday,)
    new_activity$weekday <-sub("Monday|Tuesday|Wednesday|Thursday|Friday","weekday", new_activity$weekday,)
    
    #treat weekend and weekday as factors
    new_activity$weekday <- as.factor(new_activity$weekday)
    
    # subset data frames by factors
    day_activity <- subset(new_activity, new_activity$weekday =="weekday")
    end_activity <- subset(new_activity, new_activity$weekday =="weekend")
    
    day <- interval_ave(day_activity, weekday)
    end <- interval_ave(end_activity,weekend)
    
    #combine data frames of mean values
    c <- rbind(day, end)
    
    #panel plot
    p<- xyplot(meansteps ~ interval | Factor, c, type = "l", xlab ="Interval", ylab = "Average steps")
    print(p)
```

![plot of chunk weekday_diffs](figure/weekday_diffs-1.png) 

There is a clear difference in average weekday vs. weekend activity patterns. There appears to be more variation in activity during intervals throughout the day on weekends though the mean number of steps taken on the weekend is less than during the weekdays.
