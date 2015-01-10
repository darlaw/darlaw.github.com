---
title: 'Coursera Reproducible Research: Evauating Activity Monitoring Data'
author: "Darion Lawson"
output:
  html_document:
    theme: united
    toc: yes
---



## Step 1: Setting up working environment  
The first step is to check for, install, and load require packages, then to set the working directory.  
The *today* variable is  used to label downloaded files.  


```r
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(ggplot2) # to plot data
```

```
## Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.
```

```r
if("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(dplyr) # to summarise data
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
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
setwd("C:/_portfolio/courses/_coursera.reproducibleresearch/assign01") 
today <- Sys.Date() # to label the file withthe download date
```
## Step 2: Loading and preprocessing the data  
This code downloads the file to the working directory, unzips, and then loads that data 
into the *adata* variable.  

```r
today <- Sys.Date() # to label the file withthe download date

a.url <- "https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip"
a.zipfilename <- paste("activity",today, "zip", sep=".") # generate unique zip file name
a.downloadloc <- paste(getwd(),"assignments",sep="/") # download location
a.downloadfilepath <- paste(a.downloadloc,a.zipfilename,sep="/") 

download.file(a.url,a.downloadfilepath,mode = "wb",method="curl")
```

```
## Warning: running command 'curl
## "https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip" -o
## "C:/_portfolio/courses/_coursera.reproducibleresearch/assign01/assignments/activity.2014-11-14.zip"'
## had status 127
```

```
## Warning in download.file(a.url, a.downloadfilepath, mode = "wb", method =
## "curl"): download had nonzero exit status
```

```r
a.unziploc <- paste(getwd(),"assignments","activity",sep="/") # unzip location
unzip(a.downloadfilepath,exdir=a.unziploc)
```

```
## Warning in unzip(a.downloadfilepath, exdir = a.unziploc): error 1 in
## extracting from zip file
```

```r
adata <- read.csv(list.files(a.unziploc)[1],header=TRUE)
```

```
## Warning in file(file, "rt"): cannot open file 'NA': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```
## Step 3: What is mean total number of steps taken per day?  
The first part of this process is to summarize the data by date. We are going to subset 
data and remove instances where the steps value for all intervals is NA.  

```r
    adata1 <- adata # create new working variable so we don't corrupt original data frame
```

```
## Error in eval(expr, envir, enclos): object 'adata' not found
```

```r
    # summarize (aggregate) data by age into a table containing the values specified
    adata.sub <- subset(adata1,!is.na(adata$steps)) # remove NAs
```

```
## Error in subset(adata1, !is.na(adata$steps)): object 'adata1' not found
```

```r
    adata.sub.g <- group_by(adata.sub, date)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'adata.sub' not found
```

```r
    a.summary <- summarise(adata.sub.g,
                           mean_steps = mean(steps),
                           median_steps = median(steps))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'adata.sub.g' not found
```

```r
    a.summary <- arrange(a.summary, median_steps)
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'a.summary' not found
```

Then, create the histograms plots one for median values and the second for mean values. Notice the median plot has no data. The median value for each day is zero.  

```r
ggplot(a.summary,aes(x=date, y=median_steps)) + 
    geom_histogram(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Median Number of Steps by Date ") +
    xlab("Date") + 
    ylab("Number Steps")
```

```
## Error in ggplot(a.summary, aes(x = date, y = median_steps)): object 'a.summary' not found
```

```r
ggplot(a.summary,aes(x=date, y=mean_steps)) + 
    geom_histogram(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Mean Number of Steps by Date ") +
    xlab("Date") + 
    ylab("Number Steps")
```

```
## Error in ggplot(a.summary, aes(x = date, y = mean_steps)): object 'a.summary' not found
```

Now, lets show a table with the mean and median number of steps taken each day.      

```r
    a.summary
```

```
## Error in eval(expr, envir, enclos): object 'a.summary' not found
```

## Step 4: What is the average daily activity pattern?  

First, summarize the data by intervals. The interval value resets each day, and there are 2355 intervals per day.


```r
adata2 <- adata # create new working variable so we don't corrupt original data frame.
```

```
## Error in eval(expr, envir, enclos): object 'adata' not found
```

```r
adata2.sub <- subset(adata2,!is.na(adata$steps)) # remove NAs
```

```
## Error in subset(adata2, !is.na(adata$steps)): object 'adata2' not found
```

```r
adata2.sub.g <- group_by(adata2.sub, interval)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'adata2.sub' not found
```

```r
a2.summary <- summarise(adata2.sub.g,
                       mean_steps = mean(steps))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'adata2.sub.g' not found
```

```r
a2.summary <- arrange(a2.summary, interval)
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'a2.summary' not found
```

Then, make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). There are many ways to do this. Here are the variations.  


```r
# Plot version 1: generic line plot with mean
plot(x=a2.summary$interval, y=a2.summary$mean_steps, type="l",
     main="Average Steps per Interval \nAcross all Dates - using Base Plot",
     xlab="Interval",
     ylab="Number of Steps")
```

```
## Error in plot(x = a2.summary$interval, y = a2.summary$mean_steps, type = "l", : object 'a2.summary' not found
```

```r
#Plot version 2: histogram in ggplot with mean
ggplot(a2.summary,aes(x=interval, y=mean_steps)) + 
    geom_histogram(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Average Steps per Interval \nAcross all Dates - using ggplot histogram") +
    xlab("Interval") + 
    ylab("Number Steps")
```

```
## Error in ggplot(a2.summary, aes(x = interval, y = mean_steps)): object 'a2.summary' not found
```

```r
#Plot version 3: line plot in ggplot with mean
ggplot(a2.summary,aes(x=interval, y=mean_steps)) + 
    geom_line(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Average Steps per Interval \nAcross all Dates - using ggplot line") +
    xlab("Interval") + 
    ylab("Number Steps")
```

```
## Error in ggplot(a2.summary, aes(x = interval, y = mean_steps)): object 'a2.summary' not found
```

```r
# Plot version 4: create a time series object (stats), then plot
tseries <- ts(a2.summary)
```

```
## Error in is.data.frame(data): object 'a2.summary' not found
```

```r
#Plot version 4: create a time series object (stats), then plot
tseries <- ts(a2.summary)
```

```
## Error in is.data.frame(data): object 'a2.summary' not found
```

```r
plot(tseries, plot.type = c("multiple"),
     main="Average Steps per Interval \nAcross all Dates - creating a ts object",
     nc=2,
     xlab="Interval")
```

```
## Error in plot(tseries, plot.type = c("multiple"), main = "Average Steps per Interval \nAcross all Dates - creating a ts object", : object 'tseries' not found
```

Finally, print which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.  

```r
subset(a2.summary, mean_steps==max(a2.summary$mean_steps))
```

```
## Error in subset(a2.summary, mean_steps == max(a2.summary$mean_steps)): object 'a2.summary' not found
```
## Step 5: Imputing missing values

First, calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).  


```r
adata3 <- adata  # create new working variable so we don't corrupt original data frame.
```

```
## Error in eval(expr, envir, enclos): object 'adata' not found
```

```r
sum(is.na(adata3))
```

```
## Error in eval(expr, envir, enclos): object 'adata3' not found
```

Devise a strategy for filling in all of the missing values in the dataset. The new dataset is equal to the original dataset, but with the missing data filled in.  
We'll create temporary derived column for mean steps across each interval value. Then, we'll copy the values of the temporary column to the steps column.   


```r
# create summary table aggregate data by interval containing median and mean values
adata3.sub <- subset(adata3,!is.na(adata3$steps)) # remove NAs
```

```
## Error in subset(adata3, !is.na(adata3$steps)): object 'adata3' not found
```

```r
adata3.g.i <- group_by(adata3.sub, interval) # group data
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'adata3.sub' not found
```

```r
adata3.sum.by.interval <- summarise(adata3.g.i,
                               mean_steps = mean(steps))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'adata3.g.i' not found
```

```r
# Add new columns for derived values 
adata3["d.steps.mean"] <- NA # add a new column for derived data from mean
```

```
## Error in adata3["d.steps.mean"] <- NA: object 'adata3' not found
```

```r
# populate column 4 with derived value from mean steps
i=1
for(i in 1:nrow(adata3)){
    row_interval <- adata3[i,3] # get interval value
    if(is.na(adata3[i,1])){ # check whether steps is NA
        # copy median_steps value from summary table to d.steps.median
        adata3[i,4] <- subset(adata3.sum.by.interval, adata3.sum.by.interval$interval==row_interval)[2] 
    } 
    else if(!(is.na(adata3[i,1]))){ # Otherwise when steps IS NOT NA
        # copy steps value to d.steps.median
        adata3[i,4] <- adata3[i,1]
    } 
}
```

```
## Error in nrow(adata3): object 'adata3' not found
```

```r
# copy d.steps.mean to steps, then drop d.steps.mean
adata3.1 <- adata3
```

```
## Error in eval(expr, envir, enclos): object 'adata3' not found
```

```r
adata3.1$steps <- adata3.1$d.steps.mean
```

```
## Error in eval(expr, envir, enclos): object 'adata3.1' not found
```

```r
adata3.1 <- adata3.1[,1:3]
```

```
## Error in eval(expr, envir, enclos): object 'adata3.1' not found
```

```r
adata3 <- adata3.1
```

```
## Error in eval(expr, envir, enclos): object 'adata3.1' not found
```

Make a histogram of the total number of mean and median number of steps taken each day.  


```r
# summarize (aggregate) data by age into a table using derived d.steps.mean column
adata3.g.d <- group_by(adata3, date) # group by date
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'adata3' not found
```

```r
adata3.summary.derived.date.mean <- summarise(adata3.g.d,
                                         mean_steps = mean(steps),
                                         median_steps = median(steps))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'adata3.g.d' not found
```

```r
adata3.summary.derived.date.mean <- arrange(adata3.summary.derived.date.mean, median_steps)
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'adata3.summary.derived.date.mean' not found
```

```r
ggplot(adata3.summary.derived.date.mean,aes(x=date, y=mean_steps)) + 
    geom_histogram(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Average Steps per Interval \nAcross all Dates using Derived Data for NAs") +
    xlab("Date") + 
    ylab("Average Number Steps")
```

```
## Error in ggplot(adata3.summary.derived.date.mean, aes(x = date, y = mean_steps)): object 'adata3.summary.derived.date.mean' not found
```

```r
ggplot(adata3.summary.derived.date.mean,aes(x=date, y=median_steps)) + 
    geom_histogram(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Median Steps per Interval \nAcross all Dates using Derived Data for NAs") +
    xlab("Date") + 
    ylab("Average Number Steps")
```

```
## Error in ggplot(adata3.summary.derived.date.mean, aes(x = date, y = median_steps)): object 'adata3.summary.derived.date.mean' not found
```

Calculate and report the mean and median total number of steps taken per day.  
Median total number of steps taken per day.  


```r
adata3.summary.derived.date.mean
```

```
## Error in eval(expr, envir, enclos): object 'adata3.summary.derived.date.mean' not found
```

## Step 6: Are there differences in activity patterns between weekdays and weekends?  
First, create a new factor variable (part.of.week) to indicate which day of week it is.  

```r
adata4 <- adata
```

```
## Error in eval(expr, envir, enclos): object 'adata' not found
```

```r
adata4 <- subset(adata4,!is.na(adata4$steps)) # remove NAs
```

```
## Error in subset(adata4, !is.na(adata4$steps)): object 'adata4' not found
```

```r
adata4$date <- as.Date(adata4$date)
```

```
## Error in as.Date(adata4$date): object 'adata4' not found
```

```r
adata4["part.of.week"] <-- NA # create a new column
```

```
## Error in adata4["part.of.week"] <- -NA: object 'adata4' not found
```

```r
# add values to part.of.week
adata4$part.of.week[weekdays(adata4$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- "weekday"
```

```
## Error in adata4$part.of.week[weekdays(adata4$date) %in% c("Monday", "Tuesday", : object 'adata4' not found
```

```r
adata4$part.of.week[weekdays(adata4$date) %in% c("Saturday","Sunday")] <- "weekend"
```

```
## Error in adata4$part.of.week[weekdays(adata4$date) %in% c("Saturday", : object 'adata4' not found
```

To create the plot, we summarize the data, then we plot the summary data.  

```r
# get weekend summary table
adata4.g.d <- group_by(adata4, part.of.week, interval) # group by 
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'adata4' not found
```

```r
a.summary.adata4 <- summarise(adata4.g.d,
                mean_steps = mean(steps))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'adata4.g.d' not found
```

```r
a.summary.adata4 <- arrange(a.summary.adata4, mean_steps)
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'a.summary.adata4' not found
```

```r
# plot weekday mean steps by interval
ggplot(a.summary.adata4, aes(x=interval, y=mean_steps)) + 
    geom_line(stat="identity") +
    facet_wrap(~part.of.week, ncol=1) +
    theme(axis.text.x=element_text(angle=90)) +
    ggtitle("Average Steps per Interval \nAcross all Dates: Weekday versus Weekend") +
    xlab("Interval") + 
    ylab("Number Steps")
```

```
## Error in ggplot(a.summary.adata4, aes(x = interval, y = mean_steps)): object 'a.summary.adata4' not found
```

