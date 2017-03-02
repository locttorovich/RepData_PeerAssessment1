**1. Load and preprocess the data**

    steps_df <- read.csv("activity.csv", na.strings="NA", stringsAsFactors=FALSE)

Note: this file did not require any other preprocessing before
proceeding.

**2. What is the mean total number of steps taken per day?**

    #Calculate the total steps per day and create a histogram of the results:

    #get total number of steps per day
    day_steps <- aggregate(steps ~ date, data = steps_df, FUN = sum)

    #histogram it
    hist(day_steps$steps, main = "Total Number of Steps per Day", 
         xlab = "Number of Steps", col = "red")

![](Pa1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

The mean number of total steps per day is 10766 steps, while the median
is 10765 per day.

**3. What is the average daily activity pattern?**

    #Make a time series plot of the interval and avg steps across all days:

    #get avg steps taken per interval
    int_steps <- aggregate(steps ~ interval, data = steps_df, FUN = mean)

    #plot the data
    plot(int_steps, type="l", xlab = "Interval", ylab = "Average Steps", 
         main = "Average Steps per 5-Minute Interval" )

![](Pa1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

On average, the 835th 5-minute interval contains the maximum number of
steps.

**4. Imputing missing values**

There are 2304 missing values in this dataset. I will fill the missing
values with the mean number of steps across all days for the particular
interval.

    #Create a new data set with missing values filled in:

    imp_steps <- steps_df

    for(r in 1:nrow(imp_steps)){
        if (is.na(imp_steps$steps[r])) {
            repl <- int_steps$steps[int_steps$interval == imp_steps$interval[r]];
            imp_steps$steps[r] <- repl;
        }
    }

    #Make a histogram of total steps per day with missing values filled in:

    #get # of steps per day
    day_steps <- with(imp_steps, tapply(steps, date, sum, na.rm=TRUE))

    #histogram it
    hist(day_steps, main = "Histogram of Number of Steps Per Day\nImputed Missing Values", 
         xlab = "Number of Steps Per Day", col = "red", breaks=15)

![](Pa1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

With the missing values filled in, the mean number of total steps per
day is 10766 steps, while the median is 10766 per day. The median did
not change from the missing value data set, but the mean did go up one,
a 0.00009 difference. Filling missing values with the average of the
corresponding interval had no real effect.

**5. Are there differences in activity patterns between weekdays and
weekends?**

    #Create a new factor variable in the dataset indicating if the day is a weekend or weekday:

    #convert date field to Date datatype
    imp_steps$date2 <- as.Date(imp_steps$date, format = "%Y-%m-%d")

    #get day of week for each date
    imp_steps$day <- weekdays(imp_steps$date2)

    #set new column indicating if the day is a weekend or weekday
    for (i in 1:nrow(imp_steps)) {
        if(imp_steps$day[i] == "Saturday" | imp_steps$day[i] == "Sunday") {
            imp_steps$day_type[i] <- "Weekend"
        } else {
            imp_steps$day_type[i] <- "Weekday"
        }
    }

    #convert to a factor for later
    imp_steps$day_type <- factor(imp_steps$day_type)

    #Make a time series plot of the average steps for each interval, divided out by weekend/weekday:

    day_type_steps <- aggregate(steps ~ interval + day_type, data = imp_steps, FUN = mean)

    library("lattice")

    xyplot(steps ~ interval | day_type, day_type_steps, type = "l", layout = c(1, 2), 
           xlab = "Interval", ylab = "Number of Steps", 
           main = "Average Number of Steps per 5 Minute Interval\nWeekends vs Weekdays ")

![](Pa1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)
