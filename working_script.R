

### Loading and preprocessing the data
# Load necessary libraries.
library(ggplot2)

# Set the working directory.
setwd("~/Documents/GitHub/RepData_PeerAssessment1/")
# Load the file.
activities <- read.csv("activity.csv")


### What is mean total number of steps taken per day?
# Make a histogram of the total number of steps taken each day
# Split the data by date and sum the total of steps for each date
df <- tapply(activities$steps, activities$date, sum)
df <- data.frame(df)
names(df) <- c('steps')

# Plot the histogram.
ggplot(df, aes(x=steps)) + geom_histogram(binwidth=2000, aes(fill = ..count..)) +
    theme_bw() +ylab("count of dates") + labs(title="Steps per day")

# Calculate and report the mean and median total number of steps each day.
mean(df$steps, na.rm = TRUE)
median(df$steps, na.rm = TRUE)


### What is the average daily activity pattern?
# Make a time series plot (type = "l") of the 5-minute intervals and average number of steps taken,
# averaged across all days
# Split the data by interval, and take the average of steps for each interval, excluding NA values
df2 <- tapply(activities$steps, activities$interval, mean, na.rm = TRUE)
df2 <- data.frame(df2)
df2$interval <- rownames(df2)
rownames(df2) <- NULL
names(df2) <- c('steps', 'interval')
df2$interval <- as.integer(df2$interval)
#df2$interval <- as.integer(levels(df2$interval)[df2$interval])


# Plot the data
ggplot(data=df2, aes(x=interval, y=steps)) + geom_line(aes(group=1), color="blue") + theme_bw() + 
    scale_x_discrete(breaks=seq(0, 2355, 120)) + xlab("time interval") + 
    ylab("average number of steps") + labs(title="Average steps by time interval")

# Identify the five-minute interval, on average across all days in the data set, that contains the maximum number of steps
which(df2$steps == max(df2$steps))


### Imputing missing values
# calculate and report the total number of rows with 'NA's.
sum(is.na(activities$steps))

# Devise a strategy for filling in all of the missing values. 
# Strategy: replace the missing values with the mean for the time interval across the data set.
# Create a data set that is equal to the original data set but with the missing data filled in.
cleaned_activities <- activities
for(i in 1:nrow(cleaned_activities)) {
    if(is.na(cleaned_activities[i, 1])) {
        cleaned_activities[i,1] = as.double(df2[df2$interval == cleaned_activities[i,3], 1])
    }
}

# What is mean total number of steps taken per day in the new data set?
# Make a histogram of the total number of steps taken each day
# Split the data by date and sum the total of steps for each date
df3 <- tapply(cleaned_activities$steps, cleaned_activities$date, sum)
df3 <- data.frame(df3)
names(df3) <- c('steps')

# Plot the histogram.
ggplot(df3, aes(x=steps)) + geom_histogram(binwidth=2000, aes(fill = ..count..)) +
    theme_bw() +ylab("count of dates") + labs(title="Steps per day")

# Calculate and report the mean and median total number of steps each day.
mean(df3$steps, na.rm = TRUE)
median(df3$steps, na.rm = TRUE)

# Discuss the diferences between the mean and median calculated before and after the missing values are cleaned up?
# This replacement model results in no change in the average number of steps per day.


### Are there differences in activity patterns between weekdays and weekends?
# create a new factor variable in the cleaned_activities data with two levels, weekday and weekend.
cleaned_activities$partOfWeek <- ifelse(weekdays(as.Date(cleaned_activities$date)) %in% c('Saturday', 'Sunday'), "weekend", "weekday")
cleaned_activities$partOfWeek <- as.factor(cleaned_activities$partOfWeek)

# Make a panel plot that compares the weekday and weekend data.
# Split the data by interval and part of week
split_set <- split(cleaned_activities$steps, list(cleaned_activities$interval, cleaned_activities$partOfWeek))
# use lapply to iterate over each item in the resulting list and take the average of the values.
combined_set <- lapply(split_set, mean)
# turn the reuslts back into a data frame, and split the interval and part of week back out into columns.
df4 <- do.call(rbind.data.frame, combined_set)
factors <- data.frame(t(sapply(strsplit(rownames(df4), "[.]"), c)))
df4 <- cbind(factors, df4)
df4 <- dplyr::rename(df4, interval=X1, partOfWeek=X2, steps=c.2.25115303983228..0.445283018867925..0.173165618448637..0.1979035639413..)
df4$interval <- as.integer(levels(df4$interval)[df4$interval])

# Draw the plot.
ggplot(data=df4, aes(x=interval, y=steps)) + geom_line(aes(group=1), color="blue") + 
    theme_bw() + scale_x_discrete(breaks=seq(0, 2355, 120)) + 
    xlab("time interval") + ylab("average number of steps") + 
    labs(title="Average steps by time interval") + facet_grid(partOfWeek~.)
