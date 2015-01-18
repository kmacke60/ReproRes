setwd("c:/RR/Reprores/repdata_data_activity")
library(plyr)
library(tidyr)
library(lattice)
## Read in the data
#====
        activity <- read.csv("activity.csv",header=TRUE)

## Creating dataset with missing values removed
step_data <- activity[complete.cases(activity),]
step_data$interval_revised <- sprintf("%04d", step_data$interval)
step_data$int_time <- strptime(step_data$interval, format = "%H%M")

## Histogram of step count per day, missing data removed
step_data <- transform(step_data, dayf=factor(date))
daysum <- tapply(step_data$step, step_data$dayf, sum)
hist(daysum, xlab = "Daily Step count")

## Mean and Median steps per day, missing data removed:
# First calculate total no of steps per day
step_sumnm <- tapply(step_data$steps,step_data$dayf, sum)
daily_stepsnm <- as.data.frame(step_sumnm)

# Median steps per day
step_median_me <- quantile(daily_stepsnm$step_sumnm,probs=0.5)
print("Median steps per day 1 = ")
print(step_median_me)
print("Mean steps per day 1 = ")
step_mean_me <- mean(daily_stepsnm$step_sumnm)
print(step_mean_me)

## Plot of average number of steps in each 5 minute interval
step_data <- transform(step_data, intervalf=factor(interval))
intmean <- tapply(step_data$step, step_data$intervalf, mean)
maxstep = max(intmean)
## get interval with maximum number of steps
max_index <- which(intmean == max(intmean))
max_interval <- step_data$interval_revised[max_index]
max_interval

# plot mean number of steps per interval
plot(intmean, type="l", xlab="5 minute interval", ylab = "Average number of steps")

total_length <- length(activity)
     
## Imputing missing data
# First, get count of missing data

total_length <- dim(activity)[1]
present_length <- dim(step_data)[1]
missing_count <- total_length - present_length
print("Missing = ")
print(missing_count)

# Make wide version of actitity data frame
widestep <- dcast(activity, date ~ interval, value.var = "steps")
wsdate <- widestep$date
names(widestep) <- paste("time", names(widestep), sep = "")

# replace missing values wth row means
for (i in which(sapply(widestep, is.numeric))) {
  widestep[is.na(widestep[, i]), i] <- mean(widestep[, i],  na.rm = TRUE)
}
widestep$date <- wsdate

# Make long version of activity with missing replace by mean for interval

step_nomiss <- widestep %>%
  gather(interval, step, time0:time2355)

## Histogram of step count per day
step_nomiss$interval <- as.numeric(substr(step_nomiss$interval, 5,8))
step_nomiss <- transform(step_nomiss, dayf=factor(date))
daysum <- tapply(step_nomiss$step, step_nomiss$date, sum)
hist(daysum, xlab = "Step count per day imputted missing")

## Mean and Median steps per day:
# First calculate total no of steps per day
step_summi <- tapply(step_nomiss$step,step_nomiss$date, sum)
daily_stepsmi <- as.data.frame(step_summi)

# Median steps per day
step_median_mi <- quantile(daily_stepsmi$step_summi,probs=0.5)
print("Median steps per day = ")
print(step_median_mi)
print("Mean steps per day = ")
step_mean_mi <- mean(daily_stepsmi$step_summi)
print(step_mean_mi)

meandiff <- abs(step_mean_mi - step_mean_me)
meddiff <- abs(step_median_mi - step_median_me)

step_nomiss$timedate <- as.Date(step_nomiss$timedate,format="%Y-%m-%d")
step_nomiss$day <- weekdays(step_nomiss$timedate)
step_nomiss$weekday <- (step_nomiss$day == "Saturday" | step_nomiss$day== "Sunday")
step_nomiss$weekday <- as.factor(step_nomiss$weekday)

step_nomiss$intday <- as.factor(paste(step_nomiss$weekday,step_nomiss$interval))
suppressWarnings(intmean2 <- aggregate(step_nomiss, by = list(step_nomiss$interval, step_nomiss$weekday), mean))
intmean2$day <- rep(c("Weekend", "Weekday"), each=288)
maxstep = max(intmean2$step)
print(maxstep)
xyplot(step ~ interval | day, data = intmean2, layout = c(1,2), type="l",ylab="Number of steps")
