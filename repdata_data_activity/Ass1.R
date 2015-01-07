setwd("c:/Users/km40780/Desktop/RR/repdata_data_activity")
## Read in the data
#====
        activity <- read.csv("activity.csv",header=TRUE)

## Creating dataset with missing values removed
step_data <- activity[complete.cases(activity),]

## Histogram of step count per day
step_data <- transform(step_data, dayf=factor(date))
daysum <- tapply(step_data$step, step_data$dayf, sum)
hist(daysum, xlab = "Step count per day")

## Median steps per day:
step_median <- quantile(step_data$steps,probs=0.5)
print("Median steps per day = ")
print(step_median)

## Mean steps per day:
step_mean <- mean(step_data$steps)
print("Mean steps per day = ")
print(step_mean)

## Plot of average number of steps in each 5 minute interval
step_data <- transform(step_data, intervalf=factor(interval))
intmean <- tapply(step_data$step, step_data$intervalf, mean)
maxstep = max(intmean)
plot(intmean, type="l", xlab="5 minute interval", ylab = "Average number of steps")

total_length <- length(activity)
                      