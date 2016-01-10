# This R-File is for playing the results - before format them in R Markdown

#set working directory
oldwd <- getwd()
setwd("c:/Users/Sonic/r/repdata_peerassessment1")

df <- read.csv(unz("activity.zip", "activity.csv"))

sumStepsDay <- tapply(df$steps, df$date, sum)
hist(sumStepsDay, xlab="steps", main="Distribution of Total Steps per Day")

mean(sumStepsDay, na.rm=T)
median(sumStepsDay, na.rm=T)

#barplot(tapply(df$steps, df$date, sum), main="Total number of steps per day")

meanStepsInterval <- tapply(df$steps, df$interval, mean, na.rm=T)
plot(names(meanStepsInterval), meanStepsInterval, type="l", xlab="Interval", ylab="Average Steps", main="Average Daily Activity Pattern")

dfMeanSteps <- aggregate(x=df$steps, by=list(df$interval), mean, na.rm=T)

names(dfMeanSteps) <- c("interval", "cleanedSteps")
dfCleaned <- merge(df, dfMeanSteps)
#alt1
#dfCleaned$cleanedSteps[!is.na(dfCleaned$steps)] <- dfCleaned$steps[!is.na(dfCleaned$steps)]
#dfCleaned <- dfCleaned[, c(4, 3, 1)]
#names(dfCleaned) <- names(df)
#end1

#alt2
dfCleaned$steps[is.na(dfCleaned$steps)] <- dfCleaned$cleanedSteps[is.na(dfCleaned$steps)]
#end2

sumCleanedStepsDay <- tapply(dfCleaned$steps, dfCleaned$date, sum)
hist(sumCleanedStepsDay, xlab="steps", main="Distribution of Total Steps per Day (cleaned data")

mean(sumCleanedStepsDay, na.rm=T)
median(sumCleanedStepsDay, na.rm=T)

library("lubridate")
dfWeekday <- df
dfWeekday$weekend <- ifelse(wday(ymd(df$date)) %in% c(1,7), "weekend", "weekday")

library("lattice")
library("dplyr")
agg <- dfWeekday %>%
        select(weekend, interval, steps) %>%
        filter(!is.na(steps)) %>%
        group_by(weekend, interval) %>%
        summarize(steps=mean(steps))
xyplot(steps ~ interval | weekend, agg, type="l", layout=c(1,2))

#set old working directory
setwd(oldwd)