setwd("C:/Users/nzhong/Documents/rstudio/CourseraDataScienceTrack/repreducible Research/")
data<-read.csv("activity.csv")
library(data.table)
library(ggplot2)

##part1 Ignore missing value
no_na_data<-as.data.table(data[complete.cases(data),])

#1. What is mean total number of steps taken per day?
# no_na_data$date<-as.Date(no_na_data$date,format="%Y-%m-%d")
# no_na_data$day<-format(no_na_data$date,"%d")
daily_steps<-no_na_data[,sum(steps),by=list(date)]$V1
hist(daily_steps,breaks=20,xlab="number of steps per day",main="Daily Steps Histogram(ignore missing value)",ylim=c(0,15))
mean(daily_steps)
median(daily_steps)

#2. average daily activity pattern
daily_patterns<-no_na_data[,mean(steps),by=list(interval)]
setnames(daily_patterns,c("interval","AvgSteps"))
max_steps_interval<-daily_patterns[order(-AvgSteps)][1,]
plot(daily_patterns,xlab="daily time stamps",ylab="average number of steps",main="Daily Step Patterns")


##part2 Imputing the missing value
num_missing<-dim(data)[1]-dim(no_na_data)[1]
temp<-data[is.na(data$steps),]
fill<-as.data.table(merge(temp,daily_patterns,by="interval"))
fill[,steps:=NULL]
setnames(fill,"AvgSteps","steps")
new_data<-rbind(fill,no_na_data)

daily_steps_new<-new_data[,sum(steps),by=list(date)]$V1
hist(daily_steps_new,breaks=20,xlab="number of steps per day",main="Daily Steps Histogram(missing value filled)")
mean(daily_steps_new)
median(daily_steps_new)


new_data$DayOfWeek<-ifelse(!weekdays(as.Date(new_data$date)) %in% c("Saturday", "Sunday"),
                           "weekday","weekend")
daily_patterns_new<-new_data[,mean(steps),by=list(interval,DayOfWeek)]
qplot(interval,V1,data=daily_patterns_new,facets=DayOfWeek ~.,xlab="daily time stamps",ylab="average number of steps",main="Daily Step Patterns")
