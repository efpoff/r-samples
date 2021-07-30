#setwd('~/')
getwd()
list.files()
statesInfo <- read.csv('stateData.csv')
subset(statesInfo, state.region == 2)
statesInfo[statesInfo$state.region==1, ]

data("mtcars")
str(mtcars)
subset(mtcars, mpg >= 30 | hp < 60)

reddit<-read.csv('reddit.csv')
table(reddit$employment.status)
summary(reddit)
str(reddit)
levels(reddit$age.range)

library(ggplot2)
qplot(data = reddit, x= age.range)
qplot(data = reddit, x= income.range)

#Setting levels of Ordered Factors Solutions
reddit$age.range <- ordered(reddit$age.range, levels = c('Under 18', '18-24', '25-34', '35-44', '45-54','55-64','65 or Above'))

#lAternate Solutions
reddit$age.range <- factor(reddit$age.range, levels = c('Under 18', '18-24', '25-34', '35-44', '45-54','55-64','65 or Above'), ordered = T)

#Pseudo-Facebook User Data 
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
#Find names of variables
names(pf)
#Histograms of Users' Birthdays
installed.packages('ggplot2')
library(ggplot2)
qplot(x = dob_day, data = pf) +
  scale_x_continuous(breaks = 1:31)+
  facet_wrap(~dob_month)
#or you can also use ggplot()
ggplot(aes(x = dob_day),data = pf) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31)
##Friend Counts - ends up being long tail data
qplot(x= friend_count, data = pf) 
## Friend Counts with less than 1000
qplot(x= friend_count, data = pf) +
  scale_x_continuous(limits = c(0,1000))
##or
qplot(x= friend_count, data = pf, xlim = c(0,1000)) 
## Adjusting the Bin Width and break down by gender
qplot(x= friend_count, data = pf, binwidth =25) +
  scale_x_continuous(limits = c(0,1000), breaks =seq(0, 1000, 50))+
  facet_wrap(~gender)
## Omitting NA Gender Observations
qplot(x= friend_count, data = subset(pf,!is.na(gender)), binwidth =25) +
  scale_x_continuous(limits = c(0,1000), breaks =seq(0, 1000, 50))+
  facet_wrap(~gender)

## Statistics 'by' Gender
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

## Tenure
qplot(x = tenure, data = pf, binwidth = 30,
      color = I('black'), fill = I('#099009'))

## Tenure in Years limiting to 7 years max with labels
qplot(x = tenure/365, data = pf, binwidth = .25,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(1,7,1),limits = c(0,7))+
  xlab('Number of years using Facebook') +
  ylab('Number of users in sample')

## User Ages Quiz using 113 is max age by using summary(pf$age)
qplot(x = age, data = pf, binwidth = 1,
      color = I('black'), fill = I('#e99ee9')) +
  scale_x_continuous(breaks = seq(0, 113, 5))

#Using Log10 to Transform Friend Data
summary(pf$frined_count)
summary(log10(pf$friend_count + 1))

##Histograms Transformed (create 3 - original, log10, sqrt)

install.packages('gridExtra')
library(gridExtra)
p1 <- qplot(x=friend_count, data = pf)
p2 <- qplot(x=log10(friend_count + 1), data = pf)
p3 <- qplot(x=sqrt(friend_count), data = pf)
grid.arrange(p1,p2, p3, ncol=1)

## Alternative Solution for transforming data
p1 <- ggplot(aes(x=friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()
grid.arrange(p1,p2, p3, ncol=1)

## Frequency Polygons install digest package first
qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      binwidth = 10) +
  scale_x_continuous(lim = c(0,1000), breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

qplot(x = friend_count, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      xlab = 'Friend Count',
      ylab = 'Percentage of users with that friend count',
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50))

## Frequency Polygons friend likes by gender
qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      geom = 'freqpoly', color = gender) +
  scale_x_continuous() +
  scale_x_log10()
  
##Likes on the Web
##What's the www_like count for males?
##1430175

by(pf$www_likes, pf$gender, sum)

##Which gender has more www_likes
##  Females 3507665

  

# Box Plot ----------------------------------------------------------------

qplot(x= gender, y= friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot', ylim = c(0,1000))


qplot(x= gender, y= friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  scale_y_continuous(limits = c(0,1000))
  

qplot(x= gender, y= friend_count,
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c (0, 250))

by(pf$friend_count, pf$gender, summary)

#On average, who initiated more friendships
by(pf$friendships_initiated, pf$gender, summary)

##Getting Logical
summary(pf$mobile_likes > 0)
pf$mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
## Finding %
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)

##ScatterPlot
qplot(x=age, y=friend_count, data = pf)

##GGPlot ScatterPlot
ggplot(aes(x=age, y=friend_count), data =pf) +geom_point() +
  xlim(13,90)
summary(pf$age)

##Continuous Plot
ggplot(aes(x=age, y=friend_count), data =pf) +geom_jitter(alpha = 1/20) +
  xlim(13,90)

##Coord_trans function
ggplot(aes(x=age, y=friend_count), data =pf) +geom_point(alpha = 1/20) +
  xlim(13,90) + coord_trans(y = "sqrt")
##Friends Initiated
ggplot(aes(x=age, y=friendships_initiated), data =pf) +geom_jitter(alpha = 1/10, position = position_jitter(h=0)) +
  xlim(13,90) + coord_trans(y = "sqrt")
