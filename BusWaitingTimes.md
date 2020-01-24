---
title: "BusWaitingTimes"
author: "Paul M"
date: "1/23/2018"
output:
  html_document:
    df_print: paged
    keep_md: true
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.  

***

If you know Latex, you can use it within an Rmarkdown document. For example, to create some vertical space:
\vspace*{1in}   

**Bus Waiting Time Problem**

This document is going to show results for the Bus Waiting Times questions of Week 2.
We will start by defining some global variables. We are assuming that the waiting time between buses is exponentially distributed, with time measured in hours. We embed R code in the document as follows:


```r
Lambda<-1   # the paramter of the exponential distribution for waiting times
WhenDoWeArriveAtBusStop<-10 # For part 1 of the question, this is the time at which we arrive at the bus stop
LengthOfDay<-24 # We will assume we are considering a single day of length 24 hours.
HowManySims<-1000 # The number of replicates we will simulate to answer the questions
set.seed(587641) # Remember to set the random number seed
```
You will notice that Rmarkdown is not great at wrapping long lines of Rcode.

So now let's write a function to simulate the arrival times of buses at the bus-stop over a single day.

```r
ArrivalTimes<-function(DayLength,ArrivalRate){
  Time<-0  # initialize the day to start at hour 0
  AllTimes<-NULL  # this will store the arrival times, for now ther eis nothing to store
  Time<-Time+rexp(1,ArrivalRate)  #this is when the first bus arrives
  while (Time<DayLength){
    AllTimes<-cbind(AllTimes,Time)  # store the arrival time
    Time<-Time+rexp(1,ArrivalRate)  # find the time to the next bus
  }
  return (AllTimes)  # return the vector of arrival times
}
```

Test the arrival time function:

```r
(TodaysTimes<-ArrivalTimes(LengthOfDay,Lambda))   # The outer brackets tell R to output the result
```

```
##           Time      Time      Time      Time      Time     Time     Time
## [1,] 0.1643074 0.4672582 0.5087003 0.6310177 0.6789522 2.548206 3.011344
##          Time    Time     Time     Time     Time     Time     Time
## [1,] 3.767226 4.71231 5.277462 5.888161 6.779569 7.486836 8.226866
##          Time     Time     Time     Time     Time    Time     Time
## [1,] 9.537704 9.894606 10.16172 10.32604 11.19362 11.6804 12.12816
##          Time     Time     Time     Time     Time     Time     Time
## [1,] 13.42734 14.33686 14.37506 14.55526 15.61042 15.74129 15.97298
##          Time    Time     Time     Time     Time     Time     Time
## [1,] 15.99351 17.7563 18.05427 19.01404 21.28378 21.48649 22.17591
##          Time
## [1,] 23.78344
```
Those look ok.

Now simulate a large number of days, and answers each of the questions from class.
We'll start by declaring a bunch of vector variables to store the relevent times.

```r
TimeIWaited<-c()#<-NULL # A vector that will store our waiting times
AverageTimesBetweenBuses<-NULL # A vector to store the average time between buses for each day. there's nothing in it at the  moment.
TimeIntervalBeforeMyBus<-NULL # For part 3, this will store the time between the bus I caught...
# and the one before it
TimeBeforeMyBus<-NULL # We will also store the time between my arrival at the bus stop 
# and the last bus that left the stop before I arrived
```

And now the code that does the simulation study.

```r
for (Days in 1:HowManySims){
  # First generate the arrival times for today
  # Note that, for convenience we assume the days are independent of each other
  TodaysTimes<-ArrivalTimes(LengthOfDay,Lambda)

  # For Q1 we assume we arrive at hour 10. How long do we then wait for a bus?
  #which bus is the first to arrive after I get to the stop...
  WhichBusICatch<-min(which(TodaysTimes>WhenDoWeArriveAtBusStop)) 
  # when does it arrive...
  TimeICatchBus<-TodaysTimes[WhichBusICatch] 
  # Store the time I waited...
  TimeIWaited<-cbind(TimeIWaited,TimeICatchBus-WhenDoWeArriveAtBusStop)  
  # For Q3 we need the size of the interval between buses for the interval at 
  # which I arrived at the stop. We have to check whether or not there was a bus before the one I caught that day.
  if (WhichBusICatch>1){
    TimeIntervalBeforeMyBus<-cbind(TimeIntervalBeforeMyBus,TodaysTimes[WhichBusICatch]-TodaysTimes[WhichBusICatch-1])
    TimeBeforeMyBus<-cbind(TimeBeforeMyBus,10-TodaysTimes[WhichBusICatch-1])
  }else{
     TimeIntervalBeforeMyBus<-cbind(TimeIntervalBeforeMyBus,TodaysTimes[WhichBusICatch]-0)
     TimeBeforeMyBus<-cbind(TimeBeforeMyBus,TodaysTimes[WhichBusICatch]-0)
  }
  
  # For Q2 and Q4(which are just different ways of asking the same thing) we just want the average time between buses
  HowManyArrived<-length(TodaysTimes)
  if (HowManyArrived>1){
    AverageTimesBetweenBuses<-cbind(AverageTimesBetweenBuses,(TodaysTimes[HowManyArrived]-TodaysTimes[1])/(HowManyArrived-1))
  }
}
```

So what have we got? We use R's 'summary' command to get an overview.

```r
summary(TimeIWaited[,])   # Q1
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.001976 0.293240 0.723762 1.008042 1.421349 6.687339
```

```r
summary(AverageTimesBetweenBuses[,]) # Q2 and Q4
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.5842  0.8474  0.9609  1.0072  1.1166  2.3143
```

```r
summary(TimeBeforeMyBus[,]) # Q3
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000299 0.309233 0.681337 1.015196 1.417848 6.135150
```

```r
summary(TimeIntervalBeforeMyBus[,])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.08737 0.98574 1.72360 2.02324 2.73501 7.85864
```
The first three means (Q1, Q2 and Q4) are consistent with the times being expo(1) distributed. 
The last mean is not consistent with that. We'll see what it is later.

Let's make some plots to see whether the distributions look exponential.

```r
#install.packages("ggplot2")
library(ggplot2) # a nicer graphics library for R
#library(grid)
library(gridExtra)  # to allow us to arrange our plots in a grid
#install.packages(cowplot)
library(cowplot)
```

```
## 
## ********************************************************
```

```
## Note: As of version 1.0.0, cowplot does not change the
```

```
##   default ggplot2 theme anymore. To recover the previous
```

```
##   behavior, execute:
##   theme_set(theme_cowplot())
```

```
## ********************************************************
```

```r
plot1<-qplot(AverageTimesBetweenBuses[1,], geom="histogram",fill=I("black"),col=I("blue"),binwidth=0.1,center=0.05)
# the 'binwidth' and 'center' commands control where there bins are drawn
plot2<-qplot(TimeIWaited[1,],geom="histogram",fill=I("black"),col=I("blue"),binwidth=0.4,center=0.2)
plot3<-qplot(TimeIntervalBeforeMyBus[1,], geom="histogram",fill=I("black"),col=I("blue"),binwidth=0.5,center=0.25)
plot4<-qplot(TimeBeforeMyBus[1,],geom="histogram",fill=I("black"),col=I("blue"),binwidth=0.4,center=0.2)
MyLabels<-c("Q1 - Daily average time between buses","Q1 - distn of time I waited for my bus","Q3 - distn of time between buses","Q3b - distn of time since last bus")
plot_grid(plot1, plot2, plot3,plot4, labels = MyLabels,ncol = 2, align = 'v',label_size = 8)
```

![](BusWaitingTimes_files/figure-html/plottinglibraries-1.png)<!-- -->
It would be nice to reduce the font size on the figure titles, but I don't know how to do that. Can anyone help with that?

The claim is that the time I wait is ~expo(1), because of the memoryless property of the exponential distribution, so let's check that using a QQ plot.

```r
NumberOfPoints<-length(TimeIWaited[1,])
Quantiles<-seq(from=0.5/NumberOfPoints,to=1-0.5/NumberOfPoints,1/NumberOfPoints)
qqplot(qexp(Quantiles,rate=Lambda),TimeIWaited[1,],main="Checking for expo(Lambda")
abline(a=0,b=1,col="red")
```

![](BusWaitingTimes_files/figure-html/ExpoCheck-1.png)<!-- -->
The plot lands on the diagonal, so it looks like those times are indeed ~expo(Lambda). 
But the interval between the bus I caught and the one before it is NOT distributed as an expo(1), because of size-biased sampling. In fct, it is distributed as expo(1)+expo(1). (Why is that?), which we check below

```r
NumberOfPoints<-length(TimeIntervalBeforeMyBus[1,])
Expo1<-rexp(n=NumberOfPoints,rate=1)
Expo2<-rexp(n=NumberOfPoints,rate=1)
Expo3<-Expo1+Expo2 # this will add the vectors 'pointwise'
qqplot(Expo3,TimeIntervalBeforeMyBus[1,],main="Comparing to expo(Lambda)+expo(Lambda)")
abline(a=0,b=1,col="red")
```

![](BusWaitingTimes_files/figure-html/SizeBiasedCheck-1.png)<!-- -->
This effect is known as 'size-biased' sampling. It results form the fact that we aremore likely to arrive at the bus stop during a large interval (between buses) than during a short interval. Thus, we do not sample intervals 'at random', but instead sample them with a probability that depends upon the size (in this case the ampling prob. is directly proportional to the length of the interval.)
