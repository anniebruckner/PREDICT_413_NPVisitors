# Andrea Bruckner
# Predict 413: Midterm
# Forecasts using Mt. Rainier NP Visitor Data

install.packages("forecast")
library(forecast)
install.packages("TTR")
library(TTR)
install.packages("fpp")
library(fpp)

## fpp package allows xreg -- use this next time since it's more flexible!

options(scipen = 10)

# Read in NP visitors file
NPall <- read.csv(file.path("/Users/annie/Desktop/Northwestern/PREDICT_413/Midterm","CLEAN_NP.csv"),sep=",")

# Get overview of data
summary(NPall) 
str(NPall)
head(NPall)
class(NPall)
names(NPall)

# Get rid of variables I won't use at all
NPall$Year.Month <- NULL
NPall$NonRecreation.Visitors <- NULL # summary showed Min as -112, which is a typo
NPall$Concession.Camping <- NULL
NPall$Concession.Lodging <- NULL
NPall$Misc.Campers <- NULL
NPall$TotalOvernightStays <- NULL

str(NPall)
head(NPall)

# Convert Date to Date class
NPall$Date <- as.Date(as.factor(NPall$Date),format="%Y-%m-%d")
NPall$Month <- as.character(as.factor(NPall$Month))
#NPall$Year <- as.Date(as.factor(NPall$Year),format="%Y") # This wasn't working...
str(NPall)

# Shorten variable names () 
NPall$Visitors <- NPall$Recreation.Visitors
NPall$Campers <- NPall$Tent.Campers
NPall$RVers <- NPall$RV.Campers
NPall$Backpackers <- NPall$Backcountry.Campers

# Drop duplicate variables
NPall$Recreation.Visitors <- NULL
NPall$Tent.Campers <- NULL
NPall$RV.Campers <- NULL
NPall$Backcountry.Campers <- NULL

str(NPall)

# Read in NP weather file
weather <- read.csv(file.path("/Users/annie/Desktop/Northwestern/PREDICT_413/Midterm","Weather_Rainier_Edited.csv"),sep=",")

# Get overview of data
summary(weather)
str(weather)
head(weather)
class(weather)
names(weather)

# Convert Date to Date class
weather$DATE <- as.Date(as.factor(weather$DATE),format="%Y%m%d")
str(weather)

weather$DATE
nrow(weather) # The data frame is missing 7 rows

# Missing the following dates:
# 1987-12-01
# 1994-04-01
# 1994-06-01
# 1996-01-01
# 1996-11-01
# 1997-07-01
# 2003-04-01

# Shorten variable names 
weather$Rain <- weather$TPCP
weather$Snow <- weather$TSNW
str(weather)

# Create new data frame with just the variables I'll use
NPweather <- data.frame(weather$DATE, weather$Rain, weather$Snow)

names(NPweather)

# Shorten variable names
NPweather$Date <- NPweather$weather.DATE
NPweather$Rain <- NPweather$weather.Rain
NPweather$Snow <- NPweather$weather.Snow

# Remove duplicate columns
NPweather$weather.DATE <- NULL
NPweather$weather.Rain <- NULL
NPweather$weather.Snow <- NULL

head(NPweather)

# Add missing date rows -- one by one method
# NPweather <- rbind(NPweather, data.frame("Date"="1987-12-01", "Rain"= 999, "Snow"=999))

# Add missing date rows -- multiple rows method                                         
missingDates <- data.frame(Date = c("1987-12-01","1994-04-01","1994-06-01","1996-01-01","1996-11-01","1997-07-01","2003-04-01"), Rain = c(999,999,999,999,999,999,999), Snow = c(999,999,999,999,999,999,999))

### Later I need to impute better values for the missing dates, as well as fix the Min value for Snow since it's -9999.00

# Join the missing dates to the NPweather dataframe
NPweather <- rbind(NPweather, missingDates)

nrow(NPweather)
str(NPweather) # confirms Date is still in Date format and Rain and Snow are still in Number format

# Sort by the Date
NPweatherSort <- NPweather[order(as.Date(NPweather$Date,format="%Y-%m-%d")),]
head(NPweatherSort)

NPboth <- merge(NPall,NPweatherSort,by="Date")
NPboth
nrow(NPboth)

# Add Flag column for imputed values
NPboth$Flag <- 0
str(NPboth)

# Write to CSV file so I can easily compute the imputed values
# write.csv(NPboth, file = "NPboth.csv")

# Fix Rain and Snow values by imputing means for the same month the year before and the year after
# Change Flag column value to 1 for imputed rows
NPboth[440,9] = 0 # 2015-08-01 shows -9999.00 for Snow
NPboth[440,10] = 1
NPboth[108,8] = 11.19
NPboth[108,9] = 86.8
NPboth[108,10] = 1
NPboth[184,8] = 9.87
NPboth[184,9] = 82.1
NPboth[184,10] = 1 
NPboth[186,8] = 5.74
NPboth[186,9] = 10.5
NPboth[186,10] = 1
NPboth[205,8] = 18.15
NPboth[205,9] = 98
NPboth[205,10] = 1 
NPboth[215,8] = 22.87
NPboth[215,9] = 70.8
NPboth[215,10] = 1 
NPboth[223,8] = 2.31
NPboth[223,9] = 0
NPboth[223,10] = 1 
NPboth[292,8] = 4.73
NPboth[292,9] = 31
NPboth[292,10] = 1 

NPboth
summary(NPboth)

# EDA of relationships between main variables
plot(Visitors ~ Date, data=NPboth)
plot(Backpackers ~ Date, data=NPboth)
plot(Rain ~ Date, data=NPboth)
plot(Snow ~ Date, data=NPboth)

# Since there are no backpackers in the winter, I'll focus on just Visitors
plot(Visitors ~ Rain, data=NPboth)
plot(Visitors ~ Snow, data=NPboth)

min(NPboth$Visitors) # 663
max(NPboth$Visitors) # 409106

# Subset Data by Months for Peak, Shoulder, and Low Seasons
peak <- subset(NPboth, Month == "July" | Month == "August", select = c("Date", "Visitors", "Rain", "Snow"))
shoulder <- subset(NPboth, Month == "May" | Month == "June" | Month == "September" | Month == "October", select = c("Date", "Visitors", "Rain", "Snow"))
low <- subset(NPboth, Month == "January" | Month == "February" | Month == "March" | Month == "April" | Month == "November" | Month == "December", select = c("Date", "Visitors", "Rain", "Snow"))

# Confirm subsets worked--yep!
head(peak)
head(shoulder)
head(low)

# Create duplicate of NPboth without Month, Year, Date, and Flag columns to make it cleaner for TS data
NPboth2 <- NPboth
str(NPboth2)
NPboth2$Month <- NULL
NPboth2$Year <- NULL
NPboth2$Date <- NULL
NPboth2$Flag <- NULL
str(NPboth2)

# Convert to Time Series object
NPTS <- ts(NPboth2, frequency=12, start=c(1979,1))
head(NPTS)

# Create time series plot of all the data
plot.ts(NPTS, main="Mt. Rainier NP Data, Jan 1979-Dec 2015")

par(mfrow=c(1,1))

# Shorten name for key variable
Visitors <- NPboth2$Visitors
head(Visitors)

# Create TS and plot for Visitors
VisitorsTS <- ts(Visitors, frequency=12, start=c(1979,1))
plot(VisitorsTS, main="Monthly Recreation Visitors to Mt. Rainier NP \n (January 1979 to December 2015)",
     xlab="Year", ylab="Number of Visitors")

par(mfrow=c(1,2))
# Season Plot -- reveals bell shape
seasonplot(VisitorsTS,ylab="Number of Visitors", xlab="Month", 
           main="",
           year.labels=FALSE, year.labels.left=FALSE, col=1:20, pch=20)

# Month Plot -- reveals the biggest variation occurs in July and August, the peak months
monthplot(VisitorsTS,ylab="Number of Visitors",xlab="Month",xaxt="n",
          main="")
axis(1,at=1:12,labels=month.abb,cex=0.8)

# Seasonal Deviation Plot: \n Monthly recreation visitors to Mt. Rainier NP \n (January 1979–December 2015)
Seasonal Plot: \n Monthly Recreation Visitors to Mt. Rainier NP \n (January 1979–December 2015)

par(mfrow=c(1,1))
## seasonplot and monthplot don't work if you use as.ts()

# ACF plots -- show a seasonality pattern
acf(Visitors, lag.max=24)
acf(Visitors, lag.max=24, plot=FALSE) # produces autocorrelation values

# Create models + plots of Visitors---NONE work well as LMs since the data is seasonal
VDfit <- lm(Visitors ~ Date, data=NPboth)
plot(Visitors ~ Date, xlab="Date",ylab="Visitors",main="Monthly Visitors", data=NPboth)
lines(Visitors~date,col="gray")
abline(VDfit, col="orange", lwd=2)
#lines(loess.smooth(Date,Visitors), col="red", lty=1, lwd=2)
summary(VDfit)

# This shows the relationship between Visitors and Rain (a very predictable relationship)
#v <- ts(NPboth$Visitors, frequency=12, start=c(1979,1))
#r <- ts(NPboth$Rain, frequency=12, start=c(1979,1))

# Plot Regression line and generate regression summary
#fit <- tslm(v~r, data=NPboth)
#plot(v~r, xlab="Rain",ylab="Visitors",main="Number of Visitors Given Rain",pch=20)
#abline(fit, col="red")
#summary(fit) 

# Create Test Train Sets
train <- ts(Visitors[1:372],frequency=12, start=c(1979,1)) # represents 1979 to 2009 data
test <- ts(Visitors[373:444],frequency=12, start=c(2010,1)) # represents 2010 to 2015 data

# Create Test Train Sets NPTS -- this only pulls in the Visitor data...how can I get the weather data in there too?
#trainNPTS <- ts(NPTS[1:372],frequency=12, start=c(1979,1)) # represents 1979 to 2009 data
#testNPTS <- ts(NPTS[373:444],frequency=12, start=c(2010,1)) # represents 2010 to 2015 data

# MODEL: Seasonal Naive Model -- can use stlm from fpp package
VisitorsSnaive <- snaive(train, h=72, level=c(80,90))
VisitorsSnaive

snf <- forecast(VisitorsSnaive, h=12) # forecast 1 year
plot(snf)
snf # confidence intervals
acc1 <- accuracy(snf,test)
acc1
snacc <- accuracy(snf)  # accuracy for snaive
snacc

snf2 <- forecast(VisitorsSnaive, h=24) # forecast 2 years
plot(snf2)
snf2 # confidence intervals
acc2 <- accuracy(snf2,test)
acc2

snf3 <- forecast(VisitorsSnaive, h=36) # forecast 3 years
plot(snf3)
snf3 # confidence intervals
acc3 <- accuracy(snf3,test)
acc3

snf4 <- forecast(VisitorsSnaive, h=48) # forecast 4 years
plot(snf4)
snf4 # confidence intervals
acc4 <- accuracy(snf4,test)
acc4

snf5 <- forecast(VisitorsSnaive, h=60) # forecast 5 years
plot(snf5)
snf5 # confidence intervals
acc5 <- accuracy(snf5,test)
acc5

snf6 <- forecast(VisitorsSnaive, h=72) # forecast 6 years
plot(snf6, fcol = "deepskyblue", flwd = 3, main = "Seasonal Naive MRNP Recreation Visitors Forecasts", ylab = "Number of Visitors", xlab = "Year")
lines(test,col="red", lty = 2)
legend("bottomleft", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
snf6 # confidence intervals
acc6 <- accuracy(snf6,test)
acc6


# All Seasonal Naive Forecast Confidence Intervals
#snf
#snf2
#snf3
#snf4
#snf6
snf6 # just need to print this one since it shows snf-snf5 too

# All Seasonal Naive Forecast Accuracy Statistics
acc1
acc2
acc3
acc4
acc5
acc6

# Seasonal Naive Residuals
par(mfrow=c(1,2))
residsnf <- residuals(VisitorsSnaive)
plot(residsnf, main="Residuals", type="p")
abline(0,0, col="gray")
hist(residsnf, main="Histogram of Residuals")

# MODEL: Multiple Regression Model
fitLM <- tslm(train~trend+season)
summary(fitLM)

lmf <- forecast(fitLM, h=12, level=c(80,90)) # forecast 1 year
plot(lmf)
lmf # confidence intervals
lmacc <- accuracy(lmf,test)
lmacc

lmf2 <- forecast(fitLM, h=24, level=c(80,90)) # forecast 2 years
plot(lmf2)
lmf2 # confidence intervals
lmacc2 <- accuracy(lmf2,test)
lmacc2

lmf3 <- forecast(fitLM, h=36, level=c(80,90)) # forecast 3 years
plot(lmf3)
lmf3 # confidence intervals
lmacc3 <- accuracy(lmf3,test)
lmacc3

lmf4 <- forecast(fitLM, h=48, level=c(80,90)) # forecast 4 years
plot(lmf4)
lmf4 # confidence intervals
lmacc4 <- accuracy(lmf4,test)
lmacc4

lmf5 <- forecast(fitLM, h=60, level=c(80,90)) # forecast 5 years
plot(lmf5)
lmf5 # confidence intervals
lmacc5 <- accuracy(lmf5,test)
lmacc5

lmf6 <- forecast(fitLM, h=72, level=c(80,90)) # forecast 6 years
plot(lmf6, fcol = "deepskyblue", flwd = 3, main = "Multiple Linear Regression MRNP Recreation Visitors Forecasts", ylab = "Number of Visitors", xlab = "Year")
lines(test,col="red", lty = 2)
legend("bottomleft", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
lmf6 # confidence intervals
lmacc6 <- accuracy(lmf6,test)
lmacc6

# All Linear Regression Forecast Confidence Intervals
lmf6 # just need to print this one since it shows lmf-lmf6 too

# All Seasonal Naive Forecast Accuracy Statistics
lmacc1
lmacc2
lmacc3
lmacc4
lmacc5
lmacc6

# Linear Regression Residuals
par(mfrow=c(1,2))
residlm <- residuals(fitLM)
plot(residlm, main="Residuals", type="p")
abline(0,0, col="gray")
hist(residlm, main="Histogram of Residuals")




### CODE BELOW IS FOR FURTHER EXPLORATION LATER ON ###





# MODEL: ETS AAA Model -- an additive model
VisitorsETS <- ets(train,model="AAA")
fe <- forecast(VisitorsETS,h=12)
fe
acc2<-accuracy(fe,test[1:6])
plot(fe,main="AAA")

fe2 <- forecast(VisitorsETS,h=24)
acc2<-accuracy(fe2,test[1:6])
plot(fe2,main="AAA")

fe3 <- forecast(VisitorsETS,h=36)
acc3<-accuracy(fe3,test[1:6])
plot(fe3,main="AAA")

fe4 <- forecast(VisitorsETS,h=48)
acc24<-accuracy(fe4,test[1:6])
plot(fe4,main="AAA")

fe5 <- forecast(VisitorsETS,h=60)
acc5<-accuracy(fe5,test[1:6])
plot(fe5,main="AAA")

# Decompose VisitorsTS
VisitorsTScomponents <- decompose(VisitorsTS)

VisitorsTScomponents$seasonal
# February has lowest seasonal factor (trough of visitors)
# August has highest seasonal factor (peak of visitors)
VisitorsTScomponents$trend
VisitorsTScomponents$random

plot(VisitorsTScomponents)
# Per the trend plot, the popularity of Mt. Rainier NP peaked in the early 1990s
# It generally decreased in popularity until the late 2000s, and is now showing an increasing trend between 2010 and 2015

plot(VisitorsTScomponents$figure)
# Anything above or below 1 shows seasonality--everything is seasonal


# Other plots to show that SNaive performs better than some other methods
## ALL show negative numbers for the lower bound of the confidence intervals!
h=12 # To forecast next year of data (2016 visitors)
meanf(VisitorsTS, h) 
naive(VisitorsTS, h) 
snaive(VisitorsTS, h) # The forecasted number of visitors changes per month
rwf(VisitorsTS, h)
rwf(VisitorsTS, h, drift=TRUE)

VisitorsTS2 <- window(VisitorsTS,start=1979,end=2015)
NPfit1 <- meanf(VisitorsTS2, h)
NPfit2 <- naive(VisitorsTS2, h)
NPfit3 <- snaive(VisitorsTS2, h)
NPfit4 <- rwf(VisitorsTS2, h, drift=TRUE)

plot(NPfit1, plot.conf=FALSE, 
     main="Forecasts for 2016 monthly recreation visitors to Mr. Rainier NP")
lines(NPfit2$mean,col=2)
lines(NPfit3$mean,col=3)
lines(NPfit4$mean,col=8)
legend("topright",lty=1,col=c(4,2,3,8),
       legend=c("Mean method","Naive method","Seasonal naive method", "Drift method"))
# The Naive and Drift lines overlap--SNaive is most accurate












# MODEL: Holt Winters Model
VisitorsTSHolt <- HoltWinters(VisitorsTS, beta=FALSE, gamma=FALSE)
VisitorsTSHolt

VisitorsTSHolt$fitted
plot(VisitorsTSHolt)

VisitorsTSHolt$SSE # The SSE is super high

VisitorsTSHolt2016 <- forecast.HoltWinters(VisitorsTSHolt, h=12)
VisitorsTSHolt2016
# I don't understant why the Lo's are HUGE negatives

# HELP: The prediction interval is HUGE, into the negatives. Why are the predictions negative?
plot.forecast(VisitorsTSHolt2016)
# The Point Forecast is the coefficient from HoltWinters()














### SCRAP CODE ###

# HELP: Is it possible to make September forecasts without creating a new model?
# Create Test Train Sets - September Forecasts
SeptvisitorsTrain <- NPboth[1:372,] # represents 1979 to 2009 September data
SeptvisitorsTest <- NPboth[373:444,] # represents 2010 to 2015 September data
SeptvisitorsFit <- lm(Visitors ~ Date, data = NPbothSept)
SeptvisitorsForecast <- plot(forecast(SeptvisitorsFit, newdata = SeptvisitorsTest[,"Date"]))
lines(Visitors ~ Date, data = NPbothSept, type = "p")
lines(NPboth$Visitors)
SeptvisitorsForecast # shows all the prediction intervals



## By default, HoltWinters() just makes forecasts for the same time period covered by our original time series.

# The high alpha (0.9999339) indicates that the estimate is based mostly on very recent observations in the time series
## Source: http://www.quantlego.com/howto/holt-winters-smoothing-and-forecast/

## Alpha is the smoothing parameter used for the estimation of the current underlying level.
## Value of alpha closer to 1 indicates that more weight is on the recent value of the independent variable 
## and less on the previous data. And a value closer to zero is a forerunner that the model has put more
## weight on the past data for exponential smoothing.

## Source: http://www.tatvic.com/blog/forecasting-the-number-of-visitors-on-your-website-using-r-part-ii/


# fitLM <- lm(Visitors ~ Date, data = NPboth)
# fitLMForecast <- plot(forecast(fitLM, newdata = test[1:12]))
# lines(Visitors ~ Date, data = NPboth, type = "p")
# lines(NPboth$Visitors)
# fitLMForecast # shows all the prediction intervals



# Create Test Train Sets (not in TS) for LM
lmTrain <- NPboth[1:372,] # represents 1979 to 2009 data
lmTest <- NPboth[373:444,] # represents 2010 to 2015 data

# Simple linear model -- doesn't work well since data is seasonal
lmFit <- lm(Visitors ~ Date, data = NPboth)
lmForecast <- plot(forecast(lmFit, newdata = lmTest[,"Date"]))
lmForecast # shows all the prediction intervals
lines(Visitors ~ Date, data = NPboth, type = "p")
abline(lmFit, col="orange", lwd=2)
summary(lmFit)


NPbothSept <- subset(NPboth, Month == "September")
NPbothSept


# Create Test Train Sets - September Forecasts
SeptvisitorsTrain <- NPboth[1:372,] # represents 1979 to 2009 September data
SeptvisitorsTest <- NPboth[373:444,] # represents 2010 to 2015 September data
SeptvisitorsFit <- lm(Visitors ~ Date, data = NPbothSept)
SeptvisitorsForecast <- plot(forecast(SeptvisitorsFit, newdata = SeptvisitorsTest[,"Date"]))
lines(Visitors ~ Date, data = NPbothSept, type = "p")
lines(NPboth$Visitors)
SeptvisitorsForecast # shows all the prediction intervals


accuracy(NPfit1, VisitorsTS) # meanf
accuracy(NPfit2, VisitorsTS) # naive
accuracy(NPfit3, VisitorsTS) # snaive -- by far the lowest RMSE and MAE
accuracy(NPfit4, VisitorsTS) # rwf




# I can't figure out how to call specific variables in NPTS, so I'm making Visitors a TS object too so I can analyze just it
VisitorsTS <- ts(NPboth2$Visitors, frequency=12, start=c(1979,1))
VisitorsTS


# I can't figure out how to call specific variables in NPTS, so I'm making Visitors a TS object too so I can analyze just it
visitorsTS <- ts(NPboth2$Visitors, frequency=12, start=c(1979,1))
visitorsTS

VisitorsTSlag <- window(VisitorsTS, start=1979, end=2015)
lag.plot(VisitorsTSlag, lags=12, do.lines=FALSE)
# The data is clearly autocorrelated since it's seasonal, but what do the specific lag scatter plots mean?
## Correlation of a time series with its own past and future values- is called Autocorrelation.  It is also referred as “lagged or series correlation”. 
## Source: http://www.gaussianwaves.com/2014/06/autocorrelation-correlogram-and-persistence-time-series-analysis/


# 
h=12 # To forecast next year of data (2016 visitors)
meanf(VisitorsTS, h) 
naive(VisitorsTS, h) 
snaive(VisitorsTS, h)
rwf(VisitorsTS, h)
rwf(VisitorsTS, h, drift=TRUE)

VisitorsTS2 <- window(VisitorsTS,start=1979,end=2015)
NPfit1 <- meanf(VisitorsTS2, h)
NPfit2 <- naive(VisitorsTS2, h)
NPfit3 <- snaive(VisitorsTS2, h)
NPfit4 <- rwf(VisitorsTS2, h, drift=TRUE)

plot(NPfit1, plot.conf=FALSE, 
     main="Forecasts for 2016 monthly recreation visitors to Mr. Rainier NP")
lines(NPfit2$mean,col=2)
lines(NPfit3$mean,col=3)
lines(NPfit4$mean,col=8)
legend("topright",lty=1,col=c(4,2,3,8),
       legend=c("Mean method","Naive method","Seasonal naive method", "Drift method"))
# The Naive and Drift lines overlap

VisitorsTS3 <- window(VisitorsTS, start=2015)
accuracy(NPfit1, VisitorsTS)
accuracy(NPfit2, VisitorsTS)
accuracy(NPfit3, VisitorsTS)
accuracy(NPfit4, VisitorsTS)

# Create Test Train Sets
visitorsTrain <- NPTS[1:372,] # represents 1979 to 2009 data
visitorsTest <- NPTS[373:444,] # represents 2010 to 2015 data
visitorsFitLM <- lm(Visitors ~ Date, data = NPboth)
visitorsFitTS <- tslm(Visitors ~ trend + season, data = NPTS)
visitorsForecast <- plot(forecast(visitorsFitLM, newdata = visitorsTest[,"Date"]))
lines(Visitors ~ Date, data = NPTS, type = "p")
lines(NPboth$Visitors)
visitorsForecast # shows all the prediction intervals


monthOrder <- subset(NPboth, select = c("Date", "Month", "Visitors", "Rain", "Snow"))
head(monthOrder)

monthOrder <- factor(monthOrder, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


boxplot(Visitors ~ Month, data=NPboth,
        
        main="Priceline July 2014 to June 2016",
        
        xlab="Days",ylab="Adjusted Close Price")

