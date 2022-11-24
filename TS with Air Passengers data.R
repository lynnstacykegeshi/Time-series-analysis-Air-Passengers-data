# Time series analysis using the 
#AirPassengers data in R
library(xts)
library(tseries)
library(forecast)
library(seastests)
library(FinTS)

#Get the data
air <- AirPassengers
str(air)#attributes of data
start(air)
end(air)

#Observe the trend of the TS
dev.new()   #to have the plot outside R studio
plot(air)

#Box plot to check on peak months during the period
boxplot(air~cycle(air))

#Plot mean of each year
plot(aggregate(air, FUN=mean))

#Decompose to different components
air_dec <- decompose(air, "multiplicative")
air_dec

# Graph of random, trend and seasonality in one plot
plot(air_dec)
plot(air_dec$figure,
     type="b",
     xlab="Months",
     ylab="Seasonality Index",
     col="blue",
     las= 1 )

#######
#log transform
air_add<-log(air)
plot(air_add)

#Decompose additive
air_dec_add <- decompose(air_add, "additive")#you can omit additive since the default of decompose function is additive
plot(air_dec_add)

plot(air_dec_add$seasonal,
     type="b",
     xlab="Months",
     ylab="Seasonality Index",
     col="blue",
     las= 2)

#Proof the Air Passengers data follows an multiplicative model
new_air<-air_dec$seasonal*air_dec$trend*air_dec$random
plot(new_air)
lines(air,col="#FFA500")



#######
#Plot the original data ACF and PACF
ggtsdisplay(air)

#Differencing to make data stationary
no_diff <- ndiffs(air)

#Seasonality in the data
isSeasonal(air, test="wo")
no_diff_seas <- nsdiffs(air)
#Functions to estimate the number of differences required to make a given time series stationary. nsdiffs estimates the number of seasonal differences necessary

##Test for stationarity
# If p<5%, adf (non-stationary) and kpss test (stationary)
#Augmented Dickey Fuller Test is a unit root test, null hypothesis-non stationary and alternative hypothesis is stationary.
#Weakness is you need to specify lag, if you don't specify lag results might be misleading
adf.test(air)
#Results: p-values is 0.01. 0.01<0.05, reject null hypothesis and conclude that series is stationary

#####
#AFT test while specifying lag we conclude that the TS is non stationary
adf.test(air,k=12)

#KPSS test, the null hypothesis is that the time series is stationary and the alternative hypothesis is that the TS is non stationary.
kpss.test(air)
#results 0.01<0.05, reject null hypothesis and conclude that the data is non stationary


#How to difference data
air_diff <- diff(air)
ggtsdisplay(air_diff)

#deSeasonalize the data
air_seas <- diff(air, lag=12, differences=1)
ggtsdisplay(air_seas)

#Now test if TS is stationary
adf.test(air_diff)
kpss.test(air_diff)
#Results of KPSS test after differencing , p-value=0.1, 0.1>0.05 and hence we fail to reject the null hypothesis and conclude that the data is stationary.
ndiffs(air_diff)

air_diff_dec <- decompose(air_diff, "multiplicative")

plot(air_diff_dec$trend)

#Difference and deseasonalize
diff_seas <- air %>% 
  diff(lag=12) %>% 
  diff()
dev.new()
ggtsdisplay(diff_seas)
