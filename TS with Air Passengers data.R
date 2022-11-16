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
