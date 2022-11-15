# Time series analysis using the 
#AirPassengers data in R
library(xts)
library(tseries)
library(forecast)
library(seastests)
library(FinTS)

#Get the data
air <- AirPassengers
start(air)
end(air)

#Observe the trend of the TS
dev.new()   #to have the plot outside R studio
plot(air)
