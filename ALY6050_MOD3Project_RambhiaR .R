#Module 3 Project
#Time Series using R



#installing the packages
install.packages("quantmod")
install.packages("tidyquant")
install.packages("fpp2")


#importing the libraries
library(quantmod)
library(tidyquant)
library(fpp2)


#handling warnings
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)



#loading the COST stock data
df.cost = getSymbols("COST", from = '2016-01-01',
           to = "2022-01-01",warnings = FALSE,
           auto.assign = TRUE)
df.cost



#details and description about the COST stock data

#displaying the records of the COST stock data
head(COST)
#displaying the class of the COST stock data
class(COST)
#displaying the frequency of the COST stock data
frequency(COST)
#displaying the summary of the COST stock data
summary(COST)
#plotting the COST stock data
plot(COST)
#displaying the cycle of the COST stock data
cycle(COST)


#plotting the chart series for COST stock data
chart_Series(COST['2016-01/2022-01'])
chart_Series(COST['2016-01/2017-01'])



#loading the KO stock data
df.ko = getSymbols("KO", from = '2016-01-01',
           to = "2022-01-01",warnings = FALSE,
           auto.assign = TRUE)

df.ko



#details and description about the KO stock data

#displaying the records of the KO stock data
head(KO)
#displaying the class of the KO stock data
class(KO)
#displaying the frequency of the KO stock data
frequency(KO)
#displaying the summary of the KO stock data
summary(KO)
#plotting the KO stock data
plot(KO)
#displaying the cycle of the KO stock data
cycle(KO)


#plotting the chart series for KO stock data
chart_Series(KO['2016-01/2022-01'])
chart_Series(KO['2016-01/2017-01'])



#moving average for COST
moving_avg_cost <- rollmean(COST$COST.Close,k=5)
moving_avg_cost
plot(moving_avg_cost)



#exponential smoothing for COST
ses.cost <- ses(COST$COST.Close, alpha = .2, h = 100)
ses.cost
p1 <- autoplot(ses.cost)
p1



#Plotting actual prices vs moving average for COST
plot(COST$COST.Close, type = "l", col = "blue", ylab = "Moving Average", xlab = "Actual Prices",
     main = "Actual Prices vs Moving Average")

lines(moving_avg_cost, type = "l", col = "red")



#moving average for KO
moving_avg_ko <- rollmean(KO$KO.Close,k=5)
moving_avg_ko
plot(moving_avg_ko)



#exponential smoothing for KO
ses.ko <- ses(KO$KO.Close, alpha = .2, h = 100)
ses.ko
p2 <- autoplot(ses.ko)
p2



#Plotting actual prices vs moving average for KO
plot(KO$KO.Close, type = "l", col = "blue", ylab = "Moving Average", xlab = "Actual Prices",
     main = "Actual Prices vs Moving Average")

lines(moving_avg_ko, type = "l", col = "red")




#fitting an AR1 
together <- cbind(COST,KO)

#COST stock data
together$COST.Close_0 <- lag(together$COST.Close,0)
together$COST.Close_1 <- lag(together$COST.Close,1)
together$COST.Close_2 <- lag(together$COST.Close,2)
together$COST.Close_3 <- lag(together$COST.Close,3)


#AR(1) for COST stock data
head(together)
summary(lm(data=together, COST.Close_0 ~ COST.Close_1))
summary(lm(data=together, COST.Close_0 ~ COST.Close_1 + COST.Close_2))
summary(lm(data=together, COST.Close_0 ~ COST.Close_1 + COST.Close_2 + COST.Close_3))


#KO stock data
together$KO.Close_0 <- lag(together$KO.Close,0)
together$KO.Close_1 <- lag(together$KO.Close,1)
together$KO.Close_2 <- lag(together$KO.Close,2)
together$KO.Close_3 <- lag(together$KO.Close,3)


#AR(1) for KO stock data
head(together)
summary(lm(data=together, KO.Close_0 ~ KO.Close_1))
summary(lm(data=together, KO.Close_0 ~ KO.Close_1 + KO.Close_2))
summary(lm(data=together, KO.Close_0 ~ KO.Close_1 + KO.Close_2 + KO.Close_3))



#plotting the COST and KO stock data for AR model
cost <- as.ts(together$COST.Close)
ko <- as.ts(together$KO.Close)
plot(cost, col = "red")
par(new=TRUE)
plot(ko, col = "blue")


#plotting the actual prices vs the AR model plot for COST stock data
plot(COST$COST.Close, col="blue")
par(new=TRUE)
plot(cost)


#plotting the actual prices vs the AR model plot for KO stock data
plot(KO$KO.Close, col = "red")
par(new=TRUE)
plot(ko)



#ARIMA model for COST stock data
fit_cost <- auto.arima(cost, trace = TRUE)

futurevalue_cost <- forecast(fit_cost, 10)
print(futurevalue_cost)
plot(futurevalue_cost, main = "Plot with Forecasting for COST stock data")


#ARIMA model for KO stock data
fit_ko <- auto.arima(ko, trace = TRUE)

futurevalue_ko <- forecast(fit_ko, 10)
print(futurevalue_ko)
plot(futurevalue_ko, main = "Plot with Forecasting for KO stock data")



#MODEL
model1 <- lm(data = together, COST.Close_0 ~ KO.Close_1)
cost_data <- predict(model1)
cbind(together, cost_data)




#analysis and forecasting for dry wine data


#loading the data set
dry_wine_data <- read.csv("dry_wine.csv")


#displaying data set details
head(dry_wine_data,20)
summary(dry_wine_data)
frequency(dry_wine_data)


#ARIMA model for dry wine data
fit_dry_wine <- auto.arima(dry_wine_data$Dry_wine, trace = TRUE)

futurevalue_dry_wine <- forecast(fit_dry_wine, 10)
print(futurevalue_dry_wine)
plot(futurevalue_dry_wine, main = "Plot with Forecasting for dry wine data")





