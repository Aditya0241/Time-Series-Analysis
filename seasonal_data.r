library(TSA)
library(tidyverse)
library(tseries)
data=read.csv("C:/Users/Aditya Reddy/Desktop/Stevens/MA-641/Retail Sales.csv")
summary(data)
data=na.omit(data)
data=data$Sales
plot(ts(data,frequency=12,start=(1964)),ylab='Monthly Sales',lwd=2,main='Time Series plot')

adf.test(data)
# Not stationary since the calculated p-value is not less than 
# critical p-value i.e 0.05 
# So we now try to make it stationary by making one seasonal difference

difference_data=diff(data,lag=12)
seasonal_data=ts(difference_data)
seasonal_data=na.omit(seasonal_data)
seasonal_data=ts(seasonal_data,frequency = 12)
plot(seasonal_data,ylab='Monthly Sales',lwd=2,main='seasonal_data')

acf(seasonal_data,lag.max=100,lwd=2)
pacf(seasonal_data,lag.max=100,lwd=2)
adf.test(seasonal_data)
eacf(seasonal_data)

#Looking at the eacf plot
#ARIMA(0,1)
#ARIMA(1,1)
#ARIMA(1,0)


#Models of Time Series
#Because the series is seasonal, SARIMA (Seasonal ARIMA) will be used instead of ARIMA.
#From ACF and PACF plot chose below models fit to my data:
#Model1: SARIMA(1,0,0)(1,1,0)[12]
#Model2: SARIMA(1,0,0)(1,1,1)[12]

#Since the data is monthly data, seasonality is 12.
model1<-arima(seasonal_data,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12))
print(model1)

model2<-arima(seasonal_data,order=c(1,0,0),seasonal=list(order=c(1,0,1),period=12))
print(model2)

model3<-arima(seasonal_data,order=c(1,0,1),seasonal = list(order=c(1,0,0),period=12))

#Comparing the above models based on AIC values:
#Model 1 has the least AIC values

# Residual Analysis:
# The “residuals” in a time series model are what is left over after fitting 
# a model. 
# Residuals are useful fordetermining if a model has captured all of the data's 
# information. 
# A good forecasting method will yieldresiduals with the following properties:
# 1) The residuals are uncorrelated.
# 2) The residuals have zero mean.
# We plot sample ACF and PACF to check if there's any correlation in residuals 
# and normality of theresiduals is checked using histogram and qq plots.

residuals_model1 = model1$residuals
residuals_model2 = model2$residuals
residuals_model3 = model3$residuals

#Plotting the residuals:
# For model1:
plot(residuals_model1,type='o',lwd=2)
acf(as.vector(residuals_model1),lag.max=50,lwd=2)
pacf(as.vector(residuals_model1),lag.max=50,lwd=2)

qqnorm(residuals_model1)
qqline(residuals_model1)

hist(residuals_model1)
lb_test1<-Box.test(residuals_model1,type='Ljung-Box',lag=20)
print(lb_test1)
print(shapiro.test(residuals_model1))
tsdiag(model1)

# For model2:
plot(residuals_model2,type='o',lwd=2)
acf(as.vector(residuals_model2),lag.max=50,lwd=2)
pacf(as.vector(residuals_model2),lag.max=50,lwd=2)

qqnorm(residuals_model2)
qqline(residuals_model2)

hist(residuals_model2)
lb_test2<-Box.test(residuals_model2,type='Ljung-Box',lag=20)
print(lb_test2)
print(shapiro.test(residuals_model2))
tsdiag(model2)

# For model3:
plot(residuals_model3,type='o',lwd=2)
acf(as.vector(residuals_model3),lag.max=50,lwd=2)
pacf(as.vector(residuals_model3),lag.max=50,lwd=2)

qqnorm(residuals_model3)
qqline(residuals_model3)

hist(residuals_model3)
lb_test3<-Box.test(residuals_model3,type='Ljung-Box',lag=20)
print(lb_test3)
tsdiag(model3)


#Forcasting
library(forecast)
forecast_model1<-arima(ts(data,frequency=12,start=(1964)),order=c(1,1,0),seasonal = c(1,1,0))
forecast_model1$x<-ts(data,frequency=12,start = c(1964))
forecast_model1$x

forecast_values<-forecast(forecast_model1,level = c(95),h=12)
forecast_values

plot(forecast_values,lwd=2)





