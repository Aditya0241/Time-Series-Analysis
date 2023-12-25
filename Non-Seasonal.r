library(TSA)
library(tidyverse)
library(lubridate)
library(xts)
library(quantmod)
library(tseries)
library(rugarch)
library(forecast)
data=read.csv("C:/Users/Aditya Reddy/Desktop/Stevens/MA-641/JPMorgan Chase.csv")
summary(data)
data$Date<-lubridate::as_datetime(data$Date)
data_ts<-xts(as.numeric(data$Close),data$Date)
plot(data_ts)

daily_returns<-dailyReturn(data_ts)
daily_returns<-daily_returns[-1]
plot(daily_returns)

h <- hist(daily_returns, breaks = 80) 
xfit <- seq(min(daily_returns), max(daily_returns), length = 80) 
yfit <- dnorm(xfit, mean = mean(daily_returns), sd = sd(daily_returns)) 
yfit <- yfit * diff(h$mids[1:2]) * length(daily_returns) 
lines(xfit, yfit, col = "black", lwd = 2)

acf(daily_returns,lag.max=40,lwd=2)
pacf(daily_returns,lag.max=40,lwd=2)
eacf(daily_returns)

adf.test(daily_returns)


#Fitting ARMA and GARCH Models
#ARMA(0,1)
#ARMA(1,2)
#ARMA(3,4)


model_1<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 4)),
                    mean.model = list(armaOrder = c(0, 0)),distribution.model = 'norm')

garch_model1 <- ugarchfit(spec = model_1, data = daily_returns,solver='hybrid')
print(garch_model1)
plot(garch_model1,which='all')

model_2<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 4)),
                    mean.model = list(armaOrder = c(0, 0)),distribution.model = 'sstd')

garch_model2 <- ugarchfit(spec = model_2, data = daily_returns)
print(garch_model2)
plot(garch_model2,which='all')

model_3<-ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(3, 4)),
                    mean.model = list(armaOrder = c(0, 0)),distribution.model = 'sstd')
garch_model3 <- ugarchfit(spec = model_3, data = daily_returns)
print(garch_model3)
plot(garch_model3,which='all')

#SIMULATION AND FORECASTING 
#We will be using the GJR GARCH model for the final forecasting

m<-ugarchfit(data=daily_returns,spec=model_3)
sfinal<-model_3
setfixed(sfinal)<-as.list(coef(m))
coef(m)
f2018<-ugarchforecast(data=daily_returns["/2018-12"],fitORspec = sfinal,n.ahead=252)
plot(sigma(f2018))
sim<-ugarchpath(spec = sfinal,m.sim=2,n.sim=1*252,rseed=123)
plot.zoo(fitted(sim),lwd=2)
plot.zoo(sigma(sim),lwd=2)
tail(data$Close)
p<-140.54*apply(fitted(sim),2,'cumsum')+140.54
matplot(p,type='l',lwd=3)

#Conclusion:
#Thus, we have fitted a GRJ-GRACH (3,4) model to get the daily return price 
#of the stocks. The final plot takes the closing price of the stock at the 
#end of 2018 and predicts it for the next entire year. 