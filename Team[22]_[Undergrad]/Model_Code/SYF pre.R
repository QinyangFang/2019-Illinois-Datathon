setwd("C:/Users/pendr/Desktop/Datathon/Data/Data Given")
library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);

df <- read.csv("SYF.csv")

#Closing Price
stock_prices = df$Close
plot.ts(stock_prices)
plot.ts(log(stock_prices))
# Compute the log returns for the stock
stock = diff(log(stock_prices),lag=1)
stock = stock[!is.na(stock)]

# Plot log returns 
plot(stock,type='l', main='log returns plot')
# Conduct ADF test on log returns series
print(adf.test(stock))

# Split the dataset in two parts - training and testing
breakpoint = floor(nrow(stock)*(2.9/3))

# Apply the ACF and PACF functions
par(mfrow = c(1,1))
acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=30)
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=30)

auto.arima(log(stock_prices),trace=T)

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())
arima1 <- arima(stock_prices, order=c(0,1,0), seasonal=list(order=c(0,1,0)),method="ML")
airforecast <- forecast(arima1,h=60,level=c(80,95))

plot(airforecast, df, ylim=c(0,50))
axis.Date(1,at=seq(airforecast[0],airforecast[9]),length.out=20,format="%Y-%m",las=2)
airforecast