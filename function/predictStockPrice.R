#-------------------------------------------------------------------------------
# Function for forecasting stock prices
#
# ------------------------------------------------------------------------------

predictStockPrice <- function(symbol = 'MSFT',  # symbol to forecast (defaults to Microsoft "MSFT")
                              nTimePeriods = 10 # time periods to forecast (as of now, it is hard coded for days)
                              )
  {

options(stringsAsFactors = F)


require(quantmod)
require(tseries)
require(timeSeries)
require(forecast)
require(xts)


sym = symbol

datURL <- paste0('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&outputsize=full&apikey=LQY762JM5VDA5ETC&symbol=',sym,'&datatype=csv')

dat <- read.csv(datURL)

# subset to smaller timeframe - everything from 2015 to current
dat <- subset(dat, timestamp > '2015-01-01')

# Convert stock data to xts 
stockPrices <- xts(dat$close,as.Date(dat$timestamp))

# log returns for the stock
stock = diff(log(stockPrices),lag=1)
stock = stock[!is.na(stock)]
stock = stock[!is.infinite(stock)]


# Split the dataset in two parts - training and testing
breakpoint = floor(nrow(stock)*(2.9/3))

# Apply the ACF and PACF functions
# par(mfrow = c(1,1))
# acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
# pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)


# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date(min(dat$timestamp),"%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())

# for loop (training and testing)
for (b in breakpoint:(nrow(stock)-1)) {
  
  stockTrain = stock[1:breakpoint, ]
  stockTest = stock[(b+1):nrow(stock), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters - need to learn more about this.
  # there is also the auto.arima() function
  # fit = arima(stockTrain, order = c(1, 1, 2),include.mean=FALSE)
  fit = auto.arima(stockTrain,ic = 'aic')
  # summary(fit)
  

  # Forecasting the log returns
  arima.forecast = forecast(fit, h = nTimePeriods, level=95)
  # summary(arima.forecast)
  
  # plotting the forecast
  # par(mfrow=c(1,1))
  # plot(arima.forecast, main = "ARIMA Forecast")
  # plot(arima.forecast$mean, main = "ARIMA Forecast")
  
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  # Creating a series of actual returns for the forecasted period
  Actual_return = stock[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  # print(stockPrices[(b+1),])
  # print(stockPrices[(b+2),])
  
}# end for-loop


# Adjust the length of the Actual return series
Actual_series = Actual_series[-1]

# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series,index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
# plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
# lines(forecasted_series,lwd=1.5,col='red')
# legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))
# dev.off()

# Create a table for the accuracy of the forecast
comparison = merge(Actual_series,forecasted_series)
comparison$Accuracy = sign(comparison$Actual_series)==sign(comparison$Forecasted)
# calculate accuracy percentage
Accuracy = sum(comparison$Accuracy == 1)*100/length(comparison$Accuracy)
print(paste0('Forecasting for ',symbol,' is ',round(Accuracy), ' percent accurate')) # forecasting for this stock has a __% chance of being correct



# Forecast actual stock price
fitPrice <- auto.arima(tail(stock,100)) # forecast stock price based on the last 100 obs
# the "Series" portion of this returns the p,d,q parameters for arima modelling


# fitPrice <- arima(tail(stock,100),order = c(1, 1, 2),include.mean=FALSE)
forePrice <- as.data.frame(forecast(fitPrice,h=nTimePeriods))

fp <- data.frame(price=c(as.numeric(tail(stockPrices,1)),2:nTimePeriods))

for(i in 2:10){
  fp$price[i] <- (10^forePrice$`Point Forecast`[i-1])*fp$price[i-1]
}


plot(fp$price,type='l',ylab='stock price',xlab='Day',
     ylim = c(min(fp$price)*0.95,max(fp$price)*1.05),main=paste0('Forecasted ',symbol ,' Stock Price'))
# lines(forePrice$`Lo 80`,type='l',col='red') # lower bound 
# lines(forePrice$`Hi 80`,type='l',col='red') # upper bound 


return(list(Acc = Accuracy,
            logPrices = forePrice,
            estPrices = fp))

} # end function
