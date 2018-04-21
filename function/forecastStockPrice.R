##-------------------------------------------------------------------------------
## Function for forecasting stock prices
##
## ------------------------------------------------------------------------------


#' @title predictStockPrice
#'
#' @description
#' For a given stock, give a prediction for that stock
#'
#' @param symbol character, the stock symbol. Default is Microsoft 'MSFT'.
#' @param nTimePeriods integer, the number of time periods to make a prediction. Default is 10.
#' @param firstDate character, the first date to use for the stock prediction model. Default is '2015-01-01'. 
#' 
#' @details
#'
#'
#' @export predictStockPrice
#' 


predictStockPrice <- function(symbol = 'MSFT',
                              nTimePeriods = 10,
                              firstDate = '2015-01-01'
                              ){
    ## for debugging
    ##symbol <- 'GOOGL';nTimePeriods <- 10;firstDate = '2015-01-01'

    options(stringsAsFactors = F)
    ## libraries
    ## library(quantmod)
    ## library(tseries)
    ## library(timeSeries)
    ## library(forecast)
    ## library(xts)
    library(TSA)
    
    sym = symbol
    
    datURL <- paste0('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&outputsize=full&apikey=LQY762JM5VDA5ETC&symbol=',sym,'&datatype=csv')
    
    dat <- read.csv(datURL)
    
    ## subset to smaller timeframe - everything from 2015 to current
    dat <- subset(dat, timestamp > firstDate)
    
    ## Convert stock data to xts 
    stockPrices <- xts(dat$close,as.Date(dat$timestamp))
    
    trainStock <- head(stockPrices,nrow(stockPrices)-nTimePeriods)
    testStock <- tail(stockPrices,nTimePeriods)
    dim(testStock)

    tail(trainStock)
    testStock


    auto.arima(trainStock,ic = 'aicc')
    t1 <- Sys.time()
    fit <- auto.arima(trainStock, d = NA, D = NA, max.p = 15, max.q = 15, max.P = 2,
                      max.Q = 2, max.order = 50, max.d = 5, max.D = 1, start.p = 2,
                      start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                      seasonal = TRUE, ic = c("bic"), stepwise = FALSE,
                      trace = FALSE, approximation = FALSE,
                      truncate = NULL, xreg = NULL, test = c('pp'),
                      seasonal.test = c("seas", "ocsb", "hegy", "ch"), allowdrift = TRUE,
                      allowmean = TRUE, lambda = 'auto', biasadj = FALSE, parallel = FALSE,
                      num.cores = 2)
    t2 <- Sys.time()
    t2-t1

fit

    attributes(fit)

    arima(trainStock,order=c(2,0,0))


plot(trainStock)
    
plot(forecast(fit,level=.95,h=nTimePeriods),add=TRUE)


    
} # end function
