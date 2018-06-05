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


forestStockPrice <- function(responseStock,
                              covarStock,
                              interTimePeriod,
                              startDate,
                              endDate
                              ){
    ## for debugging
    ##symbol <- 'GOOGL';nTimePeriods <- 10;firstDate = '2015-01-01'


    ## libraries
    library(quantmod)
    library(tseries)
    library(timeSeries)
    library(forecast)
    library(xts)
    ## library(TSA)
    



    stockData <- readDataLag(lagStock=covarStock,predStock=responseStock,interTimePeriod=interTimePeriod,startDate=startDate,endDate=endDate)

    head(stockData)

    
    futureStock <- readStockData(symbol=responseStock,startDate=head(stockData,1)[,'responseDate'])


    ## new covar stock for future predictions
    newCovarStock <- readStockData(symbol=covarStock,startDate=head(stockData,1)[,'covarDate'],Sys.Date()-interTimePeriod)

    futureStock
    newCovarStock

    
    ## Convert stock data to xts 
    stockResponse <- xts(stockData$responsePrice,as.Date(stockData$responseDate))
    stockCovar <- xts(stockData$covarPrice,as.Date(stockData$responseDate))
    
    nCores <- parallel::detectCores()

    t1 <- Sys.time()
    fit <- auto.arima(stockResponse, d = NA, D = NA, max.p = 10, max.q = 10, max.P = 2,
                      max.Q = 2, max.order = 50, max.d = 5, max.D = 5, start.p = 2,
                      start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
                      seasonal = TRUE, ic = c("aicc"), stepwise = FALSE,
                      trace = FALSE, approximation = FALSE,
                      truncate = NULL, xreg = stockCovar, test = c('pp'),
                      seasonal.test = c("seas", "ocsb", "hegy", "ch"), allowdrift = TRUE,
                      allowmean = TRUE, lambda = 'auto', biasadj = FALSE, parallel = TRUE,
                      num.cores = nCores)
    t2 <- Sys.time()
    t2-t1

    summary(fit)



    fCast <- forecast(fit,level=.95,xreg=with(newCovarStock,xts(close,as.Date(timestamp))))

    fCast



    dev.new();plot(fCast)



    out <- list(modelFit=fit,foreCast=fCast)

    return(out)
    
} # end function
