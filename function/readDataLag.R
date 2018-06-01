#--------------------------------------------------------------------
# Read and format data for correlated stocks
# 30 May 2018
#--------------------------------------------------------------------

# returns a data frame with date and price for each of the two stocks

readDataLag <- function(lagStock,                  # stock that "predicts" the price of the stock of interest
                        predStock,                 # stock that is lagged against the predictor stock - this is the stock we are potentially interested in buying  
                        interTimePeriod,           # number of days between the predictor stock price (lagStock) and predicted stock price (predStock)
                        startDate = '2017-01-01',  # first date of data to use
                        endDate = '2017-12-31'     # last date of data to use
                        ){

  options(stringsAsFactors = F)
  # read data for the stock that predicts price of the other
  predDat <- read.csv(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&outputsize=full&apikey=LQY762JM5VDA5ETC&symbol=",predStock,"&datatype=csv"))
  # read data for the stock that is lagged against the predictor stock
  lagDat <- read.csv(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&outputsize=full&apikey=LQY762JM5VDA5ETC&symbol=",lagStock,"&datatype=csv"))
  
  # make the timestamp column a date so we can use it to subset
  predDat$date <- as.Date(predDat$timestamp)
  lagDat$date <- as.Date(lagDat$timestamp)
  
  # subset data to time period of interest and  - this is the overall time period AT used
  predDat <- predDat[predDat$date>as.Date(startDate)+interTimePeriod & predDat$date<as.Date(endDate)+interTimePeriod,]
  lagDat <- lagDat[lagDat$date>as.Date(startDate) & lagDat$date<as.Date(endDate),]
  
  # bring the two stock prices together
  tmp <- data.frame(predPrice = predDat$close,
                    predDate = predDat$date,
                    lagPrice = lagDat$close,
                    lagDate = lagDat$date)
  
  return(tmp)
  
}

# example
# datForJS <- readDataLag(predStock = 'MSFT',
#                         lagStock = 'BE',
#                         interTimePeriod = 21,
#                         startDate = '2017-01-01',
#                         endDate= '2017-12-31')
