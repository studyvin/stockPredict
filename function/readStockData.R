##############################################
## Jared Studyvin
## 4 June 2018
## read in stock data and subset
##############################################

readStockData <- function(symbol,
                        startDate,
                        endDate=Sys.Date()
                        ){
    ##sym <- covarStock
  options(stringsAsFactors = F)


    sym = symbol
    
    datURL <- paste0('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&outputsize=full&apikey=LQY762JM5VDA5ETC&symbol=',sym,'&datatype=csv')
    
    dat <- read.csv(datURL)
    
    ## subset to smaller timeframe - everything from 2015 to current
    dat <- subset(dat, timestamp > startDate & timestamp<=endDate)
  

  return(dat[,c('timestamp','close')])
  
} #end readStockData
