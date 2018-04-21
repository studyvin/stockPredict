#-----------------------------------------------------------
# Use the forecastingFunction.R file and list of symbols to 
# determine which stocks our modelling is most accurate
#-----------------------------------------------------------

## This is the path to your local copy of the repository 
repoPath <- '~/project/stockPredict/'
## data path
dataPath <- paste0(repoPath,'data/')


## source all of the functions
cc <- lapply(list.files(paste0(repoPath,'function/'),pattern='*.R',full.names=TRUE),source)




# list of tech, energy, healthcare, and finance stock symbols
allSym <- read.csv(paste0(dataPath,'allSymbols.csv'))





# in future runs, it would be nice to give the predictStockPrice() function a giant list of stocks and let
# it filter through and find which stocks it has the highest accuracy rate
# tmp <- lapply(allSym$Symbol,FUN = function(x) predictStockPrice(symbol = x, nTimePeriods = 10))

# example for comparing Microsoft, Google, and Netflix
tmp <- lapply(c('MSFT','GOOGL','NFLX'), FUN = function(x) predictStockPrice(symbol = x, nTimePeriods = 10))

# then subset tmp to remove bad predictions (like Netflix) and keep the most accurate/believable predictions 
# like (MSFT and GOOGL)
