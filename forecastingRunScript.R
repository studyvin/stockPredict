#-----------------------------------------------------------
# Use the forecastingFunction.R file and list of symbols to 
# determine which stocks our modelling is most accurate
#-----------------------------------------------------------
rm(list=ls())
options(stringsAsFactors = F)


## This is the path to your local copy of the repository 
repoPath <- '~/project/stockPredict/'
## paths
dataPath <- paste0(repoPath,'data/')
codePath <- paste0(repoPath,'function/')

## source functions
cc <- lapply(list.files(codePath,pattern='*.R',full.names=TRUE),source)


## read in AT results about correlations between stocks
corStock <- read.csv(paste0(dataPath,'stkCompDF_computers_intra7inter3.csv'))
meanCor <- plyr::ddply(corStock,~stkSym1+stkSym2+compInd+intraStkTimePeriod+interStkTimePeriod,dplyr::summarize, meanCor=mean(abs(corVal)),n=length(corVal))
meanCor <- meanCor[with(meanCor,order(meanCor,decreasing=TRUE)),]
row.names(meanCor) <- NULL


#################################################
head(meanCor,30)
rowNum <- 16 ###### note the magic number here
## we need a good way to pick a stock combination
#################################################



foreStock <- forestStockPrice(responseStock = meanCor[rowNum,'stkSym2'],
covarStock = meanCor[rowNum,'stkSym1'],
interTimePeriod = prod(meanCor[rowNum,c('intraStkTimePeriod','interStkTimePeriod')]),
startDate = '2017-01-01',
endDate = '2018-05-27')

plot(foreStock$foreCast)

