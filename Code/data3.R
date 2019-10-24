toSave <- FALSE

###################################################################################

#Clear Workspace
rm(list = ls())

#Source files
source('./libraries.R')
source('./functions.R')

#Store condensed data sets Date -> R1, R2, ... R13, V1, V2, ... V13, Size....
files = list.files(path = '../ConsolidatedData2/', pattern = '.Rda')
tickers <- sapply(strsplit(files, '\\.'), function (x) x[1])
for(ticker in tickers) {
  print(ticker)
  
  data <- loadData(ticker)
  if(toSave) {
    save(data, file = paste0('../ConsolidatedData3/', ticker, '.Rda')) 
  }
}

####################################################################################

#Make master file
load('../ConsolidatedData3/SPY.Rda')
assign('spy', data)
spy[, Ticker := 'SPY']

master <- copy(spy)

files = list.files(path = '../ConsolidatedData2/', pattern = '.Rda')
tickers <- sapply(strsplit(files, '\\.'), function (x) x[1])
tickers <- tickers[tickers != 'SPY']
for(ticker in tickers) {
  load(paste0('../ConsolidatedData3/', ticker, '.Rda'))
  assign('temp', data)
  
  temp[, Ticker := ticker]
  
  master <- rbind(master, temp)
}

if(toSave) {
  save(master, file = '../FinalData/master.Rda')
}


##############################################################################

#Generate Regression time series data for all tickers
load('../FinalData/master.Rda')

#List min and max date for each ticker
dateRanges <- master[, .(MinDate = min(Date), MaxDate = max(Date)), by = Ticker] %>% 
  summarise(min = max(MinDate), max = min(MaxDate))

master <- master[Date %between% c(dateRanges$min, dateRanges$max)]

#Determine list of best variables for each ticker?  Done as we work>

#Start with SPY and append to it (helps to start dataframe)
temp <- getRollingRegressionData(master[Ticker == 'SPY'], setdiff(retCols, 'R13'), 'SPY')
timeSeriesRegressVar <- temp[[1]]
timeSeriesRegressModel <- temp[[2]]

print('Gathering regression time series data for all tickers')
tickers <- unique(master$Ticker)
tickers <- tickers[tickers != 'SPY']
for(ticker in tickers) {
  print(ticker)
  temp <- getRollingRegressionData(master[Ticker == ticker], setdiff(retCols, 'R13'), ticker)
  timeSeriesRegressVar <- rbind(timeSeriesRegressVar, temp[[1]])
  timeSeriesRegressModel <- rbind(timeSeriesRegressModel, temp[[2]])
}

if(toSave) {
  save(timeSeriesRegressVar, file = '../FinalData/timeSeriesRegressVar.Rda')
  save(timeSeriesRegressModel, file = '../FinalData/timeSeriesRegressModel.Rda')
}
