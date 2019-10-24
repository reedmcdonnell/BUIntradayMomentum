#FUNCTIONS
retCols <- unlist(lapply(seq(13), function(x) paste0('R', x)))
volCols <- unlist(lapply(seq(13), function(x) paste0('V', x)))

#Plots histogram for x vector with title and variable title
#TODO: Set y dimensions?
histogram <- function(x, title, varTitle) {
  std <- sd(x)
  avg <- mean(x)
  binWdth <- (max(x) - min(x)) / 100
  qplot(x,
        geom="histogram",
        binwidth=binWdth,
        xlim=c(avg - 3 * std, avg + 3 * std),
        main=title, 
        xlab=varTitle,  
        fill=I("blue"), 
        col=I("red"), 
        alpha=I(.2)) + annotate("text", label = paste0('Mean: ', signif(avg, 4), ', Std: ', signif(std, 4), '\n', 
                                                       'Kurt: ', signif(kurtosis(x), 4), ' Skew: ', signif(skewness(x), 4)),
                                x = avg - 2 * std, y = 200, size = 4, colour = "red")
}

#loadData:
#Inputs:
# ticker: String refering to ticker data set of form 'ticker'.Rda
#         .Rda data file contains minute price history with size for ticker
#Ouput:
# dataframe: Date -> R1, R2, ... R13, V1, V2, ... V13, Vol(avg daily vol), 
#                    inVol(avg daily vol except Vol13), Open, Close
loadData <- function(ticker = 'SPY') {
  #Load minute data
  load(paste0('../ConsolidatedData2/', ticker, '.Rda'))
  
  #Seperate Date and Time from POSIX for other Use
  #TODO: 'minuteData' variable name from loading Rda file?
  minuteData[, DATE := as.Date(Date)]
  minuteData[, TIME := strftime(Date, format="%H:%M:%S")]
  
  #Remove rows that don't have continuous price history
  numOfDailyObs <- length(seq(as.POSIXct("2000-01-01 9:30:00"), as.POSIXct("2000-01-01 16:00:00"), by='min'))
  #390 minutes + 1 = 391 to include bounds
  minuteData <- minuteData[, if (.N == numOfDailyObs) .SD, by = (DATE)]
  
  #Replace 9:30 Price with previous Close Price
  minuteData[, c('Open', 'Close') := list(first(Price), last(Price)), by = DATE]
  minuteData[, Price2 := shift(Price)]
  minuteData[hour(Date) == 9 & minute(Date) == 30, Price := Price2]
  minuteData <- minuteData[, Price2 := NULL]
  minuteData <- na.omit(minuteData)
  minuteData <- minuteData[, if (.N == numOfDailyObs) .SD, by = (DATE)]
  
  #Calculate minute returns by day
  minuteData[, Ret := Price / shift(Price) - 1, by = DATE]
  minuteData[, Ret := shift(Ret, -1)]
  
  #Group by 30 minute buckets and calculate return and volatility
  bucketData <- minuteData[, .(DATE = first(DATE), TIME = first(TIME), Open = first(Open), Close = first(Close),
                               Ret = (tail(cumprod(1 + Ret), 1) - 1), Vol = sd(Ret), Size = sum(Size)), 
                           by = cut(Date, '30 min')]
  names(bucketData)[1] <- 'Date'
  bucketData$Date <- as.POSIXct(bucketData$Date)
  
  #Create time group number
  bucketData[, Bucket := .GRP, by = TIME]
  
  retData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Ret')
  retData[, ncol(retData) := NULL]
  names(retData) <- c('DATE', retCols)
  
  volData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Vol')
  volData[, ncol(volData) := NULL]
  names(volData) <- c('DATE', volCols)
  dates <- volData$DATE
  #Log transformation for volatility: REMOVED BECAUSE NO IMPROVMENT IN REGRESSION FIRST ORDER VOL TERMS
  #volData <- volData[, lapply(.SD, FUN = function(x) log(x)), .SDcols = setdiff(colnames(volData), "DATE")]
  volData$DATE <- dates
  #Average daily vol
  volData[, Vol := rowMeans(.SD), by = 'DATE']
  #Average daily vol not including R13
  volData[, inVol := rowMeans(.SD), .SDcols = setdiff(colnames(volData), c('DATE', 'V13')), by = 'DATE']
  
  sizeData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Size')
  sizeData[, Size := Reduce(`+`, .SD), by = DATE]
  sizeData <- sizeData[, .(DATE, Size)]
  
  openClose <- unique(bucketData[, .(DATE, Open, Close)])
  
  data <- Reduce(function (x, y) merge(x, y, by = c('DATE')), list(retData, volData, sizeData, openClose))
  names(data)[1] <- 'Date'
  
  return(data)
}



#Given data frame
#Returns coefR1 coefR5 pValR1 pValR5 adjRSquared residualStdError
#Note: isLookback dictates whether the complete history backwards from a date is used
#      or if date minus lookback data is used
rollRegressFunc <- function(x, isLookback) {
  formula <- 'R13 ~ R1 + R5'
  model <- lm(formula, data = x)
  modelSummary <- summary(model)
  coeffs <- modelSummary$coefficients[, 1]
  
  pVals <- t(coeftest(model, vcov = vcovHC(model))[2:3, 4]) %>% data.table()
  names(pVals) <- sapply(names(pVals), function(x) paste0('PVal', x))
  coeffs <- t(coeffs[c('R1', 'R5')]) %>% data.table()
  names(coeffs) <- sapply(names(coeffs), function(x) paste0('Coeff', x))
  
  temp <- cbind(pVals, coeffs)
  
  temp[, AdjRSquared := modelSummary$adj.r.squared]
  temp[, ResidualStdErr := modelSummary$sigma]
  temp[, Date := as.character(max(x$Date))]
  temp[, Lookback := isLookback]
  
  return(temp)
}