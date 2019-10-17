---
  title: "IntradayMomentum"
output:
  html_document: default
pdf_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r echo=FALSE, warning=FALSE, message = FALSE}
#Clear Workspace
rm(list = ls())

#Packages
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)
library(psych)
```

```{r echo = FALSE}
#FUNCTIONS

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
```

```{r echo=FALSE}
#LOAD DATA

#Load minute data for SPY
#load(paste0('../ConsolidatedData/', 'minuteData.Rda'))
minuteData <- read.csv('../ConsolidatedData/SPY.csv', stringsAsFactors = FALSE) %>% data.table
minuteData[, X := NULL]
minuteData[, Date := as.POSIXct(Date)]

#Forward fill 16:00
#Generate sequence of times 9:30 - 16:00 for all 
times <- strftime(seq(
  from=as.POSIXct("2001-1-1 9:30", tz="EST"),
  to=as.POSIXct("2001-1-1 16:00", tz="EST"),
  by="min"
), format="%H:%M:%S")
dates <- as.Date(minuteData$Date)

indicies <- unlist(lapply(dates, function (d)
  unlist(lapply(times, function (t) paste0(as.character(d), ' ', as.character(t) )))
))

#Disjoint set between indicies and current Dates in minuteData
diff <- setdiff(indicies, as.character(minuteData$Date))
diff <- setDT(diff)

#Merge and forward fill by date (fills gaps for missing close price)
test <- merge(minuteData, diff)

#Fix timezone issue
#attr(minuteData$Date, "tzone") <- "EST5EDT"
#minuteData[, Date := Date - 60 * 60] #<- minuteData$Date - 60*60 #subratct hour

#Seperate Date and Time from POSIX for other Use
minuteData[, DATE := as.Date(Date)]
minuteData[, TIME := strftime(Date, format="%H:%M:%S")]


#Remove rows that don't have continuous price history
#numOfDailyObs <- length(seq(as.POSIXct("2000-01-01 9:30:00"), as.POSIXct("2000-01-01 16:00:00"), by='min')) #390 minutes + 1 = 391 to include bounds
#numOfDailyObs <- 391
#minuteData <- minuteData[, if (.N == numOfDailyObs) .SD, by = (DATE)]

#Replace 9:30 Price with previous Close Price
minuteData[, c('Open', 'Close') := list(first(Price), last(Price)), by = DATE]
minuteData[, Price2 := shift(Price)]
minuteData[hour(Date) == 9 & minute(Date) == 30, Price := Price2]
minuteData <- minuteData[, Price2 := NULL]
minuteData <- na.omit(minuteData)
#minuteData <- minuteData[, if (.N == numOfDailyObs) .SD, by = (DATE)]

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
retCols <- unlist(lapply(seq(13), function(x) paste0('R', x)))
names(retData) <- c('DATE', retCols)

volData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Vol')
volData[, ncol(volData) := NULL]
volCols <- unlist(lapply(seq(13), function(x) paste0('V', x)))
names(volData) <- c('DATE', volCols)
dates <- volData$DATE
volData <- volData[, lapply(.SD, FUN = function(x) log(x)), .SDcols = setdiff(colnames(volData), "DATE")]
volData$DATE <- dates
volData[, Vol := rowMeans(.SD), by = 'DATE']

sizeData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Size')
sizeData[, Size := Reduce(`+`, .SD), by = DATE]
sizeData <- sizeData[, .(DATE, Size)]

openClose <- unique(bucketData[, .(DATE, Open, Close)])

dateData <- Reduce(function (x, y) merge(x, y, by = c('DATE')), list(retData, volData, sizeData, openClose))
names(dateData)[1] <- 'Date'

data <- dateData

#Calculate average vol
```

## Preliminary data analysis
Histograms
```{r, echo = 15, warning=FALSE}
histogram(data$R1, 'First 30-Minute Returns', 'R1')
describe(data$R1)
histogram(data$R13, 'Last 30-Minute Returns', 'R3')
describe(data$R13)
#histogram(data$V1, 'First 30-Minute Volatility', 'Vol')
#describe(data$V1)
#histogram(data$V13, 'Last 30-Minute Volatility', 'Vol')
#describe(data$V13)
histogram(data$Vol, 'Daily Log Volatility', 'Vol')
describe(data$Vol)
histogram(data$Size, 'Trade Size', 'Size')
describe(data$Size)
```

## Regressions
```{r}
#R13 = B0 + B1*R1 + B2*R2 + ... + B13*R13
formula <- paste("R13 ~", paste(head(retCols, length(retCols) - 1), collapse = " + "))
model1 <- lm(formula, data = data)
summary(model1)

#R13 = B0 + B1*R1 + B2*R2 + ... + B13*R13
#formula <- paste("R13 ~", paste(head(retCols, length(retCols) - 1), collapse = " + "))
#model1 <- lm(formula, data = data)
#summary(model1)
```

## Strategies
Long/Short Strategy: R13 = B0 + B1*R1 + ... B12*R12
```{r}
#Long/Short Strategy
#Predict value
data[, Strat1Signal := predict(model1)]
data[, Strat1Ret := ifelse(Strat1Signal > 0, R13, -R13)]
data[, Strat1CumRet := cumprod(1 + Strat1Ret)]

#Random Strategy
data[, RandomSignal := rbinom(.N, 1, .5)]
data[, RandomRet := ifelse(Strat1Signal == 1, R13, -R13)]
data[, RandomCumRet := cumprod(1 + RandomRet)]

#SPY Hold Strategy
data[, SPYRet := Close / shift(Close) - 1]
data[is.na(SPYRet), SPYRet := 0]
data[, SPYCumRet := cumprod(1 + SPYRet)]
```

## Plot Strategies
```{r}
#Plot strategies
plot <- ggplot() +
  geom_line(data = data, aes(x = Date, y = SPYCumRet, colour = 'SPY Long')) + 
  geom_line(data = data, aes(x = Date, y = Strat1CumRet, colour = 'Long/Short Strategy')) + 
  geom_line(data = data, aes(x = Date, y = RandomCumRet, colour = 'Random Strategy')) + 
  xlab('Dates') + ylab('Return') + ggtitle('Strategy Return')
plot(plot)
```

## Histogram of Strategy returns
```{r warning=FALSE}
histogram(data$Strat1Ret, 'Long/Short Strategy Daily Returns', 'Strat1Ret')
histogram(data$Strat1Ret - data$SPYRet, 'Excess Daily Returns', 'Excess Returns')
```

## Omit observations with daily volatility oustide 2 and 3 standard devations
Due to the extrodinary circumstances of 2008, we omit observations with daily volatility greater than 2 & 3 standard deviations compared with the mean daily volatility
```{r}
data[, c('Strat1Ret_Vol2', 'Strat1Ret_Vol3') := list(Strat1Ret, Strat1Ret)]
data[!(Vol %between% c(mean(Vol) - 2*sd(Vol), mean(Vol) + 2*sd(Vol))), Strat1Ret_Vol2 := 0]
data[!(Vol %between% c(mean(Vol) - 3*sd(Vol), mean(Vol) + 3*sd(Vol))), Strat1Ret_Vol3 := 0]

data[, Strat1_Vol2CumRet := cumprod(1 + Strat1Ret_Vol2)]
data[, Strat1_Vol3CumRet := cumprod(1 + Strat1Ret_Vol3)]

#Plot Long/Hold std modified strategy
plot <- ggplot() +
  geom_line(data = data, aes(x = Date, y = SPYCumRet, colour = 'SPY Long')) + 
  geom_line(data = data, aes(x = Date, y = Strat1CumRet, colour = 'Long/Short Strategy')) + 
  geom_line(data = data, aes(x = Date, y = Strat1_Vol2CumRet, colour = 'Long/Short Strategy +/- 2 Std. Dev. Volatility')) + 
  geom_line(data = data, aes(x = Date, y = Strat1_Vol3CumRet, colour = 'Long/Short Strategy +/- 3 Std. Dev. Volatility')) + 
  xlab('Dates') + ylab('Return') + ggtitle('Strategy Return')
plot(plot)
```

## Histograms of strategies modified by volatiliyt
```{r, warning=FALSE}
histogram(data$Strat1Ret - data$SPYRet, 'Long/Short Strategy Daily Returns', 'Returns')
describe(data$Strat1Ret - data$SPYRet)
histogram(data$Strat1Ret_Vol2 - data$SPYRet, 'Long/Short Strategy (+/- 2 Std. Dev. Volatility) Excess Daily Returns', 'Returns')
describe(data$Strat1Ret_Vol2 - data$SPYRet)
histogram(data$Strat1Ret_Vol3 - data$SPYRet, 'Long/Short Strategy (+/- 3 Std. Dev. Volatility) Excess Daily Returns', 'Returns')
describe(data$Strat1Ret_Vol3 - data$SPYRet)
```
