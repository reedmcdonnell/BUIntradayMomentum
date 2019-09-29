#Clear Workspace
rm(list = ls())

#Packages
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

#Set directory
setwd('../ConsolidatedData/')
path <- paste0(getwd(), '/')

#Load minute data for SPY
load(paste0(path,'minuteData.Rda'))

attr(minuteData$Date, "tzone") <- "EST5EDT"
minuteData[, Date := Date - 60 * 60] #<- minuteData$Date - 60*60 #subratct hour
minuteData$Ret <- minuteData$Price / shift(minuteData$Price) - 1
#Move returns backward so Ret for 9:30 -> Price @ 9:31 / Price @ 9:30 - 1
minuteData$Ret <- shift(minuteData$Ret, -1)

#Compute 1 minute returns

#Aggregate minute data by date and get P1 (10:00 price), P2 (15:00 price), and P3 (15:30 price)
dateData <- minuteData[, .(Open = first(Price), Close = last(Price), 
                           P1 = Price[hour(Date) == 10 & minute(Date) == 0],
                           Vol1 = sd(Ret[hour(Date) < 10]),
                           P2 = Price[hour(Date) == 15 & minute(Date) == 0],
                           Vol2 = sd(Ret[hour(Date) == 15 & minute(Date) < 30 ]),
                           P3 = Price[hour(Date) == 15 & minute(Date) == 30],
                           Vol3 = sd(Ret[hour(Date) == 15 & minute(Date) >= 30 ]),
                           Size = sum(Size)),
                       by = cut(Date, "1 day")]
#Reset date column (cut resets formatting to factor?)
names(dateData)[1] <- 'Date'
dateData$Date <- as.Date(dateData$Date)

#Sort data
dateData <- dateData[order(Date)]

#Remove some NAs (not too many)
dateData <- na.omit(dateData)

#Volatility group based on first 30 minutes return volatility
dateData[, VolGroup := ifelse(Vol1 < median(Vol1), 1, 2)]

#30 minute returns R1 (first half hour), R2 (2nd to last half hour), R3 (last half hour)
dateData <- dateData[, .(Date, Open, Close, P1, P2, P3, Size, 
                         Vol1, Vol2, Vol3, VolGroup,
                         R1 = P1 / shift(Close) - 1, 
                         R2 = P3 / P2 - 1, 
                         R3 = Close / P3 - 1)]
#Remove some NAs (not too many)
dateData <- na.omit(dateData)

################
###Regression###
################
#Regression by Volatility Groups
#Lower vol
volmod1 <- lm(R3 ~ R1, data=dateData[VolGroup == 1])
summary(volmod1)

#Higher vol
volmod2 <- lm(R3 ~ R1, data=dateData[VolGroup == 2])
summary(volmod2)

#Normal Regression
#R3 ~ R1
mod1 <- lm(R3 ~ R1, data=dateData)
summary(mod1)

#Two variables
#R3 ~ R1 + R2
mod2 <- lm(R3 ~ R1 + R2, data=dateData) 
summary(mod2)

#Save regression coefficients for prediction (2 variable model)
B0 <- coef(mod2)[[1]]
B1 <- coef(mod2)[[2]]
B2 <- coef(mod2)[[3]]

#Compute Daily Retruns and Cummulative Returns for Strategies
#Long SPX
dateData$SPXRet <- (dateData$Close / shift(dateData$Close)) - 1
dateData <- na.omit(dateData)
dateData$SPXCumRet <- cumprod(1 + dateData$SPXRet)

#Long/Hold
dateData[, Strat1Ret := ifelse(B0 + B1 * R1 + B2 * R2 > 0, R3, -R3)]
dateData$Strat1CumRet <- cumprod(1 + dateData$Strat1Ret)

#Long Only
dateData[, Strat2Ret := ifelse(B0 + B1 * R1 + B2 * R2 > 0, R3, 0)]
dateData$Strat2CumRet <- cumprod(1 + dateData$Strat2Ret)

#Short only
dateData[, Strat3Ret := ifelse(B0 + B1 * R1 + B2 * R2 > 0, 0, -R3)]
dateData$Strat3CumRet <- cumprod(1 + dateData$Strat3Ret)

dateData$Test <- dateData$Strat1Ret - dateData$SPXRet

#Naive
dateData$NaiveCumRet <- cumprod(1 + dateData$R3)

hist(dateData$Test)

hist(dateData$Strat1Ret)

hist(dateData$Strat1Ret - dateData$SPXRet)




#Plot strategies
plot <- ggplot() +
  geom_line(data = dateData, aes(x = Date, y = Test, colour = 'Cominbed')) + 
  
  #geom_line(data = dateData, aes(x = Date, y = SPXCumRet, colour = 'SPX Long')) + 
  #geom_line(data = dateData, aes(x = Date, y = Strat1CumRet, colour = 'Long/Short Strategy')) + 
  #geom_line(data = dateData, aes(x = Date, y = Strat2CumRet, colour = 'Long Strategy')) + 
  #geom_line(data = dateData, aes(x = Date, y = Strat3CumRet, colour = 'Short Strategy')) + 
  #geom_line(data = dateData, aes(x = Date, y = NaiveCumRet, colour = 'Naive Strategy')) + 
  xlab('Dates') + ylab('Return') + ggtitle('Strategy Return')
plot(plot)

#Plot daily return density
#Compute mean, var, skew, kertosis (especially skew - right tail returns look heavy :) )
plot(density(dateData$Strat1Ret), main = 'Strat1Ret Density')

#Regression values vs time plots
regTable <- data.frame(Date=as.Date(character()),
                    Coeff=numeric(), 
                    StdErr=numeric(), 
                    tVal=numeric(), 
                    pVal=numeric(),
                    R2=numeric(),
                    Type=character(),
                    stringsAsFactors=FALSE)

print('Calculating regression output time-series tables...')
for(date in dateData$Date) {
  if(date < '2002-01-01') {
    next
  }
  date <- as.Date(date, origin = '1970-01-01')
  temp <- dateData[Date <= date, ]
  #R3 ~ R1 + R2
  tempMod <- lm(R3 ~ R1 + R2, data=temp)
  regVals <- summary(tempMod)$coefficients %>% data.table
  r2 <- summary(tempMod)$adj.r.squared
  regVals$R2 <- r2
  regVals$Type <- c('B0', 'B1', 'B2')
  regVals$Date <- date
  names(regVals) <- c('Coeff', 'StdErr', 'tVal', 'pVal', 'R2', 'Type', 'Date')
  regVals <- regVals[, .(Date, Coeff, StdErr, tVal, pVal, R2, Type)]
  
  regTable <- rbind(regTable, regVals)
}
print('done')

#Plots
#Coefficients
plot <- ggplot() + 
  geom_line(data = regTable[Type == 'B0'], aes(x = Date, y = Coeff, colour = 'B0')) + 
  geom_line(data = regTable[Type == 'B1'], aes(x = Date, y = Coeff, colour = 'B1')) + 
  geom_line(data = regTable[Type == 'B2'], aes(x = Date, y = Coeff, colour = 'B2')) +
  xlab('Dates') + ylab('Value') + ggtitle('Coefficients')
plot(plot)

#P-vals
plot <- ggplot() + 
  geom_line(data = regTable[Type == 'B0'], aes(x = Date, y = pVal, colour = 'B0')) + 
  geom_line(data = regTable[Type == 'B1'], aes(x = Date, y = pVal, colour = 'B1')) + 
  geom_line(data = regTable[Type == 'B2'], aes(x = Date, y = pVal, colour = 'B2')) +
  xlab('Dates') + ylab('%') + ggtitle('p-Values')
plot(plot)

#Adj R2
plot <- ggplot() + 
  geom_line(data = regTable, aes(x = Date, y = R2, colour = 'Adj R^2')) + 
  xlab('Dates') + ylab('%') + ggtitle('Adj R^2')
plot(plot)

##############################
###Out-of-Sample Regression###
##############################
#Scatter plot data
scatter.smooth(x=dateData$R1, y=dateData$R3, main="R3 ~ R1")
#Interesting here
scatter.smooth(x=dateData$R2, y=dateData$R3, main="R3 ~ R2")

#Densitys - make sure theyre normal
plot(density(dateData$R1), main = 'R1 Density')
plot(density(dateData$R2), main = 'R2 Density')
plot(density(dateData$R3), main = 'R3 Density')
qqplot(dateData$R3, dateData$R1)

cor(dateData$R1, dateData$R3)

# Create Training and Test data -
set.seed(Sys.time())  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(dateData), 0.8*nrow(dateData))  # row indices for training data
trainingData <- dateData[trainingRowIndex, ]  # model training data
testData  <- dateData[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(R3 ~ R1, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

#Test accuracy here
#Test for same sign
sum(ifelse(distPred * testData$R3 > 0, 1, 0)) / length(distPred)
