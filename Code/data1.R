#Create RDA files by ticker from TAQ source data
#POSIX -> Price, Size
#By minute
#Forward fill for 16:00:00 (end of day) missing data for price
#Note: 16:00:00 trade size is 1 and forward filled value will reduce 
#1-minute return voliatliy for last bucket
#NEED MORE DECIMAL PLACES FOR PRICES!!

#Clear Workspace
rm(list = ls())

#Packages
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)
library(psych)
library(zoo)

#Change source directory to consolidated data
getwd()
setwd('../ConsolidatedData/')

#Iterate TAQ csv file
files <- list.files(pattern="*.csv")

for(file in files) {
  ticker <- strsplit(file, '\\.')[[1]][1]
  print(ticker)
  
  minuteData <- read.csv(file, stringsAsFactors = FALSE) %>% data.table
  if('X' %in% colnames(minuteData)) {
    minuteData[, X := NULL]
  }
  
  minuteData[, Date := as.POSIXct(Date)]
  
  #Forward fill 16:00
  dates <- as.Date(minuteData$Date)
  closeTimes <- unlist(lapply(as.character(dates), function (d) paste0(d, ' ', '16:00:00')))
  
  #Set union to get complete Datetime index list
  indicies <- union(as.character(minuteData$Date), closeTimes)
  indicies <- setDT(as.data.table(indicies))
  colnames(indicies) <- c('Date')
  indicies[, Date := as.POSIXct(Date)]
  
  #Merge and forward fill by date (fills gaps for missing close price)
  minuteData <- merge(minuteData, indicies, all.y = TRUE)
  minuteData$Price <- na.locf(minuteData$Price)
  minuteData[is.na(Size), Size := 1]
  
  save(minuteData, file=paste0('../ConsolidatedData2/', ticker, '.Rda'))
}

#Consolidate SPY
load('../ConsolidatedData/SPY2014.Rda')
spy2014 <- minuteData
rm(minuteData)

#Fix timezone issue
attr(spy2014$Date, "tzone") <- "EST5EDT"
spy2014[, Date := Date - 60 * 60] #<- minuteData$Date - 60*60 #subratct hour

load('../ConsolidatedData2/SPY.Rda')
spy2015 <- minuteData
rm(minuteData)

min(spy2014$Date)
max(spy2014$Date)
min(spy2015$Date)
max(spy2015$Date)

#Bind
minuteData <- rbind(spy2014, spy2015)
min(minuteData$Date)
max(minuteData$Date)

save(minuteData, file = '../ConsolidatedData2/SPY.Rda')

