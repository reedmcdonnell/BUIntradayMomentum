#Clear workspace
rm(list = ls())

#Packages
library(data.table)
library(lubridate)
library(dplyr)

#Aggregate trades by minute and store here
minuteData <- data.frame(Date=as.POSIXct(character()),
                         Price=numeric(),
                         Size=numeric(),
                         stringsAsFactors=FALSE)

#Iterate csv files
setwd('../../SourceData/')
path <- paste0(getwd(), '/')

files <- list.files()
files <- files[files %like% '*csv']
for(file in files) {
  print(file)
  header <- fread(paste0(path, file), header = F, sep = ',', nrows = 1)
  #Read file in chunks of 10MM rows
  for(i in 0:100) {
    print(i)
    #Chunk will fail if skip rows > # of rows (done reading)
    done <- tryCatch({
      data <- fread(paste0(path, file), header = F,
                    sep = ',', skip = 1+i*10e6, nrow = 10e6) %>% data.table() #10MM rows per chunk
      
      colnames(data) <- as.character(as.vector(header[1,]))
      return <- FALSE
    }, error = function(e) {
      print('Finished reading file.')
      return <- TRUE
    })
    
    #Done reading file
    if(done) {
      break
    }
    
    #Create POSIX datetime column
    data$Date <- as.POSIXct(strptime(paste0(as.character(data$DATE), ' ', data$TIME), "%Y%m%d %H:%M:%S"))
    data[, DATE := NULL]
    data[, TIME := NULL]
    
    #Subset normal trading hours: [9:30 AM - 4:00 PM] (EST?)
    data <- data[(hour(Date) == 9 & minute(Date) >= 30) |
                   (hour(Date) > 9 & hour(Date) < 16) |
                   (hour(Date) == 16 & minute(Date) == 0 & second(Date) == 0)]
    
    #Omit trade correctoins (small nrow reduction)
    data <- data[CORR == 0]
    
    #SUBSET BY CONDITION HERE!!! - TODO
    #data <- data[COND %in% c()]

    #Weighted average price per minute
    data <- data[, .(Price = sum(PRICE * SIZE)/sum(SIZE), Size = sum(SIZE)), by=cut(Date, "1 min")]
    
    #Reformat Date column
    names(data)[1] <- 'Date'
    data$Date <- as.POSIXct(strptime(data$Date, "%Y-%m-%d %H:%M:%S"))
    
    minuteData <- rbind(minuteData, data)

    #Garbage collection to save memory
    rm(data)
    gc()
  }
}

#Remove overlap
minuteData <- minuteData[, .(Price = sum(Price * Size) / sum(Size), Size = sum(Size)),
                         by=cut(Date, "1 min")]

names(minuteData)[1] <- 'Date'
minuteData$Date <- as.POSIXct(strptime(minuteData$Date, "%Y-%m-%d %H:%M:%S"))

