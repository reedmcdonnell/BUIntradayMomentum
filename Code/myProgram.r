library(tidyverse)
library(lubridate)

files <- dbSendQuery(wrds, "select distinct table_name
                            from information_schema.columns
                            where table_schema='taqmsec'
                            and table_name LIKE 'ctm_%'                             
                            order by table_name")

dates <- dbFetch(files)
dbClearResult(files)
dates
dates <- dates %>% filter(table_name >= 'ctm_20150101')
dates <- dates$table_name 
dates <- paste("select date, time_m, price, size from", dates,
               "where sym_root = 'EFA' and tr_corr = '00'
                and (time_m <= '16:01:00.00' and time_m >= '09:30:00.00')")

data_pull <- function(str) {
  res <- dbSendQuery(wrds, str)
  data <- dbFetch(res)
  dbClearResult(res)
  return(data)
}

data <- map(dates, data_pull)
data <- bind_rows(data)
data$Date <- ymd_hms(paste(data$date, data$time_m))
data      <- data %>% select(c(Date, price, size))
data      <- data %>% group_by(Date = cut(Date, breaks="1 min")) %>%
               summarize(Price = round(sum(price * size)/sum(size), 2), Size = sum(size))
data      <- write.csv(data, file="EFA.csv", row.names=FALSE)
