setwd("z:/articles/transit and crime/LAPDcrimedata")

library(lubridate)

# load crime and station information
crime.data <- read.csv("LAPD crime counts 1988-2014 merged.csv",as.is=TRUE)
station.rd.xwalk <- read.csv("METROdata/RD station xwalk.csv",as.is=TRUE)
station.rd.xwalk$open.date <- ymd(station.rd.xwalk$open.date)

# merge station information (opening date & line) onto crime data
crime.data$station.date <- ymd(NA)
crime.data$line         <- NA

xwalk.var <- strsplit(crime.data$rd,";")
for(i in which(!is.na(station.rd.xwalk$open.date)))
{
   j <- which(sapply(xwalk.var, function(x) {station.rd.xwalk$rd[i] %in% x}))
   crime.data$station.date[j] <- station.rd.xwalk$open.date[i]
   crime.data$line[j]         <- station.rd.xwalk$Line[i]
}

# compute number of quarters station open (negative means days until open)
crime.data$Q.open <- NA
i <- !is.na(crime.data$station.date)
crime.data$Q.open[i] <- with(crime.data[i,], 
   (year+Q/4-1/8)-decimal_date(station.date))
