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

# add strike flag
#   strike September 16-October 18, 2000
#   strike October 14-November 18, 2003
crime.data$strike03 <- with(crime.data, as.numeric(Q==3 & year==2003))

write.csv(crime.data,file="data.csv",
          row.names=FALSE,quote=FALSE)

# examine 2003 strike
# get RDs with stations that should be open in 2002Q2
RDstations2003 <- with(subset(crime.data,year==2002 & Q==2 & !is.na(Q.open)), 
                       rd[Q.open>0])
data0 <- subset(crime.data, (year %in% 2002:2004) & (Q %in% 1:4) &
                            (rd %in% RDstations2003))
data0$TOT <- with(data0, AGG+BTFV+BURG+GTA+GTP+HOM+ROBB)

b <- list(list(form=TOT ~Q+year,lab="Total crimes"),
          list(form=AGG ~Q+year,lab="Assaults"),
          list(form=BTFV~Q+year,lab="Burglary/theft from vehicle"),
          list(form=BURG~Q+year,lab="Burglary"),
          list(form=GTA ~Q+year,lab="Auto theft"),
          list(form=GTP ~Q+year,lab="Grand theft from person"),
          list(form=HOM ~Q+year,lab="Homicide"),
          list(form=ROBB~Q+year,lab="Robbery"))

par(mfrow=c(2,4))
for(i in 1:8)
{
   a <- aggregate(b[[i]]$form,data=data0,FUN=sum)
   plot(1:12,a[,3],axes=FALSE,xlab="Quarter",ylab=b[[i]]$lab)
   box()
   axis(1,1:12,paste0(a$year,"Q",a$Q))
   axis(2)
   abline(v=c(6.5,7.5),col="red")
   text(7.0,(par()$usr[3]+a[7,3])/2,"Strike",col="red",srt=90)
}
