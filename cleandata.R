# read in LAPD crime data OCRd from the LA public library (1988-2005)
# read in LAPD incident data (2004-2010)
# read in LAPD incident data (2011-2014)
#    2011,2012 - came from LAPD Office of CIO
#    2013,2014 - published at data.lacity.org
# merge the three to make a 1988-2014 dataset
setwd("z:/articles/transit and crime/LAPDcrimedata")
library(doParallel)
library(lubridate)
library(lattice)

##############################################################################
# load data from LAPD incidents
incd.data <- read.csv("LAPD crime counts from incidents.csv")

##############################################################################
# load and polish data from LAPL, 1988-2005
lapl.data <-
foreach(year=1988:2005) %do%
{
   a <- read.csv(paste(year," export ready.csv",sep=""))

   a$AGG <- a$aggr.assault 
   a$aggr.assault <- NULL
   a$BTFV <- with(a, burg.from.auto+theft.from.auto)
   a$burg.from.auto <- NULL
   a$theft.from.auto <- NULL
   a$BURG <- with(a, burg.bus+burg.res+burg.oth)
   a$burg.bus <- NULL
   a$burg.res <- NULL
   a$burg.oth <- NULL
   a$GTA <- a$auto.theft
   a$auto.theft <- NULL
   a$GTP <- with(a, theft.from.pers+purse.snatch)
   a$theft.from.pers <- NULL
   a$purse.snatch <- NULL
   a$HOM <- a$murder
   a$murder <- NULL
   a$ROBB <- with(a, robb.st+robb.oth)
   a$robb.st <- NULL
   a$robb.oth <- NULL
   
   a$rape <- NULL
   a$grand.theft <- NULL
   a$other.theft <- NULL
   a$bike.theft <- NULL
   a$bunco <- NULL
   a$total <- NULL
   a$division <- NULL
   
   names(a)[names(a)=="RD"] <- "rd"
   return(a)
}

##############################################################################
# compare on overlapping 2004 data
#    use for 2005 data, LAPL data source was problematic for some quarters
a <- lapl.data[[17]]
b <- subset(incd.data, quarter %in% c("2004Q1","2004Q2","2004Q3","2004Q4"))
a <- lapl.data[[18]]
b <- subset(incd.data, quarter %in% c("2005Q1","2005Q2","2005Q3","2005Q4"))

# which RDs are not listed in both sources
setdiff(a$rd,b$rd)
setdiff(b$rd,a$rd)

# link datasets by quarter and RD
d <- merge(a,b,by=c("quarter","rd"),all=TRUE,suffixes=c(".a",".b"))
# RDs that only show up in one source, set counts to 0
d[is.na(d)] <- 0
# plot by quarter, ideally diagonal line
#   examine those far from diagonal
#   large differences are in RD 9999
xyplot(AGG.a ~ AGG.b | quarter, data=d)
i <- order(-abs(d$AGG.a-d$AGG.b))[1:10]
d[i,]
xyplot(BTFV.a ~ BTFV.b | quarter, data=d)
i <- order(-abs(d$BTFV.a-d$BTFV.b))[1:10]
d[i,]
xyplot(BURG.a ~ BURG.b | quarter, data=d)
i <- order(-abs(d$BURG.a-d$BURG.b))[1:10]
d[i,]
xyplot(GTA.a ~ GTA.b | quarter, data=d)
i <- order(-abs(d$GTA.a-d$GTA.b))[1:10]
d[i,]
xyplot(GTP.a ~ GTP.b | quarter, data=d)
i <- order(-abs(d$GTP.a-d$GTP.b))[1:10]
d[i,]
xyplot(HOM.a ~ HOM.b | quarter, data=d)
i <- order(-abs(d$HOM.a-d$HOM.b))[1:10]
d[i,]
xyplot(ROBB.a ~ ROBB.b | quarter, data=d)
i <- order(-abs(d$ROBB.a-d$ROBB.b))[1:10]
d[i,]

##############################################################################
# process data from data.lacity.org, 2011-2014
crimetype.xwalk <- read.csv("CrimeClassCode-CrimeTypeXWalk.csv",as.is=TRUE)
data.11.14 <- list() 
data.11.14[["2011"]] <- read.csv("open_data_crm_2011.csv.gz",as.is=TRUE)
data.11.14[["2012"]] <- read.csv("occ_data_crm_2012.csv.gz",as.is=TRUE)
data.11.14[["2013"]] <- 
   read.csv("LAPD_Crime_and_Collision_Raw_Data_for_2013.csv.gz",as.is=TRUE)
data.11.14[["2014"]] <- 
   read.csv("LAPD_Crime_and_Collision_Raw_Data_-_2014.csv.gz",as.is=TRUE)

# merge 2011-2014 together
#   make variable names consistent
names(data.11.14[[1]])[names(data.11.14[[1]])=="DR..NO"] <- "DR.NO"
names(data.11.14[[2]])[names(data.11.14[[2]])=="DR..NO"] <- "DR.NO"
data.11.14[[3]]$Location.1 <- NULL
data.11.14[[4]]$Location.1 <- NULL
#   merge
data.11.14 <- do.call(rbind,data.11.14)
#   fix dates and add quarters
data.11.14$DATE.OCC <- mdy(data.11.14$DATE.OCC)
data.11.14$quarter <- "Q1"
i <- with(data.11.14, month(DATE.OCC) %in% 4:6)
data.11.14$quarter[i] <- "Q2"
i <- with(data.11.14, month(DATE.OCC) %in% 7:9)
data.11.14$quarter[i] <- "Q3"
i <- with(data.11.14, month(DATE.OCC) %in% 10:12)
data.11.14$quarter[i] <- "Q4"
with(data.11.14, table(month(DATE.OCC),quarter))
data.11.14$quarter <- with(data.11.14, paste0(year(DATE.OCC),quarter))

# make sure only crimes occuring in 2011-2014 are included
data.11.14 <- subset(data.11.14, year(DATE.OCC) %in% 2011:2014)

# collapse crime codes into aggregated crime types
i <- match(data.11.14$Crm.Cd,crimetype.xwalk$crimeclasscode)
data.11.14$crimetype <- crimetype.xwalk$crimetype[i]
# check that those not linked are not key crime categories
table(data.11.14$crimetype)
with(data.11.14, sort(table(Crm.Cd.Desc[is.na(crimetype)])))

# tabulate crimes by type, RD, quarter
results <-
foreach(i.quarter=unique(data.11.14$quarter)) %do%
{
   a <- with(subset(data.11.14,quarter==i.quarter),
             table(RD,crimetype))
   a <- as.data.frame.matrix(a)
   a$quarter <- i.quarter
   a$rd <- rownames(a)
   return(a)      
}
data.11.14 <- do.call(rbind,results)
data.11.14$rd <- as.integer(data.11.14$rd)
# just use the crime types shared with earlier years (i.e. drop rape)
data.11.14 <- subset(data.11.14, select=names(lapl.data[[1]]))

##############################################################################
# merge datasets, LAPL 1988-2004, LAPD incident counts 2005-2014
data.final <- 
   rbind(do.call(rbind,lapl.data[1:17]),
         subset(incd.data, !(quarter %in% c("2004Q1","2004Q2","2004Q3","2004Q4"))),
         data.11.14)

data.final$year <- as.numeric(substring(data.final$quarter,1,4))
data.final$Q    <- as.numeric(substring(data.final$quarter,6,6))

# check for groups of missing RDs
with(data.final, table(year,Q,floor(rd/100)))

##############################################################################
# search for births and deaths of RDs
a <- sort(unique(data.final$rd))
pdf("plots.pdf")
for(i in a[floor(a/100)==1])
{
   plot(AGG~quarter,data=subset(data.final,rd==i),
        main=i,axes=FALSE)
   axis(1,at=seq(1,108,by=4),1988:2014,las=2,lwd.ticks=3)
   axis(1,at=1:108,labels=rep(" ",108))
}
dev.off()

#library(readstata13)
#grog <- read.dta13("../LAPDdata/lapd_from grogger.dta")
#unique(grog[,c("rd95","rd96","rd97","rd99","rpdst")])


##############################################################################
# convert all old RD numbers into new RD numbers
data.final$rd14 <- data.final$rd
#  1994->1995 change
rd.xwalk <- read.csv("LAPD1994xwalk.csv",as.is=TRUE)
i <- data.final$year<=1994
j <- match(data.final$rd14[i],rd.xwalk$RD94)
data.final$rd14[i][!is.na(j)] <- rd.xwalk$RD95[j[!is.na(j)]]
#  check
subset(data.final, year<=1996 & rd==695)

#  1995->1996 change
rd.xwalk <- read.csv("LAPD1995xwalk.csv",as.is=TRUE)
i <- data.final$year<=1995
j <- match(data.final$rd14[i],rd.xwalk$RD95)
data.final$rd14[i][!is.na(j)] <- rd.xwalk$RD96[j[!is.na(j)]]
#  check
subset(data.final, year<=1998 & rd==501)

#  1996->1997 change
rd.xwalk <- read.csv("LAPD1996xwalk.csv",as.is=TRUE)
i <- data.final$year<=1996
j <- match(data.final$rd14[i],rd.xwalk$RD96)
data.final$rd14[i][!is.na(j)] <- rd.xwalk$RD97[j[!is.na(j)]]
#  check
subset(data.final, year<=1999 & rd==971)

#  2008->2009 change
rd.xwalk <- read.csv("LAPD2008xwalk.csv",as.is=TRUE)[,c("RD08","RD09")]
rd.xwalk <- subset(rd.xwalk, RD08!=RD09)
i <- data.final$year<=2008
j <- match(data.final$rd14[i],rd.xwalk$RD08)
data.final$rd14[i][!is.na(j)] <- rd.xwalk$RD09[j[!is.na(j)]]
#  check
subset(data.final, year<=2010 & rd==211)

write.csv(data.final,file="LAPD crime counts 1988-2014 merged.csv",
          row.names=FALSE,quote=FALSE)
