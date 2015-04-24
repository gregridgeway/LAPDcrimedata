# read in LAPD crime data OCRd from the LA public library (1988-2005)
# read in LAPD incident data (2004-2010)
# merge the two to make a 1988-2010 dataset
setwd("z:/articles/transit and crime/LAPDcrimedata")


##############################################################################
# load data from LAPD incidents
incd.data <- read.csv("LAPD crime counts from incidents.csv")

##############################################################################
# load and polish data from LAPL, 1988-2005
library(doParallel)
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
library(lattice)
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
# process data from data.lacity.org, 2013-2014
data13 <- read.csv("LAPD_Crime_and_Collision_Raw_Data_for_2013.csv.gz",as.is=TRUE)


data14 <- read.csv("LAPD_Crime_and_Collision_Raw_Data_-_2014.csv.gz",  as.is=TRUE)

crimetype.xwalk <- read.csv("CrimeClassCode-CrimeTypeXWalk.csv",as.is=TRUE)

##############################################################################
# merge datasets, LAPL 1988-2004, LAPD incident counts 2005-2010
data.final <- 
   rbind(do.call(rbind,lapl.data[1:17]),
         subset(incd.data, !(quarter %in% c("2004Q1","2004Q2","2004Q3","2004Q4"))))

write.csv(data.final,file="LAPD crime counts 1988-2010 merged.csv",
          row.names=FALSE,quote=FALSE)
