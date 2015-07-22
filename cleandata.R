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
library(rjson)
library(geosphere)
library(maptools)
library(rgdal)

# shapefiles
LAPDmap09 <- readOGR("lapd reporting district.shp","lapd reporting district")
LAPDmap09$number <- as.numeric(as.character(LAPDmap09$number))
LAPDmap05 <- readOGR("lapdrd_05.shp","lapdrd_05")
proj4string(LAPDmap05) <- CRS("+proj=lcc +lat_1=35.46666666666667 +lat_2=34.03333333333333 +lat_0=33.5 +lon_0=-118 +x_0=2000000.0001016 +y_0=500000.0001016001 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
LAPDmap05 <- spTransform(LAPDmap05, LAPDmap09@proj4string)
LAPDmap05$REPDIST <- as.numeric(as.character(LAPDmap05$REPDIST))


##############################################################################
# load data from LAPD incidents
#    these data were collected from another study from 2004-2010
#    collapsed from incident data to RD, quarter, year, crime type
#    RDs from 2009 and 2010 have already been converted to 2005 RDs
incd.data <- read.csv("LAPD crime counts from incidents.csv")

##############################################################################
# load and polish data from LAPL, 1988-2005
lapl.data <-
foreach(year=1988:2005) %do%
{
   print(year)
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
   # only theft from person, not grand theft
   a$GTP <- with(a, theft.from.pers+purse.snatch)
   a$theft.from.pers <- NULL
   a$purse.snatch <- NULL
   a$HOM <- a$murder
   a$murder <- NULL
   a$ROBB <- with(a, robb.st+robb.oth)
   a$robb.st <- NULL
   a$robb.oth <- NULL
   
   a$rape <- NULL
   # theft missing recorded 2004-2007
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

# CONCLUSION: 2004 data looks okay, but 2005 LAPL data does not look reliable
#    Note LA Times story about downgrading in 2005, corrected in incident data

##############################################################################
# tabulate 2011-2014 data
##############################################################################
data11 <- read.csv("open_data_crm_2011.csv.gz",as.is=TRUE)
names(data11)[names(data11)=="DR..NO"] <- "DR.NO"
data11 <- subset(data11, Crm.Cd!=997)
data11$LOCATION     <- gsub("  *"," ",data11$LOCATION)
data11$Cross.Street <- gsub("  *"," ",data11$Cross.Street)
data11$LOCATION <- gsub("^ ","",data11$LOCATION)
data11$Cross.Street <- gsub("^ ","",data11$Cross.Street)
data11$x <- NA
data11$y <- NA
data11$street <- as.character(NA)
i <- data11$Cross.Street==""
data11$street[i]  <- data11$LOCATION[i]
i <- with(data11, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) < gsub("^(N|S|E|W) ","",Cross.Street)))
data11$street[i] <- with(data11[i,], paste(LOCATION,"AND",Cross.Street))
i <- with(data11, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) >= gsub("^(N|S|E|W) ","",Cross.Street)))
data11$street[i] <- with(data11[i,], paste(Cross.Street,"AND",LOCATION))

data12 <- read.csv("occ_data_crm_2012.csv.gz",as.is=TRUE)
names(data12)[names(data12)=="DR..NO"] <- "DR.NO"
data12 <- subset(data12, Crm.Cd!=997)
data12$LOCATION     <- gsub("  *"," ",data12$LOCATION)
data12$Cross.Street <- gsub("  *"," ",data12$Cross.Street)
data12$LOCATION <- gsub("^ ","",data12$LOCATION)
data12$Cross.Street <- gsub("^ ","",data12$Cross.Street)
data12$x <- NA
data12$y <- NA
data12$street <- as.character(NA)
i <- data12$Cross.Street==""
data12$street[i]  <- data12$LOCATION[i]
i <- with(data12, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) < gsub("^(N|S|E|W) ","",Cross.Street)))
data12$street[i] <- with(data12[i,], paste(LOCATION,"AND",Cross.Street))
i <- with(data12, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) >= gsub("^(N|S|E|W) ","",Cross.Street)))
data12$street[i] <- with(data12[i,], paste(Cross.Street,"AND",LOCATION))

data13 <- read.csv("LAPD_Crime_and_Collision_Raw_Data_for_2013.csv.gz",as.is=TRUE)
data13 <- subset(data13, Crm.Cd!=997)
data13$LOCATION     <- gsub("  *"," ",data13$LOCATION)
data13$Cross.Street <- gsub("  *"," ",data13$Cross.Street)
data13$LOCATION     <- gsub("^ ","",  data13$LOCATION)
data13$Cross.Street <- gsub("^ ","",  data13$Cross.Street)
data13$x <- as.numeric(gsub(".*, (.*)\\)","\\1",data13$Location.1))
data13$y <- as.numeric(gsub("\\((.*),.*", "\\1",data13$Location.1))
data13$street <- as.character(NA)
i <- data13$Cross.Street==""
data13$street[i]  <- data13$LOCATION[i]
i <- with(data13, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) < gsub("^(N|S|E|W) ","",Cross.Street)))
data13$street[i] <- with(data13[i,], paste(LOCATION,"AND",Cross.Street))
i <- with(data13, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) >= gsub("^(N|S|E|W) ","",Cross.Street)))
data13$street[i] <- with(data13[i,], paste(Cross.Street,"AND",LOCATION))
data13$Location.1 <- NULL

data14 <- read.csv("LAPD_Crime_and_Collision_Raw_Data_-_2014.csv.gz",as.is=TRUE)
data14 <- subset(data14, Crm.Cd!=997)
data14$LOCATION     <- gsub("  *"," ",data14$LOCATION)
data14$Cross.Street <- gsub("  *"," ",data14$Cross.Street)
data14$LOCATION     <- gsub("^ ","",  data14$LOCATION)
data14$Cross.Street <- gsub("^ ","",  data14$Cross.Street)
data14$x <- as.numeric(gsub(".*, (.*)\\)","\\1",data14$Location.1))
data14$y <- as.numeric(gsub("\\((.*),.*", "\\1",data14$Location.1))
data14$street <- as.character(NA)
i <- data14$Cross.Street==""
data14$street[i]  <- data14$LOCATION[i]
i <- with(data14, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) < gsub("^(N|S|E|W) ","",Cross.Street)))
data14$street[i] <- with(data14[i,], paste(LOCATION,"AND",Cross.Street))
i <- with(data14, (Cross.Street!="") & 
   (gsub("^(N|S|E|W) ","",LOCATION) >= gsub("^(N|S|E|W) ","",Cross.Street)))
data14$street[i] <- with(data14[i,], paste(Cross.Street,"AND",LOCATION))
data14$Location.1 <- NULL

data.1114 <- rbind(data11,data12,data13,data14)

# format dates
#   fix dates and add quarters
data.1114$DATE.OCC <- mdy(data.1114$DATE.OCC)
data.1114$quarter <- "Q1"
i <- with(data.1114, month(DATE.OCC) %in% 4:6)
data.1114$quarter[i] <- "Q2"
i <- with(data.1114, month(DATE.OCC) %in% 7:9)
data.1114$quarter[i] <- "Q3"
i <- with(data.1114, month(DATE.OCC) %in% 10:12)
data.1114$quarter[i] <- "Q4"
with(data.1114, table(month(DATE.OCC),quarter))
data.1114$quarter <- with(data.1114, paste0(year(DATE.OCC),quarter))

# make sure only crimes occuring in 2011-2014 are included
data.1114 <- subset(data.1114, year(DATE.OCC) %in% 2011:2014)

# get the coordinates for 2011-2012 crime incidents
gc.locations <- read.csv("geocode0914.csv",as.is=TRUE)
i <- which(year(data.1114$DATE.OCC) %in% 2011:2012)
j <- match(data.1114$street[i], gc.locations$street)
data.1114$x[i] <- gc.locations$x[j]
data.1114$y[i] <- gc.locations$y[j]
# fill in missing 2013 & 2014 (x,y) if geocodable
i <- which((year(data.1114$DATE.OCC) %in% 2013:2014) &
           is.na(data.1114$x))
j <- match(data.1114$street[i], gc.locations$street)
data.1114$x[i] <- gc.locations$x[j]
data.1114$y[i] <- gc.locations$y[j]

# spatial join with 2005 LAPD RD map
data.1114$rd.gc <- NA
i <- which(!is.na(data.1114$x))
a <- over(SpatialPoints(data.1114[i,c("x","y")],
                        proj4string=LAPDmap05@proj4string),
          LAPDmap05)
data.1114$rd.gc[i] <- a$REPDIST

# deal with those on boundaries or just outside shape
#   very slow - parallel process - about 8 hours
cl <- makeCluster(3)
registerDoParallel(cl)

obs.todo <- which(is.na(data.1114$rd.gc) & !is.na(data.1114$x))
a <-
foreach(i=obs.todo, .packages="geosphere") %dopar%
{
   # compute the distance from this point to all RDs
   d <- sapply(LAPDmap05$REPDIST, function(a)
        {
           dist2Line(round(data.1114[i,c("x","y")],5), 
                     subset(LAPDmap05,REPDIST %in% a))[,"distance"]
        })
   # drop those polygons that are not RDs
   d[is.na(LAPDmap05$REPDIST)] <- Inf
   return(LAPDmap05$REPDIST[which.min(d)])
}
data.1114$rd.gc[obs.todo] <- unlist(a)

stopCluster(cl)

# check for strange differences between current RD and 2005 RD
# get 2005 RD centroids
b <- t(sapply(LAPDmap05@polygons, function(x) x@labpt))
rownames(b) <- as.character(LAPDmap05$REPDIST)

# compute median centroid of current RDs
a <- with(subset(data.1114,year(DATE.OCC) %in% 2011:2012),
          cbind(sapply(split(x,RD),median,na.rm=TRUE),
                sapply(split(y,RD),median,na.rm=TRUE)))
plot(subset(LAPDmap05,floor(REPDIST/100)==1))
i <- which(as.numeric(rownames(a))<200)
points(a[i,])

i <- match(as.character(data.1114$RD[year(data.1114$DATE.OCC) %in% 2011:2012]),
           rownames(a))
d <- data.frame(x2011=a[i,1],y2011=a[i,2])
i <- match(as.character(data.1114$rd.gc[year(data.1114$DATE.OCC) %in% 2011:2012]),
           rownames(b))
d$x2005 <- b[i,1]
d$y2005 <- b[i,2]
d$d <- apply(d, 1, function(x) distHaversine(x[1:2],x[3:4]))

hist(log(d$d))
a <- subset(data.1114, (year(DATE.OCC) %in% 2011:2012))
a[d$d>3000 & d$d<3020,c("RD","rd.gc","street","y","x")]
a <- a[d$d>3000 & d$d<3020,c("RD","rd.gc","street","y","x")]

# set as NA those geocoded more than 3km from original RD
data.1114$x[year(data.1114$DATE.OCC) %in% 2011:2012][d$d>3000] <- NA
data.1114$y[year(data.1114$DATE.OCC) %in% 2011:2012][d$d>3000] <- NA

# xwalk those with missing (x,y)
#  for those missing coordinates use their reported RD and lookup the most
#  common 2005 RD associated with that RD
#  20+ minutes
cl <- makeCluster(3)
registerDoParallel(cl)

obs.todo <- which(is.na(data.1114$x))
a <-
foreach(i=obs.todo, .packages="lubridate") %dopar%
{
   b <- with(subset(data.1114, (RD==RD[i]) &
                               (year(DATE.OCC)==year(DATE.OCC[i]))),
             as.numeric(names(sort(-table(rd.gc)))[1]))
   if(is.null(b) || (length(b)==0)) b <- NA
   return(b)
}
data.1114$rd.gc[obs.todo] <- unlist(a)

stopCluster(cl)

# check join
plot(jitter(rd.gc)~jitter(RD),data=data.1114, 
     xlab="2011-4 RD", ylab="2005 RD", pch=".")
for(i in 100*(1:21)) {abline(v=i); abline(h=i)}

# swap the original and 2005 geocoded RDs
data.1114$rd.original <- data.1114$RD
data.1114$RD          <- data.1114$rd.gc

# get crime categories
crimetype.xwalk <- read.csv("CrimeClassCode-CrimeTypeXWalk.csv",as.is=TRUE)
# collapse crime codes into aggregated crime types
i <- match(data.1114$Crm.Cd,crimetype.xwalk$crimeclasscode)
data.1114$crimetype <- crimetype.xwalk$crimetype[i]
# check that those not linked are not key crime categories
table(data.1114$crimetype)
with(data.1114, sort(table(Crm.Cd.Desc[is.na(crimetype)])))

# tabulate crimes by type, RD, quarter
results <-
foreach(i.quarter=unique(data.1114$quarter)) %do%
{
   a <- with(subset(data.1114,quarter==i.quarter),
             table(RD,crimetype))
   a <- as.data.frame.matrix(a)
   a$quarter <- i.quarter
   a$rd <- rownames(a)
   return(a)      
}
data.1114.tab    <- do.call(rbind,results)
data.1114.tab$rd <- as.integer(data.1114.tab$rd)

# just use the crime types shared with earlier years (i.e. drop rape)
data.1114.tab <- subset(data.1114.tab, select=names(lapl.data[[1]]))

##############################################################################
# merge datasets, LAPL 1988-2004, LAPD incident counts 2005-2014
data.final <- 
   rbind(do.call(rbind,lapl.data[1:17]),
         subset(incd.data, !(quarter %in% c("2004Q1","2004Q2","2004Q3","2004Q4"))),
         data.1114.tab)

data.final$year <- as.numeric(substring(data.final$quarter,1,4))
data.final$Q    <- as.numeric(substring(data.final$quarter,6,6))

data.final$rd <- as.character(data.final$rd)

##############################################################################
# use crosswalk for 1988-2004,2006-2008 to 2005 RDs
##############################################################################
xwalk <- read.csv("final RD xwalk 1988-2008.csv",as.is=TRUE)
for(i.year in c(1988:2004,2006:2008))
{
   xwalk.var <- strsplit(xwalk[,paste0("RD",i.year)],";")
   i <- sapply(data.final$rd[data.final$year==i.year],
          function(x)
          {
            j <- which(sapply(xwalk.var, function(y) {x %in% y}))
            if(length(j)==0) j <- NA
            return(j)
          })
   data.final$rd[data.final$year==i.year] <- xwalk$RD2005[i]
}

# several 2005 RDs don't appear in shapefile
#    almost all seem to be coders using 2004 RDs
#    eg RD1742 has several crimes in 2005, but should be RD1997
i <- which(data.final$year==2005 & 
           !(data.final$rd %in% LAPDmap05$REPDIST) &
           !(data.final$rd %in% as.character(100*1:19)))
xwalk.var <- strsplit(xwalk$RD2004,";")
j <- sapply(data.final$rd[i],
          function(x)
          {
            j <- which(sapply(xwalk.var, function(y) {x %in% y}))
            if(length(j)==0) j <- NA
            return(j)
          })
data.final$rd[i] <- xwalk$RD2005[j]

data.final <- subset(data.final, !is.na(rd))

# get final dataset merging any RDs that need to be merged
#  in some years RDs might be 121;122 and another 122;123
#  need to make one 121;122;123 RD for all years
all.rds <- sort(unique(data.final$rd))
i <- 1
while(i<length(all.rds))
{
   xwalk.var <- strsplit(all.rds,";")
   # find other merged RDs that have this RD
   j <- sapply(xwalk.var, intersect, y=xwalk.var[[i]])
   j <- which(sapply(j, length)>0)
   if(length(j)>1)
   {
      # collapse them all into one merged RD
      all.rds[i] <- paste(sort(unique(unlist(xwalk.var[j]))),collapse=";")
      # drop the others that have been merged
      all.rds <- all.rds[-setdiff(j,i)]
   } else
   {
      i <- i + 1
   }
}
# put them in order
i <- order(sapply(strsplit(all.rds,";"),function(x) as.numeric(x[1])))
all.rds <- all.rds[i]

# check that all RDs are in "all.rds"
xwalk.var <- strsplit(all.rds,";")
a <- data.frame(rd=sort(unique(data.final$rd)),count=NA,
                stringsAsFactors=FALSE)
for(i.rd in 1:nrow(a))
{
   this.rd <- strsplit(a$rd[i.rd],";")
   j <- sapply(xwalk.var, intersect, y=this.rd[[1]])
   j <- which(sapply(j, length)>0)
   a$count[i.rd] <- length(j)
}
range(a$count) # should all be 1

##############################################################################
# assemble final analytical table
##############################################################################
data.final <- subset(data.final,!is.na(rd))
# drop headquarter RDs (crimes documented at a station)
all.rds <- setdiff(all.rds, c(110,100*1:19,9999))

# finish all the merging of RDs across years (e.g. 111;112;121)
rd.split  <- strsplit(data.final$rd,";")
all.split <- strsplit(all.rds,";")

cl <- makeCluster(3)
registerDoParallel(cl)
j <- parLapply(cl=cl,
               all.split, 
               function(x,rd.split) 
                  which(sapply(rd.split, 
                        function(y) length(intersect(y,x))>0)),
               rd.split=rd.split)
stopCluster(cl)

data.final$rd05 <- NA
for(i in 1:length(all.rds))
{
   data.final$rd05[j[[i]]] <- all.rds[i]
}

data.tab <- expand.grid(rd=all.rds,year=1988:2014,Q=1:4,
#                        AGG=0,BTFV=0,BURG=0,GTA=0,
#                        GTP=0,HOM=0,ROBB=0,
                        stringsAsFactors=FALSE)
for(x in c("AGG","BTFV","BURG","GTA","GTP","HOM","ROBB"))
{
   form <- paste(x,"~rd05+year+Q")
   a  <- aggregate(formula(form),data=data.final,FUN=sum)
   data.tab <- merge(data.tab,a,all=TRUE,
                     by.x=c("rd","year","Q"),by.y=c("rd05","year","Q"))
}
# replace NAs with 0s. NAs indicate rd, year, Q combos with no crime reports
data.tab[is.na(data.tab$AGG),c("AGG","BTFV","BURG","GTA","GTP","HOM","ROBB")] <- 0

# put in order
i <- order(sapply(strsplit(data.tab$rd,";"), function(x) as.numeric(x[1])))
data.tab <- data.tab[i,]

write.csv(data.tab,file="LAPD crime counts 1988-2014 merged.csv",
          row.names=FALSE,quote=FALSE)
