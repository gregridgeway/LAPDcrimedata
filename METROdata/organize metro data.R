setwd("z:/articles/transit and crime/METROdata")
library(rgdal)
library(maptools)
library(lubridate)
library(geosphere)

# organize spatial data
LAPDmap09 <- readOGR("../LAPDcrimedata/lapd reporting district.shp","lapd reporting district")
LAPDmap09$number <- as.numeric(as.character(LAPDmap09$number))
LAPDmap05 <- readOGR("../LAPDcrimedata/lapdrd_05.shp","lapdrd_05")
proj4string(LAPDmap05) <- CRS("+proj=lcc +lat_1=35.46666666666667 +lat_2=34.03333333333333 +lat_0=33.5 +lon_0=-118 +x_0=2000000.0001016 +y_0=500000.0001016001 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
LAPDmap05 <- spTransform(LAPDmap05, LAPDmap09@proj4string)
LAPDmap05$REPDIST <- as.numeric(as.character(LAPDmap05$REPDIST))


##############################################################################
# RAIL STATION DATA
##############################################################################

# read in rail stations
station.map <- list()
station.map$blue  <- readOGR("BlueLineSta0412.shp", "BlueLineSta0412")
station.map$gold  <- readOGR("GoldLineSta0412.shp", "GoldLineSta0412")
station.map$green <- readOGR("GreenLineSta0412.shp","GreenLineSta0412")
station.map$rp    <- readOGR("RPLinesSta0412.shp",  "RPLinesSta0412")
station.map$expo  <- readOGR("ExpoLineSta0512.shp", "ExpoLineSta0512")
station.map$blue  <- spTransform(station.map$blue,  LAPDmap.new@proj4string)
station.map$gold  <- spTransform(station.map$gold,  LAPDmap.new@proj4string)
station.map$green <- spTransform(station.map$green, LAPDmap.new@proj4string)
station.map$rp    <- spTransform(station.map$rp,    LAPDmap.new@proj4string)
station.map$expo  <- spTransform(station.map$expo,  LAPDmap.new@proj4string)

# merge stations into one point shape file
#   standardize feature names
names(station.map$blue@data)[names(station.map$blue@data)=="STOPNUM"]<-"OLDSTOPNUM"
station.map$rp@data$STNSEQ   <- NULL
station.map$blue@data$DIR    <- NULL
station.map$gold@data$DIR    <- NULL
station.map$blue@data$STNSEQ <- NULL
station.map$gold@data$STNSEQ <- NULL
station.map$expo@data$STASEQ <- NULL
names(station.map$expo)[names(station.map$expo)=="LATITUDE"]  <- "POINT_Y"
names(station.map$expo)[names(station.map$expo)=="LONGITUDE"] <- "POINT_X"
a <- spRbind(station.map$blue,
             station.map$gold)
a <- spRbind(a,station.map$green)
a <- spRbind(a,station.map$rp)
a <- spRbind(a,station.map$expo)
station.map <- a

station.map$LINE <- tolower(as.character(station.map$LINE))
station.map$LINE[station.map$LINE=="red/purple"] <- "red"
station.map$LINE[station.map$LINE=="purple"]     <- "red"
station.map$col <- station.map$LINE
station.map$col[station.map$LINE=="expo"]        <- "light blue"

station.map <- spTransform(station.map, LAPDmap05@proj4string)

# plot the stations
par(mai=c(0,0,0,0))
plot(LAPDmap05,new=FALSE)
points(station.map,col=station.map$col,pch=16)

##############################################################################
# read in station history
station.hx <- read.csv("LA station history.csv",as.is=TRUE)
station.hx$Date.opened <- mdy(station.hx$Date.opened)
station.hx$Line[station.hx$Line=="rp"] <- "red"

# check that station numbers link up with shapefiles
for(x in c("blue","gold","green","red","expo"))
{
   print(x)
   i <- match(subset(station.hx,Line==x)$StopNum,
              subset(station.map,LINE==x)$STOPNUMNEW)
   print(sort(i)-1:length(i))
   i <- match(subset(station.map,LINE==x)$STOPNUMNEW,
              subset(station.hx,Line==x)$StopNum)
   print(sort(i)-1:length(i))
}

# add opening date to station shapefile
i <- match(paste(station.map$LINE,station.map$STOPNUMNEW),
           paste(station.hx$Line, station.hx$StopNum))
station.map$open.date <- station.hx$Date.opened[i]

##############################################################################
# match stations to RDs

# check which stations fall within an RD
station.join <- over(station.map,LAPDmap05)
a <- subset(station.map,!is.na(station.join$REPDIST))
points(a,col="purple",pch=16)

# link RDs to earliest nearby station opening
rd.dist <- data.frame(rd=sort(unique(LAPDmap05$REPDIST)),
                      StopNum=NA,
                      Line=NA,
                      open.date=ymd("2051-06-18"))
rd.dist$open.date[] <- NA
max.distance <- 200
# check RD119
for(i.rd in 1:nrow(rd.dist))
{
   print(rd.dist$rd[i.rd])
   # compute distance
   d <- dist2Line(station.map, 
                  subset(LAPDmap05,REPDIST==rd.dist$rd[i.rd]))
   # set distance to 0 if station inside the RD
   #   dist2Line() will compute to boundary which can be large
   i <- which(station.join$REPDIST==rd.dist$rd[i.rd])
   if(length(i)>0) d[i,"distance"] <- 0
   # which stations are within max.distance meters of the edge of an RD
   i <- which(d[,"distance"]<max.distance)

   # show how R is matching RDs to stations
   if(TRUE & length(i)>0)
   {
      par(mai=c(0,0,1,0))
      # plot the current RDs district
      plot(subset(LAPDmap05,floor(REPDIST/100) %in% floor(rd.dist$rd[i.rd]/100)),
           new=FALSE,main=floor(rd.dist$rd[i.rd]/100))
      # highlight the current RD
      plot(subset(LAPDmap05,REPDIST == rd.dist$rd[i.rd]),
           add=TRUE,col="light pink")
      # overlay the stations
      points(station.map,col=station.map$col,pch=16)
      # highlight stations within max.distance meters
      points(subset(station.map,d[,"distance"]<max.distance),
             bg=station.map$col[i],col="black",pch=21,cex=2)
      #Sys.sleep(10)
   }

   # get the station that opened first
   if(length(i)>0)
   {
      j <- which.min(station.map$open.date[i])
      rd.dist$open.date[i.rd] <- station.map$open.date[i[j]]
      rd.dist$StopNum[i.rd]   <- station.map$STOPNUMNEW[i[j]]
      rd.dist$Line[i.rd]      <- station.map$LINE[i[j]]
   }
}

# add line and open date to LAPDmap
i <- match(LAPDmap05$REPDIST,rd.dist$rd)
LAPDmap05$Line      <- rd.dist$Line[i]
LAPDmap05$open.date <- rd.dist$open.date[i]

# show map, check stations map
col.map <- c(blue="blue",expo="light blue",gold="gold",green="green",red="red")
par(mai=c(0,0,0,0))
plot(LAPDmap05,new=FALSE)
plot(subset(LAPDmap05,!is.na(Line)),
     add=TRUE,col=col.map[LAPDmap05$Line[!is.na(LAPDmap05$Line)]])
# overlay the stations
points(station.map,col=station.map$col,pch=16)
points(station.map,col="black")

# zoom in
par(mai=c(0,0,0,0))
a <- with(rd.dist, unique(floor(rd[!is.na(Line)]/100)))
plot(subset(LAPDmap05,floor(REPDIST/100) %in% c(a,12)),new=FALSE)
plot(subset(LAPDmap05,!is.na(Line)),
     col=col.map[LAPDmap05$Line[!is.na(LAPDmap05$Line)]],
     add=TRUE)
# overlay the stations
points(station.map,col=station.map$col,pch=16)
points(station.map,col="black")

write.csv(rd.dist,file="RD station xwalk.csv",
          row.names=FALSE,quote=FALSE)

save(station.map,LAPDmap05,rd.dist,file="maps.Rdata")
