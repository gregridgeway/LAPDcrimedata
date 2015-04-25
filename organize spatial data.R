setwd("z:/articles/transit and crime/LAPDcrimedata")
library(rgdal)
library(lubridate)
library(geosphere)

# organize spatial data
LAPDmap.new <- readOGR("lapd reporting district.shp","lapd reporting district")
LAPDmap.old <- readOGR("lapdrd_05.shp","lapdrd_05")

# put maps on same coordinate system
#   guessed the old projection by finding one neat Los Angeles
#   http://spatialreference.org/ref/epsg/?search=california&srtext=Search
proj4string(LAPDmap.old) <- CRS("+proj=lcc +lat_1=35.46666666666667 +lat_2=34.03333333333333 +lat_0=33.5 +lon_0=-118 +x_0=2000000.0001016 +y_0=500000.0001016001 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
LAPDmap.old <- spTransform(LAPDmap.old, LAPDmap.new@proj4string)

# make sure that both maps project to the same area
plot(LAPDmap.new)
plot(LAPDmap.old,col="red",add=TRUE)

# check changes in map
plot(subset(LAPDmap.new,number %in% c(711,713,714,715)))
plot(subset(LAPDmap.old,REPDIST %in% c(691,692,693,694)),col="red",add=TRUE)
plot(LAPDmap.new)
plot(subset(LAPDmap.old,REPDIST %in% c(691,692,693,694)),col="red",add=TRUE)

# read in LAPD old/new RD crosswalk
RDxwalk <- read.csv("lapd2009old2new rd xwalk.csv",as.is=TRUE)
a <- with(RDxwalk, NEW.RD[NEW.RD!=OLD.RD])
a <- a[!is.na(a)]
plot(LAPDmap.new)
plot(subset(LAPDmap.new,number %in% a),col="red",add=TRUE)

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

# plot the stations
par(mai=c(0,0,0,0))
plot(LAPDmap.new,new=FALSE)
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
station.join <- over(station.map,LAPDmap.new)
a <- subset(station.map,!is.na(station.join$number))
points(a,col="purple",pch=16)

# link RDs to earliest nearby station opening
rd.dist <- data.frame(rd=sort(unique(LAPDmap.new$number)),
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
                  subset(LAPDmap.new,number==rd.dist$rd[i.rd]))
   # set distance to 0 if station inside the RD
   #   dist2Line() will compute to boundary which can be large
   i <- which(station.join$number==rd.dist$rd[i.rd])
   if(length(i)>0) d[i,"distance"] <- 0
   # which stations are within max.distance meters of the edge of an RD
   i <- which(d[,"distance"]<max.distance)

   # show how R is matching RDs to stations
   if(TRUE & length(i)>0)
   {
      par(mai=c(0,0,1,0))
      # plot the current RDs district
      plot(subset(LAPDmap.new,floor(number/100) %in% floor(rd.dist$rd[i.rd]/100)),
           new=FALSE,main=floor(rd.dist$rd[i.rd]/100))
      # highlight the current RD
      plot(subset(LAPDmap.new,number == rd.dist$rd[i.rd]),
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
i <- match(LAPDmap.new$number,rd.dist$rd)
LAPDmap.new$Line      <- rd.dist$Line[i]
LAPDmap.new$open.date <- rd.dist$open.date[i]

# show map, check stations map
col.map <- c(blue="blue",expo="light blue",gold="gold",green="green",red="red")
par(mai=c(0,0,0,0))
plot(LAPDmap.new,new=FALSE)
plot(subset(LAPDmap.new,!is.na(Line)),
     add=TRUE,col=col.map[LAPDmap.new$Line[!is.na(LAPDmap.new$Line)]])
# overlay the stations
#points(station.map,col=station.map$col,pch=16)
points(station.map,col="black")

# zoom in
par(mai=c(0,0,0,0))
a <- with(rd.dist, unique(floor(rd[!is.na(Line)]/100)))
plot(subset(LAPDmap.new,floor(number/100) %in% a),new=FALSE)
plot(subset(LAPDmap.new,!is.na(Line)),
     col=col.map[LAPDmap.new$Line[!is.na(LAPDmap.new$Line)]],
     add=TRUE)
# overlay the stations
#points(station.map,col=station.map$col,pch=16)
points(station.map,col="black")

save(station.map,LAPDmap.new,rd.dist,RDxwalk,file="maps.Rdata")
