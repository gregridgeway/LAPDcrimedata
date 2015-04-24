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
station.map$LINE[station.map$LINE=="purple"] <- "red"
station.map$col <- station.map$LINE
station.map$col[station.map$LINE=="expo"] <- "light blue"

# plot the stations
par(mai=c(0,0,0,0))
plot(LAPDmap.new,new=FALSE)
points(station.map,col=station.map$col,pch=16)

# read in station history
station.hx <- read.csv("LA station history.csv",as.is=TRUE)
station.hx$Date.opened <- mdy(station.hx$Date.opened)

# check that station numbers link up with shapefiles
for(x in c("blue","gold","green","red","purple","expo"))
{
   print(x)
   i <- match(subset(station.hx,Line==x)$StopNum,
              station.map[[x]]@data$STOPNUMNEW)
   print(sort(i)-1:length(i))
   i <- match(station.map[[x]]@data$STOPNUMNEW,
              subset(station.hx,Line==x)$StopNum)
   print(sort(i)-1:length(i))
}

# match stations to RDs
station.hx$rd <- NA
station.join <- over(station.map$blue,LAPDmap.new)
a <- subset(station.map$blue,!is.na(station.join$number))
points(a,col="purple",pch=16)

for(x in c("blue","gold","green","rp","expo"))
{
   station.join <- over(station.map[[x]],LAPDmap.new)
   station.join$number <- as.numeric(as.character(station.join$number))
   i <- match(station.map[[x]]@data$STOPNUMNEW, station.hx$StopNum)
   i <- i[!is.na(station.join$number)]
   station.hx$rd[i] <- with(station.join, number[!is.na(station.join$number)])
}

# check that RDs are correctly linked to stations
par(mai=c(0,0,0,0))
plot(LAPDmap.new,new=FALSE)
col.map <- c(blue="blue",gold="gold",green="green",rp="red",expo="light blue")
for(x in c("blue","gold","green","rp","expo"))
{
   a <- with(station.hx, rd[Line==x & !is.na(rd)])
   plot(subset(LAPDmap.new,number %in% a),col=col.map[x],add=TRUE)
   points(station.map[[x]],col="black",bg=col.map[x],pch=21)
}



d <- matrix(NA,nrow=length(unique(LAPDmap.new@data$number)),
               ncol=nrow(station.map$rp@data))
rownames(d) <- unique(LAPDmap.new@data$number)
for(i in 1:nrow(d))
{
   a <- dist2Line(station.map$rp, 
                  subset(LAPDmap.new,number==rownames(d)[i]))
   d[i,] <- a[,"distance"]
}
# examine RDs close to stations
par(mai=c(0,0,0,0))
plot(subset(LAPDmap.new,floor(number/100) %in% c(1,2,6,11,15,20)),new=FALSE)
#for(x in c("blue","gold","green","rp","expo"))
for(x in c("rp"))
{
   a <- names(d)[d<200]
   plot(subset(LAPDmap.new,number %in% a),col=col.map[x],add=TRUE)
   points(station.map[[x]],col="black",bg=col.map[x],pch=21)
}

plot(subset(LAPDmap.new,floor(number/100)==6),col="salmon",add=TRUE)


library(ggplot2)
ggplot(LAPDmap.new, aes(x = long, y = lat)) + 
   geom_path(aes(group = group))
   
