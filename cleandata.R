# read in LAPD crime data OCR'd from the LA public library
library(doParallel)

setwd("z:/articles/transit and crime/LAPDcrimedata")

lapl.data <-
foreach(year=1988:2005) %do%
{
   read.csv(paste(year," export ready.csv",sep=""))
}

# check any with extra lines
for(i in 1:length(lapl.data))
{
   print((1988:2005)[i])
   print(lapl.data[[i]][nrow(lapl.data[[i]]),])
}
