# read in LAPD crime data OCR'd from the LA public library
library(doParallel)

setwd("z:/articles/transit and crime/LAPDcrimedata")

lapl.data <-
foreach(year=1988:2005) %do%
{
   read.csv(paste(year," export ready.csv",sep=""))
}

# load data from LAPD incidents
