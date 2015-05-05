setwd("z:/articles/transit and crime/LAPDcrimedata")

library(rgdal)
library(lubridate)

load("maps.RData")
data <- read.csv("LAPD crime counts 1988-2010 merged.csv")

table(data$quarter,data$rd)
