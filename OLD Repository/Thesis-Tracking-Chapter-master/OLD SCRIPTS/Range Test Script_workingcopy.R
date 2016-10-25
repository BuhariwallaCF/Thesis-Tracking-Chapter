# 2013-10-01 
# CFB initial workings of Range Test Data. 
setwd("~/Desktop/Data/R/acoustics/Range Test")

# Load in range test files 
rt <- read.csv("~/Desktop/Data/Vemco/VUE/CSV/March 2014/All detections to 2013-11-09.csv", stringsAsFactors = FALSE)
#~~~~REQUIRED PACKAGES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(lubridate)
require(plyr)
require(ggplot2)

#~~~~CLEANING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CLEAN UP THE DF TO MAKE IT WORKABLE

# [1] "Date.and.Time..UTC." "Receiver"            "Transmitter"         "Transmitter.Name"    "Transmitter.Serial"  "Sensor.Value"       
#[7]"Sensor.Unit"         "Station.Name"        "Latitude"     [10] "Longitude"          
names(rt) <- c("date", "receiver", "t", "t.name", "t.sn", "sensor", "unit","station", "lat", "long")

# convert date to posix ** 
rt$date <- ymd_hms(rt$date, tz = "UTC")
#** Check the format as vemco changes it periodically

# remove rows that i don't want.. 
rt <- rt[-c(4,9,10)]

## create seperate id column & remove the "a69-1303- & A69-1102-"

rt$id <- as.numeric(unlist(strsplit(rt$t, split = "-"))[3*(1:length(rt$t))]) #? as int ? 

######## above is applicable to all vrl files; below is for range test only (seperate out sn's i don't want) ###################
rt.backup <- rt
## isolate all of the tags we used for the range test
tags <- c(38001, 38000, 33162, 31340, 31339) #? as int ? 

rt.trash <- rt[]
#select rt by column id, 

rt.backup <- rt

## isolate out all of the dates we did the range test on (range test 2) 

start <- ymd("2013-07-13")
rt <- rt[which(rt$date >= start),]


# inital plot to get a feel for the detection data

rt.p <- ggplot(rt, aes(x = date, y = station))
rt.p + geom_point() + facet_grid(~ id)
#

## 31340 (V13_H1); 31339 (V13_H2); 38001 (V9_H); 38000 (V9_L); 33162 (V13_L) 

p.31340 <- ggplot(rt[rt$id == "31340",], aes(date, station)) + geom_jitter()
p.31340

p.31339 <- ggplot(rt[rt$id == "31339",], aes(date, station)) + geom_jitter()
p.31339

p.38001 <- ggplot(rt[rt$id == "38001",], aes(date, station)) + geom_jitter()
p.38001

p.38000 <- ggplot(rt[rt$id == "38000",], aes(date, station)) + geom_jitter()
p.38000

p.33162 <- ggplot(rt[rt$id == "33162",], aes(date, station)) + geom_jitter()
p.33162


## ok.. it may be easier to split data frame into the low and high power tags and figure out station distances from each other.

high <- rt[rt$id %in% c("31340","31339"),]
low <- rt[rt$id %in% c("38001", "38000", "33162"),]




#~~~~~~~~PLOTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trying to plot a time span.. didn't get this to work. 
start <- ymd_hms("2012-09-01 00:00:00")
end <- ymd_hms("2012-12-31 23:59:59")
span <- end - start

p <- ggplot(rt.1[rt.1$date == span,], aes(x = date, y = station))
p + geom_point(aes(colour = factor(t))) #+ facet_grid(.~station)


# PLOT TRANSMITTER TO STATION 

p.48418 <-
  
  