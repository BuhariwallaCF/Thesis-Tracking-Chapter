# 2013-10-01 
# CFB initial workings of Range Test Data. 
### Updated 2015-03-30

# Load in range test files from CURRENT detection files 
rt <- read.csv("~/Desktop/Data/vm - shared/VUE/Exports/VueExport_timeCorrectedDB_2014-09-25.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")

deploy <- read.csv("~/Desktop/Data/Cape Breton/Acoustics/Excel Files/range test/rangetest_moorings.csv", stringsAsFactors = FALSE)

#~~~~REQUIRED PACKAGES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(lubridate)
require(plyr)
require(ggplot2)
require(gridExtra)

#### Tag info####
#1125594 31340 (V13_H1) -- depth = 11m ; step 1 (6 hours) : 5 second delay; Step 2 (180 days) : 240 - 360 second delay  
#1125593 31339 (V13_H2) -- depth = 4m  ; step 1 (6 hours) : 5 second delay; Step 2 (180 days) : 240 - 360 second delay  
#1161687 38001 (V9_H) & 38000 (V9_L) -- depth = 4 m ; 30 second delay == 68 seconds between transmissions --> 1 transmission every 34 seconds
#1159871 33162 (V13_L) -- depth = 11 m ; step 1 (891 days) : 50 - 130 second delay --> 1 transmission every 



####~~~~CLEANING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# CLEAN UP THE DF TO MAKE IT WORKABLE

# [1] "Date.and.Time..UTC." "Receiver"            "Transmitter"         "Transmitter.Name"    "Transmitter.Serial"  "Sensor.Value"       
#"Sensor.Unit"         "Station.Name"        "Latitude"     [10] "Longitude"          
names(rt) <- c("date", "receiver", "t", "t.name", "t.sn", "sensor", "unit","station", "lat", "long")

# convert date to posix
rt$date <- ymd_hms(rt$date, tz = "UTC")

# remove rows that i don't want.. 
rt <- rt[-c(4,9,10)]

## create seperate id column & remove the "a69-1303- & A69-1102-" ### why did i use as factor? - CB 2015-03-30 
rt$id <- as.factor(unlist(strsplit(rt$t, split = "-"))[3*(1:length(rt$t))]) 
rt$receiver <- as.factor(unlist(strsplit(rt$receiver, split = "-"))[2*(1:length(rt$receiver))])



## isolate all of the tags we used for the range test
rt <- rt[which(rt$id %in% c("38001", "38000", "33162", "31340", "31339")),]

## isolate out all of the dates we did the range test on (range test 2) 
start2 <- ymd_hms("2013-07-23 19:53:00") # deployment time plus 1 hour
start <- ymd_hms("2013-07-24 00:53:00")# deployment time plus 6 hours to end first step of tag transmission # this isn't necessary because tags seem to be firing every 4-6 min
end <- ymd_hms("2013-07-26 17:00:00") # Recovery efforts startd at 18:00 --> minus an hour to eliminate boat noise

rt <- rt[which(rt$date >= start2 & rt$date <= end),]

## rename the station names based on the receiver and tag distance 
for(i in 1:length(rt$date)){
  # print(i) # good debugging tool 
  if(rt$id[i] %in% c("33162", "38001", "38000")){
    rt$station[i] <- deploy$lp.distance[which(deploy$receiver == rt$receiver[i])]
} else {
  rt$station[i] <- deploy$hp.distance[which(deploy$receiver == rt$receiver[i])]
}
}


rt <- rt[which(rt$date >= start2 & rt$date <= end),]

time <- as.duration(end-start2)

#### expected vs observed detections ####
ndetects <- ddply(rt, .(id, station), nrow)
ndetects.day <- ddply(rt, .(id, station, day(date)), nrow) ### this is so I can plot daily detections

tag.delay <- deploy[deploy$receiver %in% unique(rt$id),] ## isolate the id's of tags used (tag and receiver both labelled receiver in deploy df)
tag.delay <- tag.delay[-c(2:7)]                         ## just want the delay intervals 
names(tag.delay)<- c("id","delay1","delay2")

ndetects <- merge(ndetects, tag.delay, by = "id" ) # incorporate delays into other df
ndetects.day <- merge(ndetects, tag.delay, by = "id" ) # same thing 

ndetects$max.det <- time/dseconds(ndetects$delay1) # max number of detections
ndetects$min.det <- time/dseconds(ndetects$delay2) # min number of detections
ndetects$mid.det <- time/dseconds((ndetects$delay1 + ndetects$delay2)/2)

ndetects$max.eff <- (ndetects$V1/ndetects$min.det)*100
ndetects$min.eff <- (ndetects$V1/ndetects$max.det)*100
ndetects$mid.eff <- (ndetects$V1/ndetects$mid.det)*100


p.min <- ggplot(ndetects, aes(station, min.eff, group = id, col = id, shape = id)) + geom_point(size = 4) + theme_bw() ##### THIS IS WHERE THE KEY DATA ARE 
p.mid <- ggplot(ndetects, aes(station, mid.eff, group = id, col = id, shape = id)) + geom_point(size = 4) + theme_bw() ##### THIS IS WHERE THE KEY DATA ARE 
p.max <- ggplot(ndetects, aes(station, max.eff, group = id, col = id, shape = id)) + geom_point(size = 4) + theme_bw() ##### THIS IS WHERE THE KEY DATA ARE 

p.min
p.mid
p.max

###install gridExtra
grid.arrange(p.min, p.mid, p.max, ncol=3)

ggplot(ndetects, aes(station, min.eff, group = id, col = id, shape = id)) + geom_point(size = 4) + theme_bw() ##### THIS IS WHERE THE KEY DATA ARE 




####old plots####

#### inital plot to get a feel for the detection data####

rt.p <- ggplot(rt, aes(x = date, y = station))
rt.p + geom_point() + facet_grid(~ id)



#2013MR_10 was a different distance depending on what tags you used 

## 31340 (V13_H1) -- depth = 11m ; 31339 (V13_H2) -- depth = 4m; 38001 (V9_H) & 38000 (V9_L) -- depth = 4 m ; 33162 (V13_L) -- depth = 11 m  

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

p1<- ggplot(rt[rt$id %in% c("33162", "38001", "38000"),], aes(date, station)) + geom_point()
p1 + facet_grid(.~id)

p2 <- ggplot(rt[rt$id %in% c("31340", "31339"),], aes(date, station)) + geom_point()
p2 + facet_grid(.~id)


#### proportion of detections per day, per tag, per expected efficiency ####

#### plot detections per tag per stion ####





#### to do ####
# work on Range test 1 at the mouth to determine detection range and efficiencies 




  
  