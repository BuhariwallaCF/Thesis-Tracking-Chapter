# Colin Buhariwalla 
# 2014-05-05 -- Mira River Overwintering Script -- Start

# wd : setwd("~/Desktop/Data/R/Workspace"")
# Script: /Users/colinbuhariwalla/Desktop/Data/R/Scripts

require(lubridate)
require(ggplot2)
require(doBy)
require(plyr)
require(dplyr)
require(jpeg)
require(reshape2)

require(ggmap)

mr.df <- read.csv("data/mrdf.csv", stringsAsFactors = FALSE)
tag.df <- read.csv("data/tagdf.csv", stringsAsFactors = FALSE)
stn.df <- read.csv("data/stndf.csv", stringsAsFactors = F)




source("functions/dates_and_times_fun.R")
mr.df  <- dates_and_times_fun(mr.df)
tag.df <- dates_and_times_fun(tag.df)
stn.df <- dates_and_times_fun(stn.df)

# format data and metadata - parse dates, set factors, create intervals 

stn.df <- stn.df[!stn.df$station %in% c(350, 200, 100, 13.5),]









#ID's for all of the depth tags, in the mira area
depth.tags <- as.list(as.character(c(5720:5739)))


other.tags <- as.list(as.character(c(48407:48411, 48418, 33158:33162)))

ow.stations <- c("010", "011", "011.5") #overwintering array
outside.array <- c("004", "005", "006", "007", "008", "009", "012", "013", "014", "015", "016", "017", "018", "019")


#### calculate the migration times for arrival and exit from the overwintering grounds ####


 ## FUNCTION WORKS ON INIVIDUAL DFs based on tag ID
  #descent <- df[which(df$date >= ymd("2014-01-01") & df$station %in% outside.array),]
  #first.out <- which(df$date == descent$date[1]) # the first detection outside the overwintering array
  #last.ow <- df$date[first.out - 1] # last detection since the overwintering array 
  #albert.arrive <- descent$date[match(("007"), descent$station)] # finding the first detection at Albert Bridge by searching for the first appearance of "007". 
  #ow.down.mig <- albert.arrive - last.ow
leave <- function(df){
  last.date <- function(x){tail(x,1)} # function to give me the last value of a vector/df 
  first.date <- function(x){head(x, 1)}

  descent <- df[df$date >= ymd("2014-01-01") & df$station %in% outside.array,]
  
  
  first.out <- first.date(descent)
  
  last.ow <- last.date(df[which(df$date < first.out$date & df$date > ymd("2014-01-01")),]) # last overwintering date -- defined by detection on outside array
  
  last.ow$albert.first <- descent$date[match(("007"), descent$station)]
  
  last.ow$mig.dur <- as.duration(last.ow$albert.first - last.ow$date)
  
  last.ow$fst.out.s <- first.out$station
  last.ow$fst.out.d <- first.out$date
  
  return(last.ow)
}




leave.48407 <- leave(ow[ow$id == 48407,])
leave.48408 <- leave(ow[ow$id == 48408,])
leave.48409 <- leave(ow[ow$id == 48409,]) #
leave.48410 <- leave(ow[ow$id == 48410,]) 
leave.48411 <- leave(ow[ow$id == 48411,]) # 

leave.5739 <- leave(ow[ow$id == 5739,]) ### shiiitttt see that the first detection outside the ow area is a suspect detection
leave.33158 <- leave(ow[ow$id == 33158,]) #  
leave.33159 <- leave(ow[ow$id == 33159,]) #
leave.33160 <- leave(ow[ow$id == 33160,])
leave.33161 <- leave(ow[ow$id == 33161,]) #
leave.33162 <- leave(ow[ow$id == 33162,])

leave.df <- rbind( leave.48407, leave.48408, leave.48409, leave.48410, leave.48411,
                   leave.5739, leave.33158, leave.33159, leave.33160, leave.33161, leave.33162)


# day they left MR_07 and day they settled into the overwintering area

enter <- function(df){  
  
  last.date <- function(x){tail(x,1)} # function to give me the last value of a vector/df 
  first.date <- function(x){head(x, 1)}
  
  ascent <- df[which(df$date <= ymd("2013-12-31") & df$station %in% outside.array),] # this takes the last date it was detected outside the overwintering area and then finds that date
  
  last.out <- last.date(ascent) ## last detection outside the overwintering area... 
  
  first.ow <- df[which(df$date > last.out$date & df$date <= ymd("2013-12-31")),] # the ow detections 
  ow.first <- first.date(first.ow) # first date it is completely detected in the ow area.
  
  albert <- df[which(df$date <= ymd("2013-12-31") & df$station == "007"),]
  
ow.first$albert.last <- last.date(albert$date)

 
ow.first$mig.duration <- ow.first$date - ow.first$albert.last

return(ow.first)
}


#### 4 loops ####

tag.id.2 <- unique(ow$id)

ow.upmigration = as.data.frame(matrix(0, ncol = 12, nrow = 30))

for(i in 1:length(tag.id.2))
    {
  ow.upmigration[i,] <- enter(ow[ow$id == tag.id.2[i],])
}


## try again 

tag <- NULL
for(TAG in unique(ow$id))
{
  tag.in <- TAG
  tag <- c(tag, tag.in)

  test <- ow[ow$]

}

#### forcing the 'enter' function into a table/df ####

enter.48407 <- enter(ow[ow$id == 48407,])
enter.48408 <- enter(ow[ow$id == 48408,])
enter.48409 <- enter(ow[ow$id == 48409,]) #<- needs to be converted to days 
enter.48410 <- enter(ow[ow$id == 48410,]) #<- needs to be converted to days 
enter.48411 <- enter(ow[ow$id == 48411,]) #<- needs to be converted to days 
#enter.48418 <- enter(ow[ow$id == 48418,]) # exclude this tag from here on out
enter.5729 <- enter(ow[ow$id == 5729,])
enter.5730 <- enter(ow[ow$id == 5730,])
enter.5732 <- enter(ow[ow$id == 5732,]) # <- needs to be converted to days 
enter.5733 <- enter(ow[ow$id == 5733,])
enter.5734 <- enter(ow[ow$id == 5734,]) # <- needs to be converted to days 
enter.5735 <- enter(ow[ow$id == 5735,]) # <- needs to be converted to days 
enter.5736 <- enter(ow[ow$id == 5736,]) # <- needs to be converted to days 
enter.5737 <- enter(ow[ow$id == 5737,])
enter.5738 <- enter(ow[ow$id == 5738,])
enter.5739 <- enter(ow[ow$id == 5739,])
enter.33158 <- enter(ow[ow$id == 33158,])
enter.33159 <- enter(ow[ow$id == 33159,])
enter.33160 <- enter(ow[ow$id == 33160,])
enter.33161 <- enter(ow[ow$id == 33161,]) # <- needs to be converted to days 
enter.33162 <- enter(ow[ow$id == 33162,]) # <- needs to be converted to days 


enter.df <- rbind( enter.48407, enter.48408, enter.48409, enter.48410, enter.48411,
                  enter.5729, enter.5730, enter.5732, enter.5733, enter.5734, enter.5735, enter.5736,
                  enter.5737, enter.5738, enter.5739, enter.33158, enter.33159, enter.33160, enter.33161, enter.33162)

enter.df <- enter.df[enter.df$id != 5735,]

## units are messed up need to convert certain tags to days
convert.tags <- c("48409", "48410", "48411", "5732", "5734", "5736", "33161", "33162")

## this function converts the selected tag ids from above (convert.tags) that require conversion from hours into days (see note in enter."tagid" above)
convert <- function(df){
  for(i in 1:length(df$id)){
    if(df$id[i] %in% convert.tags){
      df[i,12] <- df[i,12]/24 # difficult to figure out. works well. 
    } else {
      df[i,12] <- df[i,12]
    }
  }
  return(df)
}

enter.df <- convert(enter.df)


ow.2012 <- function(df){
  last.date <- function(x){tail(x, 1)}
  first.date <- function(x){head(x,1)}
  
  ascent <- last.date(df[year(df$date) == 2012,])
  
 albert <- last.date(df[year(df$date) == 2012 & df$station == "007",])
 
 ascent$albert.last <- albert$date 
  #descent <- first.date(df[which(df$date < ymd("2013-06-01") & df$date > ymd("2013-01-01")),])
  
  #combo <- rbind(ascent, descent)
  
  return(ascent)
}

#### average time spent in overwintering area for 2013-2014 ####
## the first detection in the OW wintering area to the last detection in the overwintering area. 
ow.combined <- merge(enter.df, leave.df, by = "id") 
ow.combined$length.ow <- ow.combined$date.y - ow.combined$date.x

mean(ow.combined$length.ow) # 161.1013 days 
sd(ow.combined$length.ow) # 8.9754857

## 2012 leaving from albert to head to overwintering area 
ent12.48407 <- ow.2012(ow[ow$id == 48407,])
ent12.48408 <- ow.2012(ow[ow$id == 48408,])
ent12.48409 <- ow.2012(ow[ow$id == 48409,])
ent12.48410 <- ow.2012(ow[ow$id == 48410,])
ent12.48411 <- ow.2012(ow[ow$id == 48411,])
ent12.5720 <- ow.2012(ow[ow$id == 5720,])
ent12.5721 <- ow.2012(ow[ow$id == 5721,])
ent12.5722 <- ow.2012(ow[ow$id == 5722,])
ent12.5723 <- ow.2012(ow[ow$id == 5723,])
ent12.5728 <- ow.2012(ow[ow$id == 5728,])
ent12.5725 <- ow.2012(ow[ow$id == 5725,])
ent12.5726 <- ow.2012(ow[ow$id == 5726,])
ent12.5736 <- ow.2012(ow[ow$id == 5736,]) ## ignore - this was used as a tester pre deployment 

enter.2012 <- rbind(ent12.48407, 
                    ent12.48408,
                    ent12.48409, 
                    ent12.48410, 
                    ent12.48411,
                    ent12.5720, 
                    ent12.5721, 
                    ent12.5722,
                    ent12.5723,
                    ent12.5728,
                    ent12.5725,
                    ent12.5726
                    )
###### TO DO: 
# 1) Create arrival date:
#                         - base this on day fish was last detected at stations MR_07, MR_08, MR_09
#                         - date, time, and station of arrival
#                         - seperate these fish into groups (?) later query whether or not their arrival time is based on lenght etc. 
#
#2) Create departure date: 
#                         - figure out where the fish is last detected, in the array, prior to being detected somewhere else
#                         
#3) look at detections per day per station per fish 
#
# 4) look at depth preferences 


#### percent detections within overwintering hole ####

detect <- function(x){
  total <- length(x)
  
  mr10 <- length(which(x == "010"))
  perc10 <- mr10/total
  
  mr11 <- length(which(x == "011"))
  perc11 <- mr11/total
  
  mr11.5 <- length(which(x == "011.5"))
  perc11.5 <- mr11.5/total 
  
  print(c(total,perc10,perc11, perc11.5))
}


ow$month <- month(ow$date, label = TRUE)
#ow$month <- factor(ow$month, levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May"))

ow.summary <- data.frame(summaryBy(station~id+month, data = ow[ow$date >= ymd("2013-11-14") & ow$date <= max(ow.combined$date.y) & ow$station %in% ow.stations,], FUN = list(detect)))

names(ow.summary) <- c("id", "month","total", "10","11","11.5") 


ow.summary.2 <- data.frame(summaryBy(station~month, data = ow[ow$date >= ymd("2013-11-14") & ow$date <= max(ow.combined$date.y) & ow$station %in% ow.stations,], FUN = list(detect)))
names(ow.summary.2) <- c("month","total", "10","11","11.5") 


ow.sum.melt <- melt(ow.summary, id = c("id", "month", "total"),measured = c("10", "11", "11.5"))


names(ow.sum.melt) <- c("id", "month", "total detections", "station", "percent")


full.winter.id <- c("48407", "48408","48409","48410","48411","33158","33159","33160","33161","33162","5739")

## create boxplot of ow detections per station per month
ow.sum.melt$month <- month(ow.sum.melt$month, label = TRUE, abbr = FALSE)

ggplot(ow.sum.melt, aes(station, percent)) + geom_boxplot() + facet_grid(~month)



## this is old stuff.. 
percent.plot.subset <- ow.sum.melt[ow.sum.melt$id %in% full.winter.id,]

percent.plot.subset$id <- factor(percent.plot.subset$id, levels = full.winter.id)

percent.ow.station <- ggplot(percent.plot.subset, aes(x = month, y = percent, shape = Station, colour = Station)) + geom_point(size = 4) +
  facet_grid(~id) + geom_point(colour = "grey90") + theme_bw() + theme(strip.background = element_rect(fill = 'white'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ labs(x = "Month", y = "Proportion of detections (%)") +
  theme(axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(size = 18),
        axis.text.x = element_text(angle = 90))

percent.ow.station


ggsave(percent.ow.station, file = "overwintering-detections.png", dpi = 300, path = "~/Desktop/Thesis/Presentations/Ocean Tracking Network/2014/Poster/plots", width = 18, height = 10, units = "in")

#### creating unique fish detected per station per day and month ####

dpd <- ddply(ow,.(as.Date(date, format = "%y%m%d"), station, month, year), summarize, nfish = length(unique(id)), 
            .progress = "text") 

names(dpd) <- c("date", "station", "month", "year", "nfish") # adjust names of df

## load in the station names
stations <- read.csv("~/Desktop/Data/OTN/Cape Breton/Metadata/Receivers/combined/bsb_locations.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")

stations$station <- c("001", "002", "003", "004", "005", "006", "007", "008", "009",
                      "010", "011", "011.5", "012", "013",
                      "014", "015", "016", "017", "018", "019") # excel is a pain in the ass so I need to manually put in station names


dpd <- merge(dpd, stations, by = "station") # insert lat & lon for station names
dpd <- arrange(dpd, date)# clean it up 
dpd$date <- ymd(dpd$date) 

# detections per fish 
dpf <- ddply(ow,.(as.Date(date, format = "%y%m%d"), id, station, month, year), summarize, ndetect = length(id), .progress = "text")
names(dpf) <- c("date", "id", "station", "month", "year", "ndetect")
dpf <- merge(dpf, stations, by = "station")
dpf$date <- ymd(dpf$date)
dpf <- arrange(dpf, date)

## unique fish detected per month

dpm <- ddply(ow,.(station, month, year), summarize, nfish = length(unique(id)), .progress = "text")

#dpm <- ddply(dpd, .(station, year, month), summarize, mean = round(mean(nfish),1))

dpm <- merge(dpm, stations, by = "station") ## detections per month - number of mean fish detected per month 
dpm$month <- month(dpm$month, label = TRUE, abbr = FALSE)

dps <- ddply(ow, .(station, year, month,day), summarize, ndetect = length(id), .progress = "text") ## this is detections per station per year per month per day



dps$month <- month(dps$month, label = TRUE, abbr = FALSE)
dps <- merge(dps, stations, by = "station")

dps$date <- ymd(paste(dps$year, dps$month, dps$day))

### number of tags detected per month

length(unique(ow$id[ow$month == 5 & ow$year == 2013]))

tpm <-  ddply(ow,.(month, year), summarize, nfish = length(unique(id)), 
              .progress = "text") 

#### maping from andy ####

## Download map files
# google map
#MR <- get_map (location = c(left = -60.31 bottom = 45.83, right = -59.9681, top = 46.10), source = "google", maptype = "satellite")

MR <-  get_map (location = c(left = -60.32, bottom = 45.81, right = -59.95, top = 46.09), source = "google", maptype = "terrain")
ggmap(MR, extent = "device",legend = "bottomleft")

## View map/set map background
# extent = "device" adds/removes lat lon on edge
MRmap <- ggmap (MR, extent = "device")#, size = c (400,400))
MRmap

attr(MR, "bb")
#ll.lat    ll.lon   ur.lat    ur.lon
#1 45.79678 -60.35438 46.10232 -59.91493

mr.winter <- get_map(location = c(lon = -60.10906, lat = 46.00799 ), source = "google", maptype = "terrain", zoom = 12)

attr(mr.winter, "bb")
#ll.lat    ll.lon   ur.lat    ur.lon
#1 45.93151 -60.21875 46.08412 -59.99903

mr.winter <- get_map (location = c(left = -60.25, bottom = 45.932, right = -59.90, top = 46.08), source = "google", maptype = "terrain")

mr.winter.map <- ggmap(mr.winter, extent = "device")
mr.winter.map

####rough plots dpm, dpd, dps ####
## Use map for ggplot background
all <- MRmap +
  geom_point(data = dpd, aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red")  +
  scale_size(range = c(2,16)) + facet_wrap(~month)

all 

spring <- MRmap + geom_point(data = dpd[dpd$date >= ymd("2014-04-01") & dpd$date < ymd("2014-07-01"),], 
                             aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red") +
                              scale_size(range = c(2,16)) + facet_wrap(~month)
spring


winter <- MRmap + geom_point(data = dpd[which(dpd$date >= ymd("2013-11-01") & dpd$date < ymd("2014-05-01")),], 
                             aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red") +
                              scale_size(range = c(2,16)) + facet_wrap(~month)
winter
  

november <- MRmap + geom_point(data = dpd[which(dpd$date >= ymd("2013-11-01") & dpd$date < ymd("2013-12-01")),], 
                             aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red") +
  scale_size(range = c(2,16)) + facet_wrap(~date)
november

december  <- MRmap + geom_point(data = dpd[which(dpd$date >= ymd("2013-12-01") & dpd$date < ymd("2014-01-01")),], 
                                         aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red") +
  scale_size(range = c(2,16)) + facet_wrap(~date)
december

#### plot with nfish.month ####

test <- MRmap +
  geom_point(data = dpm, aes(x = lon, y = lat, size = mean), alpha = 0.6, colour = "red")  +
  scale_size(range = c(2,16)) + facet_wrap(~month)
test


dpm.2012 <- MRmap +
  geom_point(data = dpm[dpm$year == 2012,], aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red")  +
  scale_size(range = c(2,16)) + facet_wrap(~month)

dpm.2013 <- MRmap +
  geom_point(data = dpm[dpm$year == 2013,], aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red")  +
  scale_size(range = c(2,16)) + facet_wrap(~month)

dpm.2014 <- MRmap +
  geom_point(data = dpm[dpm$year == 2014,], aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red")  +
  scale_size(range = c(2,16)) + facet_wrap(~month)
   

# I keep getting remove rows... may have to do with the coordinate system? 
      # - problem resolved on 2014-08-13 --> MR_01 was outside the boundary of the map -- eroneously entered lat long coordinate for somewhere over in Nyanza 

attr(MR, "bb")
#ll.lat    ll.lon   ur.lat    ur.lon
#1 45.79678 -60.35438 46.10232 -59.91493


#### Plotting Functions ####

dpd.date <- function(start, end){
  MRmap +
    geom_point(data = dpd[dpd$date >= ymd(start)  & dpd$date <= ymd(end) ,], aes(x = lon, y = lat, size = nfish), alpha = 0.6, colour = "red")  +
    scale_size(range = c(2,16)) + facet_wrap(~date)
}

                      # plot dpf - detections per fish per day #
dpf.date <- function(fish, start, end){
  MRmap +
    geom_point(data = dpf[dpf$date >= ymd(start)  & dpf$date <= ymd(end) & dpf$id == fish,], aes(x = lon, y = lat, size = ndetect), alpha = 0.6, colour = "red")  +
    scale_size(range = c(2,16)) + facet_wrap(~date)
}


dps.date <- function(start, end){
  MRmap +
    geom_point(data = dps[dps$date >= ymd(start)  & dps$date <= ymd(end) ,], aes(x = lon, y = lat, size = ndetect), alpha = 0.6, colour = "red")  +
    scale_size(range = c(2,16)) + facet_wrap(~date)
}

#### plots for AFS ####

dpm.2013.mj <- ggmap(MR, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2013 & dpm$month %in% c("May", "June"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + theme(legend.title = element_text(size = 15),
                                                                                strip.background = element_rect(fill = "white"), 
                                                                                strip.text.x = element_text(size = rel(3))) +
  geom_text(aes(x = lon, y = lat, label = station), data = ws.station.afs, size = 10) ### lets incorporate some ws stations to make life easier in ppt
  
dpm.2013.ja <- ggmap(MR, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2013 & dpm$month %in% c("July", "August"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + theme(legend.title = element_text(size = 15),
                                                                                strip.background = element_rect(fill = "white"), 
                                                                                strip.text.x = element_text(size = rel(3))) +
  geom_text(aes(x = lon, y = lat, label = station), data = ws.station.afs, size = 10)


dpm.2013.so <- ggmap(MR, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2013 & dpm$month %in% c("September", "October"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) +
  geom_text(aes(x = lon, y = lat, label = station), data = ws.station.afs, size = 10, hjust = 0, vjust = 1)

dpm.2013.nd <- ggmap(MR, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2013 & dpm$month %in% c("November", "December"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) +
  geom_text(aes(x = lon, y = lat, label = station), data = ws.station.afs, size = 10)#, hjust = 0, vjust = 1)

dpm.2013.n <- ggmap(mr.winter, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2013 & dpm$month %in% c("November"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) 
  
dpm.2013.d <- ggmap(mr.winter, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2013 & dpm$month %in% c("December"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) 


dpm.2014.jfm.big <- ggmap(MR, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2014 & dpm$month %in% c("January", "February", "March"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+
 # geom_text(aes(x = lon, y = lat, label = station), data = ws.station.afs, size = 10)#, hjust = 0, vjust = 1)

dpm.2014.jfm.small <- ggmap(mr.winter, extent = "device",legend = "bottomright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2014 & dpm$month %in% c("January", "February", "March"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+

dpm.2014.jan.small <- ggmap(mr.winter, extent = "device",legend = "bottomright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2014 & dpm$month %in% c("January"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+

dpm.2014.feb.small <- ggmap(mr.winter, extent = "device",legend = "bottomright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2014 & dpm$month %in% c("February"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+

dpm.2014.mar.small <- ggmap(mr.winter, extent = "device",legend = "bottomright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2014 & dpm$month %in% c("March"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+


dpm.2014.am <- ggmap(MR, extent = "device",legend = "topright", padding = 0.01) +
  geom_point(data = dpm[dpm$year == 2014 & dpm$month %in% c("April", "May"),], aes(x = lon, y = lat, size = nfish), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+

dpm.2014.jf


dpm.2014.am

dps.summer <-  ggmap(mr.winter, extent = "device",legend = "bottomright", padding = 0.01) +
  geom_point(data = dps[dps$year == 2013 & dps$month %in% c("May", "June", "July", "August", "September", "October", "November"),], aes(x = lon, y = lat, size = ndetect), alpha = 0.55, colour = "red")  +
  scale_size(range = c(3,19), name = "# of fish") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) #+

dps.summer <- ggmap(MR, extent = "device",legend = "bottomright") +
  geom_point(data = dps[dps$year == 2013 & dps$month %in% c("May", "June", "July", "August", "September", "October", "November"),], aes(x = lon, y = lat, size = ndetect), alpha = 0.55, colour = "red")  +
  scale_size(range = c(2,20), name = "# of detections") +  facet_wrap(~month) + 
  theme(legend.title = element_text(size = 15), strip.background = element_rect(fill = "white"),  strip.text.x = element_text(size = rel(3))) 
  


wd.ow <- getwd()
# mj = may june, ja = july august, etc.
setwd("~/Desktop/Thesis/Presentations/AFS/2014/plots/detections per month")
ggsave(dpm.2013.mj, file = "2013.mj.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2013.ja, file = "2013.ja.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2013.so, file = "2013.so.pdf", width = 24, height = 13, units = "in", dpi = 300 ) 
ggsave(dpm.2013.nd, file = "2013.nd.pdf", width = 24, height = 13, units = "in", dpi = 300 )

ggsave(dpm.2013.n, file = "2013.nov.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2013.d, file = "2013.dec.pdf", width = 24, height = 13, units = "in", dpi = 300 )
wggsave(dpm.2014.jfm.big, file = "2014.jfm.big.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2014.jfm.small, file = "2014.jfm.small.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2014.jan.small, file = "2014.jan.small.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2014.feb.small, file = "2014.feb.small.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2014.mar.small, file = "2014.mar.small.pdf", width = 24, height = 13, units = "in", dpi = 300 )
ggsave(dpm.2014.am, file = "2014.am.pdf", width = 24, height = 13, units = "in", dpi = 300 )

ggsave(dpm.2013.n, file = "2013.n.pdf", width = 12, height = 12, units = "in", dpi = 300 )

#### plots with code from Jill Bennet #####

p <- ggplot(ow[ow$date >= ymd("2014-04-01") & ow$date <= ymd("2014-07-01"),], aes(date, id, group = id, shape = station))
p + geom_point()+ geom_line()

p <- ggplot(ow[ow$date >= ymd("2014-04-01") & ow$date <= ymd("2014-07-01"),], aes(date, station, group=station, col=station))
p + geom_point() + geom_line() + facet_grid(id~.)

p <- ggplot(ow[ow$date >= ymd("2014-04-01") & ow$date <= ymd("2014-07-01"),], aes(date, station))
p + geom_point() + geom_line() + facet_grid(id~.)

p <- ggplot(ow[ow$date >= ymd("2014-04-01") & ow$date <= ymd("2014-07-01"),], aes(date, id, group=id, shape=station, col = station))
p + geom_point() + facet_grid(id~.)

#### renaming x axis for Laura ####


#### NEED TO GET SOME FIGURES AND PRODUCE THIS SHIT UP ####
# X axis = detection date
  # continuous
# Y axis = station name 
  # points are detection events


#### code does not work below here ####



# need to isolate only the tag numbers I want

# paste

#### working with depth data ####
depth.df <- na.omit(ow)         # <- all depths for 
depth.df <- na.omit(depth.df)

mean(depth.df$depth) # 6.926
sd(depth.df$depth) # 1.30 # 1.116
length(depth.df$depth)# 114905 ## 115038 2014-09-23

depth.feb <- depth.df[month(depth.df$date) == "2",]

depth.feb.sum <- ddply(depth.feb, c("station"), summarise,
                   N    = length(depth),
                   mean = mean(depth),
                   sd   = sd(depth),
                   se   = sd / sqrt(N) )
depth.feb.sum

depth.sum <- ddply(depth.df[depth.df$date>= ymd("2013-12-01") & depth.df$date < ymd("2014-05-01"),], c("station", "month"), summarise,
               N    = length(depth),
               mean = mean(depth),
               sd   = sd(depth),
               se   = sd / sqrt(N) )
depth.sum

dp <-ggplot(depth.df[depth.df$date >= ymd("2013-12-01") & depth.df$date < "2014-05-01",], aes(date, depth))
dp + geom_point() + geom_line() + facet_grid(id~.)

dp.5739 <- ggplot(depth.df[depth.df$date >= ymd("2014-02-20") & depth.df$date < ymd("2014-02-21") & depth.df$id == "5739",], aes(date, depth))
dp.5739 + geom_point() + geom_line() + facet_grid(.~station)






# don't understand how this works... 

DT <- data.table(ow[ow$date <= ymd("2013-12-31") & ow$station %in% outside.array & ow$id != 48418,])

setkey(DT, date, id)
last.ow.detect <- DT[DT[,.I[.N], by=id][, V1]]
last.ow.detect ## these are the last overwintering detections prior to heading into the overwintering hole

mean(last.ow.detect$date) # 2013-11-17 @ 18:17 ---> Without 48418 |||| 2013-11-19 20:41:21 
sd(last.ow.detect$date) # 523766.4 / (60*60*24) = 6.06 days ---> without 48418 ||||   958318.(60*60*24) = 11.0916 days



### Need to to summary of detections per station as percentage of total detections 


ddply(ow[ow$date >= ymd("2013-11-13"),], .(id), FUN = detect)

tl.2012 <- c(61.4,69.5,73.4,90.6,72.4,125.0,74.4,66.4,63.6,84.4,85.6,76.9,77.6,84.4) # acoustically tagged
mean(tl.2012) # 79.0 cm
min(tl.2012) # 61.4
max(tl.2012) # 125.0 

tl.2013 <- c(69.3,75.3,63.2,63.7,52.8,49.7,90.0,79.9,50.0,66.3,69.1,82.5,51.0,49.9,57.3)# acoustically tagged
mean(tl.2013) # 64.7
min(tl.2013) # 49.7
max(tl.2013) # 90.0 
