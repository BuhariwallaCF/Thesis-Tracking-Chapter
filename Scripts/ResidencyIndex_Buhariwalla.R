# Acoustic Telemetry Residency Index
# Colin Buhariwalla 
# Started: 2015-04-24 
# Most Recent update: 2016-05-24 by CFB 

#### Load Packages ####
#install these packages if you don't already have them 
#instal.packages("package_name")
#detach(package:package_name)

require(lubridate)
intersect.lub <- intersect
require(plyr)
require(dplyr) ## this masks intersect thus the above Intersect
require(ggplot2)

#### definitions ####
# apdd <- all possible detection days
# mr.df <- detections dataframe (df) 
# det.days = number of days detected
# det.pday = number of detections per day per station
# stn.df <- deployment & recovery md df 
# tags.df <- tag metadata(md) df 

##### read in metadata (tag deployment, receiver deployment, and receiver recovery) and detection .CSVs #####




## need to have a day column in the DET and TAGS



#to read in the csvs with MB VUE data (for each year, 2012-2015)

mr.df <- read.csv("data/mrdf.csv", stringsAsFactors = FALSE)
tag.df <- read.csv("data/tagdf.csv", stringsAsFactors = FALSE)
stn.df <- read.csv("data/stndf.csv", stringsAsFactors = F)
zone.df <- read.csv("data/zonedf.csv", stringsAsFactors = F)



source("functions/dates_and_times_fun.R")
mr.df  <- dates_and_times_fun(mr.df)
tag.df <- dates_and_times_fun(tag.df)
stn.df <- dates_and_times_fun(stn.df)
zone.df <- dates_and_times_fun(zone.df)

# format data and metadata - parse dates, set factors, create intervals 

stn.df <- stn.df[!stn.df$station %in% c(350, 200, 100, 13.5),]



####Create summary of Detections, per day, per station, per fish####
# use dplyr

det.pday <- mr.df %>%
  group_by(ddate, id, station) %>% # maybe just ddate
  select(id, station) %>% ## think about using station or receiver sn. 
  summarise(
    detpday = length(id)
    )


det.pday <- arrange(det.pday, ddate, id)

####Create Ir number of days detected within ENTIRE array -- Playing Around ####
# Total number of days detected
#max, min, mean, sd # days detected  ## doesn;t mean anything since we have unequal detection probabibilities. aka lengths of time gear is deployed per area 
ir.system <- mr.df %>%
  group_by(id, year) %>%
  select(id, ddate) %>%
  summarise(
    daysdetected = n_distinct(ddate),
    all.possible.det.days = (max(ddate) - min(ddate) + 1),
    ir = daysdetected/all.possible.det.days
) #### ****** This is rough residency index. Does not mean much. Need to adjust for presence/absence of array. 

ir.station <- mr.df %>%
  group_by(id, station) %>%
  select(id, ddate) %>%
  summarise(
    days.detected = n_distinct(ddate)
    )

start.ow <- ymd("2013-12-01")
end.ow <- ymd("2014-05-01")

ir.ow.2014 <- filter(mr.df, ddate >= start.ow & ddate <= end.ow) %>%
  group_by(id, station) %>%
  select(id, ddate) %>%
  summarise(
    daysdetected = n_distinct(ddate),
    all.possible.det.days = (max(ddate) - min(ddate) + 1),
    ir = daysdetected/all.possible.det.days
)


#### Creating a function to calculate the IR for each fish based on fish id, detection data, and station data 

#The InteFunc incorporates ID (fish.id), tag deployment history (fish.data), and station deployment/recovery metadata (station.data)
# first line of code creates the number of days the Fish (ID) activity/deployment overlaps with the station deployment RESULTING in possible detection days
# The FOR LOOP : first line creates a space to build up the data that's extracted during each loop iteration
# The loop is calculating the number of possible days a fish can be detected for each staiton 
# SEE annotations next to each line of code to understand operation

#### IR function to calculate all possible days depending on tag and receiver deployment history ####
# Code to Produce All possible detection days depending on deployment history 
## create a function using master... that I can plug shit, start, end dates, etc. 
InteFunc=function(fish.ID, fish.data, station.data, start.ymd_hms, end.ymd_hms){ # second needs to be 1
  
  start <- ymd_hms(start.ymd_hms) # need to ensure start & end dates are in posix?  
  end <- ymd_hms(end.ymd_hms)
  
  for(z in 1:length(unique(fish.data$id))){ 
    ifelse(fish.data$depl_date[z] < start & fish.data$end_date[z] > start, fish.data$depl_date2[z] <- as.character(start), fish.data$depl_date2[z] <- as.character(fish.data$depl_date[z])) ## be careful to see what happens when a start date is later than the end date... 
   
    ifelse(fish.data$end_date[z] > end, fish.data$end_date2[z] <- as.character(end), fish.data$end_date2[z] <- as.character(fish.data$end_date[z]))
    
    if(fish.data$end_date[z] < start){fish.data$depl_date2[z] <- as.character(end)} ## do this to make any tags that can't be available in our deployment window have a negative day value
       
    if(fish.data$depl_date[z] > end) {fish.data$depl_date2[z] <- as.character(end)} ##  deployment date is greater than end aka tag unavailable for study window.
    if(fish.data$depl_date[z] > end) {fish.data$end_date2[z] <- as.character(start)}
  
  # if deployment date is less than start date & drop dead date is greater than start date, deployment date 2 is equal to start date, otherwise it remains the same 
  # if drop dead (end) date is greater than end, drop dead date 2 is equal to end, else, drop dead date 2 is equal to drop dead date
  # if drop dead date is before the start date, deployment date 2 is equal to end date <--- *** this negates the number of days should the tag not be present within the study window. 
  }
  
  fish.data$depl_date2 <- ymd_hms(fish.data$depl_date2)
  fish.data$end_date2 <- ymd_hms(fish.data$end_date2)
  
  fish.data$tag_int2 <- new_interval(fish.data$depl_date2, fish.data$end_date2)
  
  
  study.window <- new_interval(start, end)
  
 station.data$pos.station.day <- day(as.period(new_interval(start, end),"days")) + 1 ### need this to have proper number of days
 station.data$act.station.day <- day(as.period(intersect.lub(study.window,station.data$station_int), "days")) + 1 # to get over the 1 second hump from 29 to 30 or 30 to 31 days
  
 station.data$days.possible <- day(as.period(intersect.lub(fish.data$tag_int2[fish.data$id == fish.ID],
                                                         station.data$station_int), "days")) + 1
  
  hold=NULL # create a space to populate data extracted with the for loop
  for(i in 1:length(unique(station.data$station))){ 
    temp = subset(station.data, station==unique(station.data$station)[i]) # create 'temp' to store data associated with [i]th station 
    dp = sum(temp$days.possible, na.rm=T) # if there are multiple deployment histories, add up total number of possible days
    pos.station.day = temp$pos.station.day ## 2015-07-03 14:05 ADT sum(temp$pos.station.day, na.rm=T) ### need to change to see if we can get the possible number of days to line up with what we want 
    act.station.day = sum(temp$act.station.day, na.rm=T)
    temp2 = data.frame(station=temp$station[1], days.possible = dp, station.possible = pos.station.day, station.actual = act.station.day  ) #create temp df to hold data 
    hold = rbind(hold, temp2) # hold = place marker, temp2 is the temp df used to isolate each iteration of the data you're looking for
  }
  hold$id=fish.ID # associate a fish ID with the calculated data 
  return(hold)
} 

#**************************************************************************************************************************************************************************************************
#### IR FROM ABOVE BUT BASED ON ZONE!!!!! ####
# Code to Produce All possible detection days depending on ZONE history 
InteFunc.zone=function(fish.ID, fish.data, zone.data, start.ymd_hms, end.ymd_hms){ # second needs to be 1
  
  start <- ymd_hms(start.ymd_hms) # need to ensure start & end dates are in posix?  
  end <- ymd_hms(end.ymd_hms)
  
  for(z in 1:length(unique(fish.data$id))){ 
    ifelse(fish.data$depl_date[z] < start & fish.data$end_date[z] > start, fish.data$depl_date2[z] <- as.character(start), fish.data$depl_date2[z] <- as.character(fish.data$depl_date[z])) ## be careful to see what happens when a start date is later than the end date... 
    
    ifelse(fish.data$end_date[z] > end, fish.data$end_date2[z] <- as.character(end), fish.data$end_date2[z] <- as.character(fish.data$end_date[z]))
    
    if(fish.data$end_date[z] < start){fish.data$depl_date2[z] <- as.character(end)} ## do this to make any tags that can't be available in our deployment window have a negative day value
    
    if(fish.data$depl_date[z] > end) {fish.data$depl_date2[z] <- as.character(end)} ##  deployment date is greater than end aka tag unavailable for study window.
    if(fish.data$depl_date[z] > end) {fish.data$end_date2[z] <- as.character(start)}
    
    # if deployment date is less than start date & drop dead date is greater than start date, deployment date 2 is equal to start date, otherwise it remains the same 
    # if drop dead (end) date is greater than end, drop dead date 2 is equal to end, else, drop dead date 2 is equal to drop dead date
    # if drop dead date is before the start date, deployment date 2 is equal to end date <--- *** this negates the number of days should the tag not be present within the study window. 
  }
  
  fish.data$depl_date2 <- ymd_hms(fish.data$depl_date2)
  fish.data$end_date2 <- ymd_hms(fish.data$end_date2)
  
  fish.data$tag_int2 <- new_interval(fish.data$depl_date2, fish.data$end_date2)
  
  
  study.window <- new_interval(start, end)
  
  zone.data$pos.zone.day <- day(as.period(new_interval(start, end),"days")) + 1 ### need this to have proper number of days
  zone.data$act.zone.day <- day(as.period(intersect.lub(study.window,zone.data$zone_int), "days")) + 1 # to get over the 1 second hump from 29 to 30 or 30 to 31 days
  
  zone.data$days.possible <- day(as.period(intersect.lub(fish.data$tag_int2[fish.data$id == fish.ID],
                                                            zone.data$zone_int), "days")) + 1
  
  hold=NULL # create a space to populate data extracted with the for loop
  for(i in 1:length(unique(zone.data$zone))){ 
    temp = subset(zone.data, zone==unique(zone.data$zone)[i]) # create 'temp' to store data associated with [i]th zone 
    dp = sum(temp$days.possible, na.rm=T) # if there are multiple deployment histories, add up total number of possible days
    pos.zone.day = temp$pos.zone.day ## 2015-07-03 14:05 ADT sum(temp$pos.zone.day, na.rm=T) ### need to change to see if we can get the possible number of days to line up with what we want 
    act.zone.day = sum(temp$act.zone.day, na.rm=T)
    temp2 = data.frame(zone=temp$zone[1], days.possible = dp, zone.possible = pos.zone.day, zone.actual = act.zone.day  ) #create temp df to hold data 
    hold = rbind(hold, temp2) # hold = place marker, temp2 is the temp df used to isolate each iteration of the data you're looking for
  }
  hold$id=fish.ID # associate a fish ID with the calculated data 
  return(hold)
} 





#### ENTER specific time period of interest ####
## time division of interest (ie, monthly)? may need to change this if we want weekly ? depending on the scale of the question

#July 2012 to June 2015 == 35 months, 5 days
start <- as.character(ymd_hms("2012-07-01 00:00:01") + months(0:36))
end   <- as.character(ymd_hms("2012-08-01 23:59:59") + months(0:36) - days(1)) # this little beauty gives you the last day of every month for 0:x months. 
study.month <- data.frame(start, end)

# figure out houw to make study.week -- may be interesting to see during times of roaming
 
#### LOOP to determine all possible detection days given deployment histories ####
master=NULL
system.time(for (i in 1:length(unique(tag.df$id))){ ### approx 4 min to do
  for(j in 1:length(study.month$start)){
  output=InteFunc(unique(tag.df$id)[i],tag.df, stn.df, study.month$start[j], study.month$end[j] )#start.date, end.date) ## this is saying all you need is the unique tag ID's to operate the InteFunc
  output$year <- year(study.month$start[j])
  output$month <- month(study.month$start[j])
  master=rbind(master, output)
}})

#### SAME AS LOOP ABOVE BUT WITH ZONE --- 3.2 minutes ####
master.zone=NULL
system.time(for (i in 1:length(unique(tag.df$id))){ ### approx 4 min to do
  for(j in 1:length(study.month$start)){
    output.zone=InteFunc.zone(unique(tag.df$id)[i],tag.df, zone.df, study.month$start[j], study.month$end[j] )#start.date, end.date) ## this is saying all you need is the unique tag ID's to operate the InteFunc
    output.zone$year <- year(study.month$start[j])
    output.zone$month <- month(study.month$start[j])
    master.zone=rbind(master.zone, output.zone)
  }})
#### Summarise detections based on time period (month) & station/zone ####
ir.station <-  mr.df %>%
  group_by(id, year, month, station) %>%
  select(id, ddate) %>%
  summarise(
    days.detected = n_distinct(ddate)
  )

ir.zone <- mr.df %>% group_by(id, year, month, zone) %>% select(id, ddate) %>% summarise(days.detected = n_distinct(ddate))

#### Detection summary and possible days to produce the elusive IR!!!!!!!!!!!!!!!!!!!!!!!!! ####
ir.master <- merge(ir.station, master, by = c("id", "station", "year", "month")) # the merging will drop tag id's not present in the mr.df during study period of interest... common merge. 

ir.master$ir <- ir.master$days.detected/(ir.master$days.possible + 1) # number of total days possible for detections. 
ir.master$ir.adjusted <- ir.master$ir * (ir.master$station.actual/ir.master$station.possible) # trying to compensate for unequal deployment times # there will be a positive bias for stations deployed longer.. this brings that down. 

ir.master$station <- as.factor(ir.master$station)

ir.master$yrmo <- as.Date(paste(ir.master$year, ir.master$month, "01", sep = "-"))


ir.calc <- ir.master %>% group_by(yrmo, station) %>% transmute(med.ir = median(ir.adjusted)) 


####  Detection summary and possible days for ZONE Ir ####

ir.zone.mas <- merge(ir.zone, master.zone, by = c("id", "zone", "year","month"))
ir.zone.mas$ir <- ir.zone.mas$days.detected/(ir.zone.mas$days.possible + 1)
ir.zone.mas$yrmo <- as.Date(paste(ir.zone.mas$year, ir.zone.mas$month, "01", sep = "-"))









View(ir.zone.mas[ir.zone.mas$month == "12",])





####  Ignore below. 
#### WORK ON THIS  start plotting this shit! #### 


ggplot(ir.master[ir.master$station %in% c(7,9,10,11,12,13,14,15),], aes(as.factor(month), ir.adjusted)) + geom_boxplot() + facet_grid(~station)
ggplot(ir.master, aes(as.factor(month), ir.adjusted)) + geom_boxplot() + facet_grid(~station)

ddply(ir.master, .(station, month), summarise, mean(ir.adjusted))

ggplot(mr.df[mr.df$date >= ymd("2014-11-01")&mr.df$date <= ymd("2015-05-25"),], aes(month, depth))+ geom_point() +facet_grid(~station)



#### circular plots of IR master ####

p1 <- ggplot(ir.master, aes(x = ir.adjusted)) + geom_histogram() + coord_polar(theta = "x", start = 1, direction = 1) + facet_wrap(~station)
p1 


### construction zone ####
# make a duration of station activity for all zones
stn.df$zone <- ifelse(stn.df$station <= 4, 1,
                      ifelse(stn.df$station >= 5 & stn.df$station <= 8, 2,
                             ifelse(stn.df$station >= 9 & stn.df$station <= 15, 3,
                                    ifelse(stn.df$station >= 16, 4, "ERROR")))) 

remove.stn <- c(100.0, 200.0,350.0)
stn.df <- stn.df[!stn.df$station %in% remove.stn,]

