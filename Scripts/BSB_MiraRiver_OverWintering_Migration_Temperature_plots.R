#2016-10-19 Colin F Buhariwalla
# OVERWINTERING - plot departure/arrival times with temperature across years 
# last updated:

# This script is meant to plot the overwintering departure/return events from Albert bridge into the OW area + Leaving overwintering area
# I need: 1) Temperature at BSB MR07 from Hobo Temp Logger (include relevant metadata in the script for easy reference)
#         2) a file with albert bridge departure events, arrival in OW events, and first out events 
#         3) Need to combine this event file with temperature file (take the temperature at the closest time stamp and give it to the departure event)

# Want to Produce: a plot with date on the X axis, temperature on the y axis, have events (leaving ab, arrival in ow, exit of ow) as different symbols
#                  along the temperature line, by year? 

# I need to decide on: 
# 1) What temperature to use (mean daily?, min, max)

#Steps:
# 1) Use my OWmigration file to produce the required information
# 2) Export this into a saved data file 
# 3) read in my water temperature file - ensure that it is cleaned up.
# 4) figure out how to fill in the temperature for each fish at the time of it's departure

require(dplyr)
####

# temp df 
tdf <- read.csv("raw data/MR_07_2012-2015_temp.csv", colClasses = "character")
names(tdf) <- c("date", "temp")
tdf$event <- "hobo"
tdf$station <- "007"

#events df
edf <- read.csv("data/overwintering_events.csv", stringsAsFactors = F)

#stn df to eliminate associated hobo loggings of non deployed station
stn.df <- read.csv("data/stndf.csv", stringsAsFactors = F)

#get some posix on the go
source("functions/dates_and_times_fun.R")
tdf <- dates_and_times_fun(tdf)
edf <- dates_and_times_fun(edf)
#tdf <- dates_and_times_fun(tdf)

# recovery/deploy metadata 
rdf <- read.csv("raw data/bsb_metadata_recovery_2012-2015_update20160829.csv", colClasses = "character")
ddf <- read.csv("raw data/bsb_metadata_deployment_2012-2015_update20160829.csv", colClasses = "character") # careful, this file had quotations (" ") in it and R had trouble reading it in

rdf <- select(rdf, STATION_NO, CONSECUTIVE_DEPLOY_NO, INS_SERIAL_NO, RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss.)
ddf <- select(ddf, STATION_NO, CONSECUTIVE_DEPLOY_NO, INS_SERIAL_NO, DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss.,DEPLOY_LAT, DEPLOY_LONG, BOTTOM_DEPTH, INSTRUMENT_DEPTH)

names(rdf) <- c("station", "deploy_no", "serial_no","recov_date")
names(ddf) <- c("station", "deploy_no", "serial_no", "depl_date", "lat", "long", "depth", "ins_depth")

#rdf$stn <- as.numeric(rdf$station) # the formats aren't the same during the imports.. 
#ddf$stn <- as.numeric(ddf$station)

stndf <- full_join(ddf, rdf, by = c("station", "deploy_no", "serial_no"))
stndf <- stndf[1:106,] # the blank rows that excel always has in it get imported, need to delete. 

source("functions/dates_and_times_fun.R")
stndf <- dates_and_times_fun(stndf)


########## heavy hitting construction zone ######


hobo_cleanup_fun <- function(temp_data, stn_metadata, station_of_interest){ # we input: hobo temp file cleaned up above; station metadata; and the station number we want to work with (character) e.g. "007" 
  {
stn <- stn_metadata %>% filter(station == station_of_interest) # isolate only the metadata we want
stn_recovered <- stn %>% transmute(recovered_int = interval(lag(recov_date,1),depl_date))# create an interval of the time hobo was out of the water between deployments; basically, recovery date 1 - deploy date 2; NOTE: row 1 WILL == NA!!!!
temperature.df <- temp_data %>% filter(date < stn$recov_date[length(stn$recov_date)] & date > stn$depl_date[1])# Remove data points before (depl_date 1) and after (last recov_date) study window -  

### now let's skip some rope and jump into a loop... here we gooooooo 
deletion.bin.df <- NULL # space holder
for(i in 2:length(stn_recovered$recovered_int)){ # need to start at 2 due to the NA at stn_recovered$recovered_int[1] 
  temp <- temperature.df[temperature.df$date %within% stn_recovered$recovered_int[i],] # are there dates that fall within the recovered intervals? 
  deletion.bin.df <- rbind(deletion.bin.df, temp) # bind that shit together
}
  }
  clean_temp <- anti_join(temperature.df, deletion.bin.df, by = "date") # get rid of the terrible, awful, just monsterous detections from the orginal data file and call it something new and shiny/pretty... I am tired. 
 # remove(deletion.bin.df)
  return(clean_temp)
}




trash <- hobo_cleanup_fun(tdf, stndf, "007") #YESSSSS IT WORKKKSSSS !!!!!!!!!!! I CAN FINALLY SLEEEEPPPP!!!!

