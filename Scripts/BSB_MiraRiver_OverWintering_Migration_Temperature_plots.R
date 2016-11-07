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
#4) figure out how to fill in the temperature for each fish at the time of it's departure

require(ggplot2)
require(scales)
require(dplyr)
require(tidyr)
#require(grid) #?
#require(gridExtra)#?

"%nin%" <- Negate("%in%")
####

# temp df 
tdf <- read.csv("data/MR07t_2012-2015_UTC_temperature.csv", colClasses = "character")
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
tdf <- mutate(tdf, date = date + hours(3)) ### ATTENTION: this needs to happen with the combined hobo file from above since UTC correction wasn't applied
edf <- dates_and_times_fun(edf)
#edf$ddate <- ymd(substr(edf$date, 1, 10))

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
stndf <- dates_and_times_fun(stndf)

source("functions/hobo_cleanup_fun.R") # to see how this works, see "hobo_cleanup_fun.R"/ page 102 of Lab book
df <- hobo_cleanup_fun(tdf, stndf, "007")
df$temp <- as.numeric(df$temp)
### now there are also some problems where a logger was removed without being redeployed with the station (for rebatterying)
df <- df[!df$date %within% interval("2013-04-24 23:00:00", "2013-05-03 21:00:00") & !df$date %within% interval("2014-05-03 16:30:00", "2014-05-03 17:00:00"),]
#df <- df %>% mutate(year = year(date), month = month(date), ddate = ymd(substr(df$date, 1, 10)))
#df$winter <- ifelse(df$month > 6, paste(df$year, df$year+1, sep="-"), paste(df$year-1,df$year, sep="-"))

#### feel the construction love ####
# idea: join the events df with df and then use the closest temp value to insert into the dataframe (lead for next possible temp event)

adf <- union_all(df, edf)
adf$temp <- as.numeric(adf$temp)
adf <- adf %>% arrange(date)
adf <- adf %>% fill(temp)

# add a winter/year/months/ddate column to data
adf <- adf %>% mutate(year = year(date), month = month(date), ddate = ymd(substr(adf$date, 1, 10)))
adf$winter <- ifelse(adf$month > 6, paste(adf$year, adf$year+1, sep="-"), paste(adf$year-1,adf$year, sep="-"))
adf <- adf %>% group_by(ddate) %>% mutate(mtemp = mean(temp))
adf$event <- factor(adf$event, levels = c("Last Albert Bridge", "Last Outside Overwintering", "First Overwintering","End Overwintering", "hobo")) 


#### PLOT: Mean Daily Temperature (°C) vs Date of EVENTs (last albert, first ow, first outside) ####
daily12 <- filter(adf, winter == "2012-2013")
daily13 <- filter(adf, winter == "2013-2014")
daily14 <- filter(adf, winter == "2014-2015")

p1 <- ggplot(data = filter(daily12, month %in% c(10:12,1:5)), aes(ddate, mtemp)) + geom_line(data = filter(daily12, month %in% c(10:12,1:5))) + geom_line(data = filter(daily12, month %in% c(4,5))) +
  geom_point(data = filter(daily12, event %nin% c("hobo", "Last Outside Overwintering")),aes(shape = event, size = 5), position = "jitter") + scale_shape_manual(values=c(20,3)) + theme_bw() + xlab("")+ ylab("") +
  theme(legend.position = "none", axis.text.x = element_blank()) + scale_x_date(breaks = pretty_breaks(8),limits = c(ymd("2012-10-01"), ymd("2013-05-31"))) + guides(size = F) 

p2 <- ggplot(data = filter(daily13, month %in% c(10:12,4,5)), aes(ddate, mtemp)) + geom_line(data = filter(daily13, month %in% c(10:12,1:5))) + geom_line(data = filter(daily13, month %in% c(4,5))) +
  geom_point(data = filter(daily13, event %nin% c("hobo", "Last Outside Overwintering")),aes(shape = event, size = 5), position = "jitter") + scale_shape_manual(values=c(20, 8, 3)) + theme_bw() + ylab("Mean Daily Temperature (°C)") + xlab("") +
  theme(legend.position = c(0.5, 0.7), legend.direction = "horizontal", legend.title = element_blank() , axis.text.x = element_blank()) + scale_x_date(breaks = pretty_breaks(8),limits = c(ymd("2013-10-01"), ymd("2014-05-31"))) + guides(size = F)

p3 <- ggplot(data = filter(daily14, month %in% c(10:12,4,5)), aes(ddate, mtemp)) + geom_line(data = filter(daily14, month %in% c(10:12,1:5))) + geom_line(data = filter(daily14, month %in% c(4,5))) +
  geom_point(data = filter(daily14, event %nin% c("hobo", "Last Outside Overwintering")),aes(shape = event, size = 5), position = "jitter") + scale_shape_manual(values=c(20, 8, 3)) + theme_bw() + xlab("Date") + ylab("") +
  theme(legend.position = "none", legend.title = element_blank()) + scale_x_date(breaks = pretty_breaks(8),labels = date_format("%B"), limits = c(ymd("2014-10-01"), ymd("2015-05-31"))) + guides(size = F)

source("functions/multiplot_fun.R")
multiplot_fun(p1, p2, p3)







 