# 2016-11-07 Colin F. Buhariwalla
# Circular Statistics 
# 1) Leaving/Return Events
# 2) Migration to and from OVERWINTERING

# Methodology from Zarr 1999. 

#### Leaving/Return Events ####
dir("data/")
lrdf <- read.csv("data/BSB_MiraRiver_LeavingEvents.csv", stringsAsFactors = F)
ldf <- select(lrdf, id, exit)
names(ldf) <- c("id", "date")
rdf <- select(lrdf, id, enter)
names(rdf) <- c("id", "date")
# need to create time periods of interest... 
# Day of the year, Month of the year, Week of the year, Time of the day, and then compute the angle 
# a = ((360)*X)/k where x is the unit of interest and k is the number of units of x in one complete cycle ( the period )

# create day of year column * keep in mind leap years will be different

source("functions/dates_and_times_fun.R")
ldf <- dates_and_times_fun(ldf)
rdf <- dates_and_times_fun(rdf)
