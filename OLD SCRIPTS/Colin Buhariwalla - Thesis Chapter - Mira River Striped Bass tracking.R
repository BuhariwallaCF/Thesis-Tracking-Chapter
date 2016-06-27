# Colin Buhariwalla - Thesis Chapter - Mira River Striped Bass tracking
# 2012-2015 
Intersect <- intersect

require(plyr)
require(dplyr)
require(lubridate)
require(ggplot2)

####Reading/cleaning data ####
df <- read.csv("~/Desktop/Data/R/Data Files/MIRA_RIVER_ALL_YEARS_TIME&STATION_CORRECTED.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")
df <- df[-c(4,5,9,10)]


names(df) <-c("date","vr2","tag", "depth", "unit", "station")
df$id <- as.factor(unlist(strsplit(df$tag, split = "-"))[3*(1:length(df$tag))]) 
df$receiver <- as.character(unlist(strsplit(df$vr2, split = "-"))[2*(1:length(df$vr2))])

df$ddate <- ymd(substr(df$date, 1, 10))
df$date <- ymd_hms(df$date, tz = "UTC")
df$day <- day(df$date)
df$month <- month(df$date)
df$year <- year(df$date)

df$depth <- 0.43970*df$depth-1.7587 #need to set to 1 decimal place? 
df$unit <- "m"


##############tagging metadata
# make sure that the tag md df is not screwed up by excel formatting going to the csv (creating blank spaces causing 'NA's)
tags.df <- read.csv("~/Desktop/Data/R/Data Files/BSB_tagging.csv", stringsAsFactors = FALSE, skip = 4, header = TRUE, sep = ",", dec = "." )
tags.df<- tags.df[1:length(tags.df[grep("VEMCO",tags.df$TAG_MANUFACTURER)]),] ## remove the NA's introduced during the import

# set up date and time within the tags.df dataframe
tags.df$date <- gsub("T", " ", tags.df$UTC_RELEASE_DATE_TIME,) # this allows you to convert date/time md to posix
tags.df$ddate <-  ymd(substr(tags.df$date, 1, 10))
tags.df$date <- ymd_hms(tags.df$date, tz = "UTC")
#tags.df <- tags.df[,-c(33:65)]
tags.df$id <- as.factor(tags.df$TAG_ID_CODE) ## need to do this to keep str() same across dfs ## EDIT: CHANGED TO FACTOR ABOVE.. SEE HOW IT WORKS
tags.df <- tags.df[!tags.df$CAPTURE_LOCATION == "FULLER'S BRIDGE",]

#colin's code to filter out other tag id's that are not deployed by me 
# subset the det det based on my tag id's ## eliminated 2144 detections
tag.id <- as.list(as.character(na.omit(tags.df$TAG_ID_CODE)))
df <- df[df$id %in% tag.id,] 


#Clean up dataframes#
df <- arrange(df, date)
df <- distinct(df) # remove any duplicates 
unique(df$station)
#[1] "005"              "03"               "009"              "015"              "006"              "007"              "12"               "04"               "08"               "07"              
#[11] "02"               "01"               "06"               "14"               "09"               "11"               "16"               "13"               "13.5"             "20"              
#[21] "21"               "05"               "Range Test 100 m" "Range Test 200m"  "Range Test 300m"  "013"              "15"               "001"              "17"               "18"              
#[31] "19"               "10"               "0011"             "014"              "011"             

### Clean up stations -- Colin's script ###
df$station <- ifelse(df$station == "01"| df$station =="001", "1",
                     ifelse(df$station == "02"| df$station =="002", "2",
                            ifelse(df$station == "03"| df$station =="003", "3",
                                   ifelse(df$station == "04"| df$station =="004", "4",
                                          ifelse(df$station == "05"| df$station =="005", "5",
                                                 ifelse(df$station == "06"| df$station =="006", "6",
                                                        ifelse(df$station == "07"| df$station =="007", "7",
                                                               ifelse(df$station == "08"| df$station =="008", "8",
                                                                      ifelse(df$station == "09"| df$station =="009", "9",
                                                                             ifelse(df$station == "10"| df$station =="010", "10",
                                                                                    ifelse(df$station == "11"| df$station =="011"| df$station =="0011", "11",
                                                                                           ifelse(df$station == "12"| df$station =="012", "12",
                                                                                                  ifelse(df$station == "13"| df$station =="013", "13",
                                                                                                         ifelse(df$station == "14"| df$station =="014", "14",
                                                                                                                ifelse(df$station == "15"| df$station =="015", "15",
                                                                                                                       ifelse(df$station == "16"| df$station =="016", "16",
                                                                                                                              ifelse(df$station == "17"| df$station =="017", "17",
                                                                                                                                     ifelse(df$station == "18"| df$station =="018", "18",
                                                                                                                                            ifelse(df$station == "19"| df$station =="019", "19",
                                                                                                                                                   ifelse(df$station == "20"| df$station =="020", "20",
                                                                                                                                                          ifelse(df$station == "21"| df$station =="021", "21", "ERROR" ))))))))))))))))))))) 
## When I want to incorporate MR_13.5 in analysis, I can replace Error with df$station # acoustic data 

##### FILTERING ####  
df <- df[!df$station == "NA",]
df <- df[!df$station == "ERROR",] ### Tag id 33162  was used for a range test and this escaped above filtering
df$station <- as.numeric(df$station)

df.backup <- df

ow.stations <- c(10,11,12,13) #overwintering array
outside.array <- c(1:9,14:21) ## what about the change in station names? 

remove.tags.df <- NULL
for(i in tags.df$id){
  temp <- df[df$id == i & df$date < tags.df$depl_date[tags.df$id == i],]  
  len <- length(temp$date)
  temp$tagdate <- rep(tags.df$depl_date[tags.df$id == i], times = len)
  remove.tags.df <- rbind(remove.tags.df, temp)
}

df <- anti_join(df, remove.tags.df, by = c("date", "id")) ### this is a beautiful little piece of code right here... always remember "In Hadley We Trust"

df <- arrange(df, date, id)

##### Seasonal Movements ####

######### MOVEMENT DATA ***************************************************************************************************

##manually figure out when fish leave, make a table - in and out scandal####
in.out <- NULL
in.out$id[1] <- 48409
in.out$leave[1] <- "2013-09-04 01:54:47"
in.out$entry[1] <- "2013-10-09 05:33:48"

in.out$id[2] <- 48410
in.out$leave[2] <- "2012-09-06 08:47:45"
in.out$entry[2] <- "2012-10-24 04:29:54"

in.out$id[3] <- 48410
in.out$leave[3] <- "2013-08-02 02:19:07"
in.out$entry[3] <- "2013-10-07 07:35:31"

in.out$id[4] <- 48410
in.out$leave[4] <- "2014-07-27 04:42:36"
in.out$entry[4] <- "2014-09-09 06:38:27"

in.out$id[5] <- 48411
in.out$leave[5] <- "2013-08-29 23:59:23" # detected at station 5
in.out$entry[5] <- "2013-10-07 07:15:13" # returned to station 2 then # 7

in.out$id[6] <- 33158
in.out$leave[6] <- "2013-08-18 02:16:48" # 2
in.out$entry[6] <- "2013-10-03 05:17:30" # 2

in.out$id[7] <- 33158
in.out$leave[7] <- "2014-07-27 04:01:38" #1
in.out$entry[7] <- "2014-08-30 04:03:51" #1

in.out$id[8] <- 5729
in.out$leave[8] <- "2013-08-26 23:54:01" #2
in.out$entry[8] <- "2013-10-07 07:30:27" #2

in.out$id[9] <- 5736 ## caught and released on 2013-08-29T04:38:00
in.out$leave[9] <- "2013-09-27 23:29:33" #2 ### delayed exit could be due to tagging effect
in.out$entry[9] <- "2013-10-14 01:46:18" #2

in.out$leave <- ymd_hms(in.out$leave)
in.out$entry <- ymd_hms(in.out$entry)
in.out$duration <- round(as.period(new_interval(in.out$leave, in.out$entry), "seconds")/(3600*24), digits = 2)
in.out$year <- year(in.out$leave)
in.out <- data.frame(in.out)

in.out.summary<- ddply(in.out, .(year), summarise, 
                       n = length(unique(id)),
                       mean_date_leave = mean(leave),
                       mean_date_return = mean(entry),
                       mean_duration = mean(duration),
                       sd = sd(duration))

in.out.summary$sd_leave[1] <- sd(in.out$leave[in.out$year == 2012])/(3600*24)
in.out.summary$sd_leave[2] <- sd(in.out$leave[in.out$year == 2013])/(3600*24)
in.out.summary$sd_leave[3] <- sd(in.out$leave[in.out$year == 2014])/(3600*24)

in.out.summary$sd_entry[1] <- sd(in.out$entry[in.out$year == 2012])/(3600*24)
in.out.summary$sd_entry[2] <- sd(in.out$entry[in.out$year == 2013])/(3600*24)
in.out.summary$sd_entry[3] <- sd(in.out$entry[in.out$year == 2014])/(3600*24)

#year n     mean_date_leave    mean_date_return mean_duration        sd    sd_leave    sd_entry
#1 2012 1 2012-09-06 08:47:45 2012-10-24 04:29:54        47.820        NA          NA          NA
#2 2013 6 2013-08-28 12:58:56 2013-10-08 01:49:47        40.535 16.267704 18.87939290 18.87939290
#3 2014 2 2014-07-27 04:22:07 2014-09-04 05:21:09        39.040  7.127636  0.02011653  0.02011653


## circular plot of fish leave time #### 

in.out$leave.utc <- hour(in.out$leave)
in.out$enter.utc <- hour(in.out$entry)
in.out$leave.adt <- c(22, 5, 23, 1, 20, 23, 1, 20, 20)
in.out$enter.adt <- c(2, 1, 4, 3, 4, 2, 1, 4, 22)

### is it dark at time of entry/exit -- day, dark, nautical twilight (http://www.nrc-cnrc.gc.ca/eng/services/sunrise/) used New Glasgow 
in.out$leave.sun <- c("dark", "light", "dark", "dark", "dark", "dark","dark", "dark","dark")
in.out$enter.sun <- c("dark", "dark", "dark","dark","dark","dark","dark","dark","dark")

p1 <- ggplot(in.out, aes(x = leave.adt, fill = leave.sun)) + geom_histogram(breaks = seq(0,24), width = 1) + coord_polar(start = 0, direction = 1) + scale_x_continuous(breaks = seq(0,24), labels = seq(0,24)) + xlab("Hour of Day (AST)") + ggtitle("Departure Time") + theme(axis.text = element_text(size = 30), axis.title = element_text(size =30), plot.title = element_text(size = 30)) +
  scale_fill_manual(values = c("black", " dark grey")) + theme_bw()
p1 

p2 <- ggplot(in.out, aes(x = enter.adt, fill = enter.sun))+  geom_histogram(breaks = seq(0,24), width = 1) + coord_polar(start = 0, direction = 1) + scale_x_continuous(breaks = seq(0,24), labels = seq(0,24)) + xlab("Hour of Day (AST)") + ggtitle("Return Time") + theme(axis.text = element_text(size = 30), axis.title = element_text(size =30), plot.title = element_text(size = 30)) +
  scale_fill_manual(values = c("black", "dark grey")) + theme_bw()
p2

ggsave(p1, file = "DeparturePlot.png", height = 13, width = 13)
ggsave(p2, file = "ReturnPlot.png", height = 13, width = 13)




####How to automate above using a csv of sunrise, sunset, twilight
# code below to add colour to hourly breaching plot for night/day/twilight
# to read in sunset, sunrise and twilight times data

bramber.sun.2014 <- read.csv("Sunrise_set_Bramber_2014_forR.csv", stringsAsFactors = FALSE)


# to fix dates with lubridate (year had to be corrected to 2014 after Excel chose 2015 automatically)

bramber.sun.2014$Date <- ymd(bramber.sun.2014$Date) - years(1)

# to combine date and time columns for twilight, sunrise and sunset

bramber.sun.2014$nts <- paste(bramber.sun.2014$Date, bramber.sun.2014$Nautical.Twilight.Start, sep = " ")
bramber.sun.2014$nte <- paste(bramber.sun.2014$Date, bramber.sun.2014$Nautical.Twilight.End, sep = " ")
bramber.sun.2014$rise <- paste(bramber.sun.2014$Date, bramber.sun.2014$Sunrise, sep = " ")
bramber.sun.2014$set <- paste(bramber.sun.2014$Date, bramber.sun.2014$Sunset, sep = " ")

# to fix date formats
bramber.sun.2014$nts <- ymd_hm(bramber.sun.2014$nts)
bramber.sun.2014$nte <- ymd_hm(bramber.sun.2014$nte)
bramber.sun.2014$rise <- ymd_hm(bramber.sun.2014$rise)
bramber.sun.2014$set <- ymd_hm(bramber.sun.2014$set)

# to choose start and end times for DST
start <- ymd_hm("2014-03-09 02:00")
end <- ymd_hm("2014-11-02 01:59")

# to fix hours in nt/sunrise/sunset file (so that it is UTC-3 in summer)
bramber.sun.2014$nts <- ifelse(bramber.sun.2014$nts >= start & bramber.sun.2014$nts <= end, as.character(bramber.sun.2014$nts + hours(1)), as.character(bramber.sun.2014$nts))
bramber.sun.2014$nts <- ymd_hms(bramber.sun.2014$nts)

#bramber.sun.2014$nts <- ifelse(bramber.sun.2014$nts >= start & bramber.sun.2014$nts <= end, as.character(bramber.sun.2014$nts + hours(1)), as.character(bramber.sun.2014$nts))
#bramber.sun.2014$nts <- ymd_hms(bramber.sun.2014$nts)
#
#
# do above for other columns too... ie sunrise and sunset
# to rename stuff for simplicity in next part
sun <- bramber.sun.2014
sep27 <- sept.breaches.14P0027

# to set times of naut. twilight, day and night
sep27$sun.ast <- NULL
for(i in 1:length(sep27$date)){sep27$sun.ast[i] <- ifelse(sep27$timestamp.ast[i] >= sun$nts[sun$Date == sep27$date[i]] & sep27$timestamp.ast[i] < sun$rise[sun$Date == sep27$date[i]], "twilight",
                                                          ifelse(sep27$timestamp.ast[i] >= sun$rise[sun$Date == sep27$date[i]] & sep27$timestamp.ast[i] < sun$set[sun$Date == sep27$date[i]], "day",
                                                                ifelse(sep27$timestamp.ast[i] >= sun$set[sun$Date == sep27$date[i]] & sep27$timestamp.ast[i] < sun$nte[sun$Date == sep27$date[i]], "twilight", "night")))}

# to rename back to original names
sept.breaches.14P0027 <- sep27
bramber.sun.2014 <- sun


# to plot the number of breaches per hour (AST) with colours
plot.sept2.hour.ast.col <- ggplot(sept.breaches.14P0027, aes(x = hour.ast, fill = sun.ast)) + geom_histogram(breaks = seq(0, 24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_bw() + ylab("Count") + ggtitle("September 2014 Breaches by Hour (AST) 14P0027") + scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) + scale_fill_manual(values = c("dark grey", "black", "orange"))

# to view the plot
print(plot.sept2.hour.ast)


#### OVERWINTERING ***************************####

######### OVERWINTERING ******************************
## calculate OW exit stations and arrival at Albert Bridge####

leave.fun <- function(data, year, fish){ ## will work on data after 2013 since OW arrays were deployed thenceforth 
  
  first.det.out <- head(data[data$year == year & data$id == fish & data$station %in% outside.array,],1) ## first detection in outside array
  first.det.out$first.albert <- head(data$date[data$year == year & data$id == fish & data$station == 7],1) ## first albert bridge detection
  last.ow.det <- tail(data[data$year == year & data$id == fish & data$date < first.det.out$date,],1) ## last detection in the OverWintering area 
  end.ow <- merge(last.ow.det, first.det.out, by="id") # merge the two (long format, maybe want to make it short format)
  
  return(end.ow)
}

leave.master=NULL
for (i in unique(df$id)){
  for(j in unique(df$year[df$year >= 2014])){
    output=leave.fun(df,j,i)
    leave.master=rbind(leave.master, output)
  }}


############################
##########################
#######################
######################
##################
#### NEEED TO FIX THIS UP... won't work with data$first.albert WHY THE FUCK NOT?
## also figure out how to label this shit. 
leave.fun.2013 <- function(data, fish){ # a lot of unnecessary code here, but it could be decent 
  first.det.out <- head(data[data$year == 2013 & data$month < 6 & data$id == fish,],1)
  first.det.out$first.albert <- as.character(head(data$date[data$year == 2013 & data$id == fish & data$station == 7],1)) ## no idea why this doesn't work
  last.ow.det <- head(data[data$year == 2013 & data$month < 6 & data$id == fish & data$station %in% outside.array,],1)
  end.ow <- merge(last.ow.det, first.det.out, by ="id")
  return(end.ow)
}


leave.master.2013 = NULL
for(i in unique(df$id[df$year == 2013 & df$month <= 6])){
  output = leave.fun.2013(df,i)
  leave.master.2013 = rbind(leave.master.2013, output)
}


leave.master.2013$first.albert <- leave.master.2013$ddate.y ### for some reason when I tried to include a first albert column in leave.fun.2013 I got an error "replacement has 1 row, data has 0"... wtf? 


leave.df <- rbind(leave.master, leave.master.2013)

trash <- leave.df 

leave.df <- leave.df[c(1,2,8,7,13,19,18,23,24)]

names(leave.df) <- c("id", "last_ow_date", "vr2_ow", "last_ow_stn", "first_out_date", "vr2_out", "first_out_stn", "year", "first_albert")


leave.df <- leave.df[!leave.df$id == 48418,]
leave.df <- arrange(leave.df, year, id)

leave.df$winter <- ifelse(leave.df$year == 2013, "2012-2013", ## create a winter category so we can figure out how to match up with entry data 
                          ifelse(leave.df$year == 2014, "2013-2014",
                                 ifelse(leave.df$year == 2015, "2014-2015", "ERROR")))

## calculate entrance to OW area and when they've left albert Bridge ####

enter.fun <- function(data, year, fish){
  
  if(year == 2012){
    last.detection.out <- tail(data[data$year == 2012 & data$id == fish & data$station %in% outside.array,], 1)
    first.detection.in <- tail(data[data$year == 2012 & data$id == fish & data$station %in% outside.array,], 1)
    last.albert <- tail(data[data$year == 2012 & data$id == fish & data$station == 7, c(7,1)], 1)
    names(last.albert) <- c("id", "last_albert")
  } else{
    last.detection.out <- tail(data[data$year == year & data$id == fish & data$station %in% outside.array,], 1)
    first.detection.in <- head(data[data$year == year & data$id == fish & data$date > last.detection.out$date & data$station %in% ow.stations,], 1)
    last.albert <- tail(data[data$year == year & data$id == fish & data$station == 7, c(7,1)], 1)
    names(last.albert) <- c("id", "last_albert") 
  }
  
  start.ow <- merge(last.detection.out, first.detection.in, by = "id")
  start.ow <- merge(start.ow, last.albert, by = "id")
  
  return(start.ow)
}


enter.df <- NULL 
for(i in unique(df$id)){ ### this takes 1 min 45 seconds on my (CBU) computer
  for(j in unique(df$year)){
    output = enter.fun(df, j, i)
    enter.df <- rbind(enter.df, output)
  }
}

enter.df <- enter.df[c(1,2,3,7,12,13,17,21,22)] ## change out 2 & 12 for 8 and 18
names(enter.df) <- c("id", "out_date", "out_vr2", "out_station", "ow_date", "ow_vr2", "ow_station", "year", "last_albert")
enter.df$winter <- ifelse(enter.df$year == 2012, "2012-2013",
                          ifelse(enter.df$year == 2013, "2013-2014",
                                 ifelse(enter.df$year == 2014, "2014-2015", "ERROR"))) 

enter.df <-enter.df %>% ## for some reason the enter.fun produces duplicates likely to do with the if statement 
  arrange(year, id) %>%
  distinct()
## Figure out how long fish were overwintering for ######
##- will define as from first detection in the OW area to the last detection in the ow area 

ow.df <- merge(enter.df, leave.df, by = c("id","winter")) ## only 22 records for this 
ow.df <- arrange(ow.df, winter)
ow.df$ow_duration <- day(as.period(new_interval(ow.df$ow_date, ow.df$last_ow_date), "days"))
ow.df$mig_duration <- as.period(new_interval(ow.df$last_albert, ow.df$ow_date))
ow.summary <- ddply(ow.df, .(id, winter), summarise, 
                    last.albert = last_albert,
                    start.ow = ow_date,
                    end.ow = last_ow_date,
                    ow.duration = ow_duration,
                    last.out = out_station,
                    first.ow = ow_station,
                    last.ow = last_ow_stn,
                    first.out = first_out_stn)
## figure out how long fish took to migrate upriver####

enter.df$mig_duration <- as.duration(new_interval(enter.df$last_albert, enter.df$ow_date)) 

## need to convert all to hours or days from duration
## NOTE **** CAN'T USE 2012 MIGRATION TIMES AS WE HAVE NO OW ARRAY WITHIN THE SYSTEM

mig.dur <- ddply(enter.df, .(winter), summarise, 
                 n = length(unique(id)),
                 mean_mig_dur = mean(mig_duration)/(60*60*24),## days
                 sd_mig_dur = sd(mig_duration)/(60*60*24)) ## days

#MIGRATION HOURS
#   winter  n mean_mig_dur sd_mig_dur
#1 2012-2013 14     77.23222  230.73869
#2 2013-2014 19     77.20889  124.97298
#3 2014-2015 10     30.32306   25.61724

#MIGRATION DAYS
#   winter  n mean_mig_dur sd_mig_dur
#1 2012-2013 14     3.218009   9.614112
#2 2013-2014 19     3.217037   5.207207
#3 2014-2015 10     1.263461   1.067385


# SPRING MOVEMENTS *************************

### RESIDENCY INDEX ************************



#### MORPHOMETRICS #


######### RESIDENCY INDEX (Ir)************************

## Read in station info ####
stn.df <- read.csv("~/Desktop/Data/R/Data Files/bsb_condensed_metadata_2012-2015_UpdatedStations.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")
#1] "STATION_NO"                               "DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss." "INS_SERIAL_NO"                            "X"                                        "STATION_NO.1"                            
#[6] "INS_SERIAL_NO.1"                          "RECOVERED..y.n.l.f."                      "RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss." 

stn.df <-stn.df[,-c(4:7)]
names(stn.df)<- c("station", "depl_date", "sn", "recov_date")
stn.df$station <- as.numeric(stn.df$station)
stn.df$depl_date <- gsub("T", " ", stn.df$depl_date,) ; stn.df$recov_date <- gsub("T", " ", stn.df$recov_date,) ### changing the date format - 3 & 10 refer to deploymenet and recovery date columns, respectively
stn.df$depl_date <- ymd_hms(stn.df$depl_date, tz = "UTC")
stn.df$recov_date <- ymd_hms(stn.df$recov_date, tz = "UTC")
## calculate the period of activity for tag and station ####

# tag - identifying tag deployment, and last day of activity of tag, then create an interval of tag activity 
tag.depl <- tags.df[,c("id", "date")]
names(tag.depl) <- c("id","depl_date")

tag.end <- mr.df %>%
  group_by(id) %>%
  select(id, date) %>%
  summarise(
    end_date = max(date)
  )
tag.df <- merge(tag.depl, tag.end, by = "id")

tag.df$tag_int <- new_interval(tag.df$depl_date, tag.df$end_date)
tag.df$days_active <- tag.df$end_date - tag.df$depl_date

# station  - use the station metadata to create an interval of deployment and recovery 
stn.df$station_int <- new_interval(stn.df$depl_date, stn.df$recov_date)
stn.df$days_active <- stn.df$recov_date - stn.df$depl_date

backup.stn.df <- stn.df
## remove tag data associated with tags prior to their official deployment ####
remove.tags.df <- NULL
for(i in tag.df$id){
  temp <- mr.df[mr.df$id == i & mr.df$date < tag.df$depl_date[tag.df$id == i],]  
  len <- length(temp$date)
  temp$tagdate <- rep(tag.df$depl_date[tag.df$id == i], times = len)
  remove.tags.df <- rbind(remove.tags.df, temp)
}

mr.df <- anti_join(mr.df, remove.tags.df, by = c("date", "id")) ### this is a beautiful little piece of code right here... always remembe
## write everything to csv ####
#write.csv(mr.df, file = "MiraRiver_StripedBass_AllDetects_Cleaned_2015-06-25.csv", row.names = FALSE, sep = ",")
#write.csv(stn.df, file = "MiraRiver_StripedBass_StationMetadata_2015-06-25.csv", row.names = FALSE)
#write.csv(tag.df, file = "MiraRiver_StripedBass_TagMetadata_2015-06-25.csv", row.names = FALSE)
## Create summary of Detections, per day, per station, per fish####
# use dplyr

det.pday <- mr.df %>%
  group_by(ddate, id, station) %>% # maybe just ddate
  select(id, station) %>% ## think about using station or receiver sn. 
  summarise(
    detpday = length(id)
  )


arrange(det.pday, ddate, id)
## Create Ir number of days detected within ENTIRE array####
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



## Creating a function to calculate the IR for each fish based on fish id, detection data, and station data

#The InteFunc incorporates ID (fish.id), tag deployment history (fish.data), and station deployment/recovery metadata (station.data)
# first line of code creates the number of days the Fish (ID) activity/deployment overlaps with the station deployment RESULTING in possible detection days
# The FOR LOOP : first line creates a space to build up the data that's extracted during each loop iteration
# The loop is calculating the number of possible days a fish can be detected for each staiton 
# SEE annotations next to each line of code to understand operation
## Code to Produce All possible detection days depending on deployment history ####
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
  station.data$act.station.day <- day(as.period(Intersect(study.window,station.data$station_int), "days")) + 1 # to get over the 1 second hump from 29 to 30 or 30 to 31 days
  
  station.data$days.possible <- day(as.period(Intersect(fish.data$tag_int2[fish.data$id == fish.ID],
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
## create start and end times (months) for the IRview ####
study.month <- NULL
study.month$start <- as.character(ymd_hms("2014-11-01 00:00:01") + months(0:6)) # why did I do this
study.month$end <- as.character(ymd_hms("2014-05-31 23:59:59") %m+% months(0:6)) # why did I do this?


master=NULL
system.time(for (i in 1:length(unique(tag.df$id))){ ### approx 4 min to do
  for(j in 1:length(study.month$start)){
    output=InteFunc(unique(tag.df$id)[i],tag.df, stn.df, study.month$start[j], study.month$end[j] )#start.date, end.date) ## this is saying all you need is the unique tag ID's to operate the InteFunc
    output$year <- year(study.month$start[j])
    output$month <- month(study.month$start[j])
    master=rbind(master, output)
  }})


ir.station <-  mr.df %>%
  group_by(id, year, month, station) %>%
  select(id, ddate) %>%
  summarise(
    days.detected = n_distinct(ddate)
  )

ir.master <- merge(ir.station, master, by = c("id", "station", "year", "month")) # the merging will drop tag id's not present in the mr.df during study period of interest... common merge. 

ir.master$ir <- ir.master$days.detected/(ir.master$days.possible + 1)
ir.master$ir.adjusted <- ir.master$ir * (ir.master$station.actual/ir.master$station.possible)

ir.master$station <- as.factor(ir.master$station)

ggplot(ir.master[ir.master$station %in% c(7,9,10,11,12,13,14,15),], aes(as.factor(month), ir.adjusted)) + geom_boxplot() + facet_grid(~station)

ddply(ir.master, .(station, month), summarise, mean(ir.adjusted))

ggplot(mr.df[mr.df$date >= ymd("2014-11-01")&mr.df$date <= ymd("2015-05-25"),], aes(month, depth))+ geom_point() +facet_grid(~station)
## circular plots of IR master ####

p1 <- ggplot(ir.master, aes(x = ir.adjusted)) + geom_histogram() + coord_polar(theta = "x", start = 1, direction = 1) + facet_wrap(~station)
p1 
## residency bar charts ####
ow.bar <- ggplot(ir.ow.2014, aes(as.factor(ir), station), group = (id)) + geom_bar(stat = 'identity')
ow.bar



######### DEPTH DATA ******************************************************************************************************
depth.df <- df[df$id %in% c(5720:5743),]
ggplot(depth.df, aes(date, depth)) + geom_point() + facet_wrap(~id)




###### LEAVING THE SYSTEM ***************** 

#### TEMP, SALIN, ETC. **********************************************************************************

######## MORPHOMETRIC STUFF***************************
#### tagging metadata ####
tags.df$tag_life <- as.numeric(gsub(" days", "", tags.df$EST_TAG_LIFE))
tagging.summary <- tags.df %>%
  group_by(year(date), TAG_MODEL) %>%
  summarise(
    n = length(unique(id)),
    meanFL = mean(LENGTH..cm.),
    minFL = min(LENGTH..cm.),
    maxFL = max(LENGTH..cm.),
    sdFL = sd(LENGTH..cm.))

tagging.summary$tagLife[1] <- 793
tagging.summary$tagLife[2] <- 162
tagging.summary$tagLife[3] <- 881
tagging.summary$tagLife[4] <- 162
tagging.summary$tagLife[5] <- 162


#year(date) TAG_MODEL  n   meanFL minFL maxFL      sdFL tagLife
#1       2012    V13-1L  6 76.81667  57.0 116.6 21.550816     793
#2       2012   V13P-1H  8 71.62500  58.9  79.9  8.020821     162
#3       2013    V13-1L  5 68.40000  59.2  84.6  9.906563     881
#4       2013   V13P-1H 10 56.80000  46.3  77.5 12.039288     162
#5       2014   V13P-1H  3 45.40000  29.5  56.7 14.171450     162
#TOTAL                  32  65.0      29.5  116.6   16.0

######### TEMP, SALINITY, DO WORKINGS ****************
## 2014 overwintering temp loggers #####
ow3.10 <- read.csv("~/Desktop/Data/R/Data Files/MR_OWT3_10m.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")
ow3.10 <- ow3.10[,1:2]
names(ow3.10) <- c("date", "temp")
ow3.10$ddate <- ymd(substr(ow3.10$date, 1, 10))
ow3.10$date <- ymd_hms(ow3.10$date)
ow3.10 <- ow3.10[ow3.10$date < ymd("2015-05-25"),]

ow3.mean <- ow3.10 %>%
  group_by(ddate) %>%
  summarise(
    mean_temp = mean(temp)
  )

ggplot(ow3.mean, aes(ddate, mean_temp)) + geom_line() 

mr7 <- read.csv("~/Desktop/Data/R/Data Files/MR_07_2012-2015.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")
mr7 <- mr7[,1:2]
names(mr7) <- c("date", "temp")
mr7$ddate <- ymd(substr(mr7$date, 1, 10))
mr7$date <- ymd_hms(mr7$date)
mr7 <- mr7[mr7$date < ymd("2015-05-25"),]

mr7.mean <- mr7 %>%
  group_by(ddate) %>%
  summarise(
    mean_temp = mean(temp)
  )

ggplot(mr7.mean[mr7.mean$ddate >= ymd("2014-11-01"),], aes(ddate, mean_temp)) + geom_line() + geom_vline(aes(xintercept = e.2014))


## need to figure out what the temps are for the window of upriver migration 

s.2012 <- ymd(min(ow.df$last_albert[ow.df$winter == "2012-2013"])) # 2012-11-21
e.2012 <- ymd(max(ow.df$last_albert[ow.df$winter == "2012-2013"])) # 2012-11-25

s.2013 <- ymd(min(ow.df$last_albert[ow.df$winter == "2013-2014"])) # 2013-11-12
e.2013 <- ymd(max(ow.df$last_albert[ow.df$winter == "2013-2014"])) # 2013-11-21

s.2014 <- ymd(min(ow.df$last_albert[ow.df$winter == "2014-2015"])) # 2014-11-20
e.2014 <- ymd(max(ow.df$last_albert[ow.df$winter == "2014-2015"])) # 2014-11-22

mean(mr7$temp[mr7$date >= s.2012 & mr7$date <= (e.2012+days(1))]) # 9.1°C
mean(mr7$temp[mr7$date >= s.2013 & mr7$date <= (e.2013+days(1))]) # 8.6°C
mean(mr7$temp[mr7$date >= s.2014 & mr7$date <= (e.2014+days(1))]) # 6.3°C

p.2014.last.albert <- ggplot(mr7.mean, aes(ddate, mean_temp)) + geom_line() + geom_vline(xintercept = as.numeric(s.2012)) + geom_vline(xintercept = as.numeric(e.2012)) + geom_vline(xintercept = as.numeric(s.2013))+                                                                                                                                                                                                geom_vline(xintercept = as.numeric(e.2013)) + geom_vline(xintercept = as.numeric(s.2014)) + geom_vline(xintercept = as.numeric(e.2014)) + scale_x_datetime(breaks = date_breaks("1 months"), labels = date_format("%b %Y"), limit = c(ymd("2014-11-01"), ymd("2014-12-30")))

p.2013.last.albert <- ggplot(mr7.mean, aes(ddate, mean_temp)) + geom_line() + geom_vline(xintercept = as.numeric(s.2012)) + geom_vline(xintercept = as.numeric(e.2012)) + geom_vline(xintercept = as.numeric(s.2013))+                                                                                                                                                                                                geom_vline(xintercept = as.numeric(e.2013)) + geom_vline(xintercept = as.numeric(s.2014)) + geom_vline(xintercept = as.numeric(e.2014)) + scale_x_datetime(breaks = date_breaks("1 months"), labels = date_format("%b %Y"), limit = c(ymd("2013-11-01"), ymd("2013-12-30")))

p.2012.last.albert <- ggplot(mr7.mean, aes(ddate, mean_temp)) + geom_line() + geom_vline(xintercept = as.numeric(s.2012)) + geom_vline(xintercept = as.numeric(e.2012)) + geom_vline(xintercept = as.numeric(s.2013))+                                                                                                                                                                                                geom_vline(xintercept = as.numeric(e.2013)) + geom_vline(xintercept = as.numeric(s.2014)) + geom_vline(xintercept = as.numeric(e.2014)) + scale_x_datetime(breaks = date_breaks("1 months"), labels = date_format("%b %Y"), limit = c(ymd("2012-11-01"), ymd("2012-12-30")))

