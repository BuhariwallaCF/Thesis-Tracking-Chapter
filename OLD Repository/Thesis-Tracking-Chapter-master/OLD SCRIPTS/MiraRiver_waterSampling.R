#### info ####
# 2014-08-09 C.F. Buhariwalla

# Mira River water sampling 2013 - 

# YSI 63 used for temp, salinity (PSU), pH, 
# YSI 550a used for temp, DO1 & DO2 (mg/L & %, respectively)

#### required packages ####
require(plyr)
require(lubridate)
require(ggplot2)
require(doBy)
require(reshape2)

#### read in csv files ####
#files below have a different number of columns 
files <- dir("/Users/colinbuhariwalla/Desktop/Data/water sampling/water sampling files", full.name = TRUE) # get the names of the csv files 
ws <- files[grep("mr", files)] #make a list of all ws files
ws <- lapply(ws, "read.csv", header = TRUE, dec = ".", stringsAsFactors = FALSE)
ws <- rbind.fill(ws) ## plyr function rbinds but will allow for dissimilar number of columns to be read in together 

#### clean data frame ####
ws.backup <- ws

# names 
print(names(ws))
#[1] "date"                   "week"                   "station"                "depth"                  "ph"                     "temp1"                 
#[7] "salinity"               "dissolved.oxygen"       "bottom.depth"           "comments"               "weather"                "air"                   
#[13] "water"                  "depth3"                 "start.time"             "end.time"               "north"                  "west"                  
#[19] "station.2"              "old.station.from.entry" "dissolved.oxygen2"      "distance"               "ENTERING.COMMENTS"      "temp2"

ws <- ws[!is.na(ws$week),] # remove 1,000,000 rows of NA (likely resulting from the empty spaces at the end of the excel sheet... fucking excel)
ws$station <- factor(ws$station) # not sure if this will help us now.. maybe i'll change it to character vector 
backup <- ws

ws <- ws[-c(10:13,15,16,19,20,22,23)] # removing above columns that are not necessary... refer to backup for the mysteries the deleted columns withhold  

for(i in 1:length(ws$week)){
  if(is.na(ws$temp2[i])){
    ws$temp[i] <- ws$temp1[i]
  } else {
    ws$temp[i] <- ws$temp2[i]}}



backup <- ws

# further remove shit we don't need
#[1] "date"              "week"              "station"           "depth"             "ph"               
#[6] "temp1"             "salinity"          "dissolved.oxygen"  "bottom.depth"      "depth3"           
#[11] "north"             "west"              "dissolved.oxygen2" "temp2"             "temp"             
> 
ws <- ws[-c(5,6,10,14)]

names(ws) <- c("date", "week", "station", "depth", "salin", "do1", "bot.depth", "lat", "lon", "do2", "temp")
ws$date <- ymd(ws$date)
ws$month <- month(ws$date, label = TRUE, abbr = TRUE) # label == TRUE gives month label as character string vs numeric, abbr = TRUE -> January = Jan 


## this just in, to make life easier i need to reshape the data to long format - will help with plotting, summary functions, etc. 

wsl <- melt(ws[-c(8,9)], id.vars = c("date", "month", "week", "station", "depth"), variable.name = "phys_parameter", value.name = "param_value")

#### quick dirty plots ####

qplot(x = depth, y = temp, data = ws[ws$week == 7,], geom = "line", facets = .~station) # just testing it out

qplot(x = depth, y = temp, data = ws[ws$station == 19,], geom = "line", facets = .~week)

qtemp <- function(station){
p <-  qplot(x = depth, y = temp, data = ws[ws$station == station,], geom = "line", facets = week~.)
  return(p)
}

qsalin.week <- function(station){
  p <-  qplot(x = depth, y = salin, data = ws[ws$station == station,], geom = "line", facets = .~week)
  return(p)
}

qsalin.month <- function(station){
  p <-  qplot(x = depth, y = salin, data = ws[ws$station == station,], geom = "line", facets = .~month)
  return(p)
}
#### summary of the temp and salinity ####

## does not work... need to figure out how to pass functions on to the main dataframe
sum.fun <- function(df){
{
          surface <- which(df$depth == min(df$depth))
          bottom <- which(df$depth == max(df$depth))
        
          top.3 <- length(which(df$depth <= 3.0))-1
          bottom.3 <- length(which(df$depth >= (max(df$depth)-3.0)))
}
          
  surface.salin <- (c(df$salin[surface:(surface+top.3)]))
  surface.temp  <- (c(df$salin[surface:(surface+top.3)]))

  bottom.salin <- (c(df$salin[(bottom-bottom.3):bottom]))
  bottom.temp <- (c(df$salin[(bottom-bottom.3):bottom]))


summary<- c(mean(surface.salin), mean(surface.temp), mean(bottom.salin), mean(bottom.temp))

return(summary)
}


ws.summary.fun <- function(x){ ddply(ws[ws$station %in% c(x),], .(station, month), summarize, 
                    mean.salin = round(mean(salin),1),
                    min.salin = min(salin),
                    max.salin = max(salin),
                    mean.temp = round(mean(temp),1),
                    min.temp = min(temp),
                    max.temp = max(temp),
                    max.do1 = max(do1),
                    min.do1 = min(do1),
                    max.do2 = max(do2),
                    min.do2 = min(do2),
                    .progress = "text" )  }

      
ws.summary <- ddply(ws[ws$station %in% c(23,19,7,4),], .(station, month), summarize, 
      mean.salin = round(mean(salin),1),
      min.salin = min(salin),
      max.salin = max(salin),
      mean.temp = round(mean(temp),1),
      min.temp = min(temp),
      max.temp = max(temp),
      max.do1 = max(do1),
      min.do1 = min(do1),
      max.do2 = max(do2),
      min.do2 = min(do2),
      .progress = "text" )      


#### get the coordinates for each water sampling station ####
### need to do this so I can plot it quickly on the other plots and create a table with values
df.sort.station <- ws[order(ws$week),]
ws.station.coord <- df.sort.station[!duplicated(ws$station), c(3,8,9)]

ws.station.afs <- ws.station.coord[ws.station.coord$station %in% ws.summary$station,]

ws.station.afs$station <- c(4,3,1,2) ## rename the stations (lowest being from downriver, working way upriver)

names(ws.station.afs) <- c("station", "lat", "lon") ## this is to be consistent with the Overwintering names... has been fixed in code above, so this can be deleted upon compilation of script 
# for AFS plots 

#### delete at the end of the session #### 
