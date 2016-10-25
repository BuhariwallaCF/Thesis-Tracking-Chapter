# 2016-06-27 Colin F Buhariwalla
# Acoustic Telemetry Cleaning function 
# File must be in VUE output format : WRONG. OTN DETECTION FILES
# [1] "Date.and.Time..UTC." "Receiver"            "Transmitter"         "Transmitter.Name"    "Transmitter.Serial"  
#"Sensor.Value"        "Sensor.Unit"         "Station.Name"  
#[9] "Latitude"            "Longitude" 

acoustic_cleaning_fun <- function(data)
{
  require(dplyr)
  require(lubridate)
  #data <- data[-c(4,5,9,10)] # [1] "Date.and.Time..UTC." "Receiver"            "Transmitter"         "Transmitter.Name"    "Transmitter.Serial"  
  #"Sensor.Value"        "Sensor.Unit"         "Station.Name"  
  #[9] "Latitude"            "Longitude" 
  #names(data)  <- c("date","receiver","tag","sensor","unit","station")
  
  # the following will work for all matched detection files from OTN (above is for VUE exports)
  data$date <- ymd_hms(data$datecollected)
  #data$id <- unlist(strsplit(data$tagname, split = "-"))[3*(1:length(data$tagname))] # separate the elements of the tag ID vector to isolate strictly IDs 
  data$id <- sapply(strsplit(data$tagname, split = "-"), "[", 3) # this is more consistent/predictable than the unlist above. 
  #data$receiver.model <- as.character(unlist(strsplit(data$receiver, split = "-"))[seq(from = 1, to = 2*length(data$receiver), by = 2)]) # this sequence is required since strsplit breaks the character vector into two
  # and unlist causes it all to go into one vector. 
  #data$receiver <- as.character(unlist(strsplit(data$receiver, split = "-"))[2*(1:length(data$receiver))])

  data$ddate <- ymd(substr(data$date, 1, 10))
  data$date <- ymd_hms(data$date, tz = "UTC")
  data$day <- day(data$date)
  data$month <- month(data$date)
  data$year <- year(data$date)
  data <- arrange(data, date)
  return(data)
}



