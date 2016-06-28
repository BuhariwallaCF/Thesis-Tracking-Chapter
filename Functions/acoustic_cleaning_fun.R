# 2016-06-27 Colin F Buhariwalla
# Acoustic Telemetry Cleaning function 
# File must be in VUE output format :
# [1] "Date.and.Time..UTC." "Receiver"            "Transmitter"         "Transmitter.Name"    "Transmitter.Serial"  
#"Sensor.Value"        "Sensor.Unit"         "Station.Name"  
#[9] "Latitude"            "Longitude" 

acoustic_cleaning_fun <- function(data)
{
  data <- data[-c(4,5,9,10)] # [1] "Date.and.Time..UTC." "Receiver"            "Transmitter"         "Transmitter.Name"    "Transmitter.Serial"  
  #"Sensor.Value"        "Sensor.Unit"         "Station.Name"  
  #[9] "Latitude"            "Longitude" 
  names(data)  <- c("date","receiver","tag","sensor","unit","station")
  data$id <- unlist(strsplit(data$tag, split = "-"))[3*(1:length(data$tag))] # separate the elements of the tag ID vector to isolate strictly IDs 
  
  data$ddate <- ymd(substr(data$date, 1, 10))
  data$date <- ymd_hms(data$date, tz = "UTC")
  data$day <- day(data$date)
  data$month <- month(data$date)
  data$year <- year(data$date)

}