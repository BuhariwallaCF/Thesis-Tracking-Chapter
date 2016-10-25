# 2016-06-27 Colin F Buhariwalla
# Station Cleaning function 

# see source input file for structure (compiled using metadata and excel)


station_cleaning_fun <- function(data){
  require(lubridate)
  data <-data[,-c(4:7)]
  names(data)<- c("station", "depl_date", "sn", "recov_date")
 #data$station <- as.numeric(data$station) # nah, don't like this.. categorical data shouldn't be doing this shit here. 
  data$depl_date <- gsub("T", " ", data$depl_date,) ; data$recov_date <- gsub("T", " ", data$recov_date,) ### changing the date format - 3 & 10 refer to deploymenet and recovery date columns, respectively
  data$depl_date <- ymd_hms(data$depl_date, tz = "UTC")
  data$recov_date <- ymd_hms(data$recov_date, tz = "UTC")
  
  #need to remove stns 13.5,100.0,200.0 350.0
  
  data <- data[!data$station %in% c("13.5", "100.0","200.0","350.0"),]
  return(data)
}

#[1] "STATION_NO"                               "DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss." "INS_SERIAL_NO"                            "X"                                       
#[5] "STATION_NO.1"                             "INS_SERIAL_NO.1"                          "RECOVERED..y.n.l.f."                      "RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss." 