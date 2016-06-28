# 2016-06-28 Colin F Buhariwalla
# Create a function to automatically parse dates and times on readin of data frame

# this is to parse dates and times of all cleaned up data frames that I read in
# columns required (any of the following): date, ddate, depl_date, recov_date, days_active


dates_and_times_fun <- function(df){
  
  require(lubridate)
  
  if(length(df$date)>= 1){
    df$date <- ymd_hms(df$date, tz = "UTC")
  }
  if(length(df$depl_date) >= 1){
    df$depl_date <- ymd_hms(df$depl_date, tz = "UTC")
  }
  if(length(df$recov_date) >= 1){
    df$recov_date <- ymd_hms(df$recov_date, tz = "UTC")
    df$activity_interval <- new_interval(df$depl_date, df$recov_date)
  }
  if(length(df$ddate)>=1){
    df$ddate <- ymd(df$ddate, tz = "UTC")
  }
  if(length(df$days_active)>= 1){
    df$days_active <- as.difftime(as.numeric(df$days_active), units="days")
  }
  return(df)
}



#dates_and_times_fun <- function(df){
  
#  require(lubridate)
  
#  if(length(df$date)>= 1){
#    df$date <- strptime(df$date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
#  }
#  if(length(df$depl_date) >= 1){
#    df$depl_date <- strptime(df$depl_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
#  }
#  if(length(df$recov_date) >= 1){
#    df$recov_date <- strptime(df$recov_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
#    df$activity_interval <- new_interval(df$depl_date, df$recov_date)
#  }
 # if(length(df$end_date)>=1){
 #   df$end_date <- strptime(df$end_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
#  }
#  if(length(df$ddate)>=1){
#    df$ddate <- strptime(df$ddate, "%Y-%m-%d", tz = "UTC")
#  }
#  if(length(df$days_active)>= 1){
#    df$days_active <- as.difftime(as.numeric(df$days_active), units="days")
#  }
#  return(df)
#}


# it appears that lubridte would have worked for this. 
# shit. 