#2016-08-11 Colin F. Buhariwalla
# READ IN THE LEAVING EVENT FILE


left_and_leaving_fun <- function(df){
  require(lubridate)
  
  df <- read.csv("data/BSB_MiraRiver_LeavingEvents.csv", stringsAsFactors = F)
  
  df$exit <- ymd_hms(df$exit)
  df$enter <- ymd_hms(df$enter)
  df$time_away <- duration(df$time_away, units = "seconds")
  
  return(df)
}