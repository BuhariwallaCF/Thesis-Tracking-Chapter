#2016-08-14 Colin F. Buhariwalla
#NRC_sunrise_sunset_cleaning_fun

#DATA SOURCE:
#http://www4.rncan.gc.ca/search-place-names/unique/CAZVS
#http://www.nrc-cnrc.gc.ca/eng/services/sunrise/advanced.html


NRC_sunrise_sunset_cleaning_fun <- function(df){
  #Function_NOTES: 
  #must have columns:
  #"Date","Nautical.Twilight.Start","Sunrise","Sunset","Nautical.Twilight.End","Local.Sidereal.Time..00.00.STD..hhmmss","year"
  # **** YEAR column needs to be added to RAW CSV upon download
  names(df) <- c("date", "nautical.twilight.start.time","sunrise.time","sunset.time","nautical.twilight.end.time","sidreal", "year")
  df$date <- paste(df$year, df$date, sep = "-")
  df$date <- gsub(" ", "-", df$date)
  df$date <- as.character(as.Date(df$date, "%Y-%B-%d"))
  
  df$ntstart <- ymd_hm(paste(df$date, df$nautical.twilight.start.time, sep = " "), tz = "America/Halifax") #pasting together nautical twilights start 
  df$ntend <- ymd_hm(paste(df$date, df$nautical.twilight.end.time, sep = " "),tz = "America/Halifax") # pasting together nautical twilight end
  df$sunrise <- ymd_hm(paste(df$date, df$sunrise.time, sep = " "),tz = "America/Halifax") # pasting together sunrise 
  df$sunset <- ymd_hm(paste(df$date, df$sunset.time, sep = " "),tz = "America/Halifax") # pasting together sunset times
    return (df)
}

