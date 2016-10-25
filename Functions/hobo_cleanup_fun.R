# 2016-10-20 Colin F Buhariwalla
# HOBO water temp logger 

#purpose: CLEAN UP THE HOBO TEMP DATA TO REMOVE DATAPOINTS DURING RECOVERIES, PRE/POST DEPLOYMENTS

# temp_data = hobo temperature file with names() <- c("date", "temp", "station") # station being the receiver station name
# stn_metadata = OTN combined metadata files (deployment recovery)... 
# stn_of_interest = character of the station number you're interested in 


hobo_cleanup_fun <- function(temp_data, stn_metadata, station_of_interest){ # we input: hobo temp file cleaned up above; station metadata; and the station number we want to work with (character) e.g. "007" 
  {
    stn <- stn_metadata %>% filter(station == station_of_interest) # isolate only the metadata we want
    stn_recovered <- stn %>% transmute(recovered_int = interval(lag(recov_date,1),depl_date))# create an interval of the time hobo was out of the water between deployments; basically, recovery date 1 - deploy date 2; NOTE: row 1 WILL == NA!!!!
    temperature.df <- temp_data %>% filter(date < stn$recov_date[length(stn$recov_date)] & date > stn$depl_date[1])# Remove data points before (depl_date 1) and after (last recov_date) study window -  
    
    ### now let's skip some rope and jump into a loop... here we gooooooo 
    deletion.bin.df <- NULL # space holder
    for(i in 2:length(stn_recovered$recovered_int)){ # need to start at 2 due to the NA at stn_recovered$recovered_int[1] 
      temp <- temperature.df[temperature.df$date %within% stn_recovered$recovered_int[i],] # are there dates that fall within the recovered intervals? 
      deletion.bin.df <- rbind(deletion.bin.df, temp) # bind that shit together
    }
  }
  clean_temp <- anti_join(temperature.df, deletion.bin.df, by = "date") # get rid of the terrible, awful, just monsterous detections from the orginal data file and call it something new and shiny/pretty... I am tired. 
  # remove(deletion.bin.df)
  return(clean_temp)
}
