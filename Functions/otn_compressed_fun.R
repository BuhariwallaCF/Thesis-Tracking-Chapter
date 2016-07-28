#2016-07-28 Colin F Buhariwalla
# function to read in OTN comressed detection files and spit them into something I can use for calculations

otn_compressed_fun <- function(){
  mrdf2015 <- read.csv("data/mrdf2015_compressed_detections_v00.csv", stringsAsFactors = F)
  mrdf2014 <- read.csv("data/mrdf2014_compressed_detections_v00.csv", stringsAsFactors = F)
  mrdf2013 <- read.csv("data/mrdf2013_compressed_detections_v00.csv", stringsAsFactors = F)
  mrdf2012 <- read.csv("data/mrdf2012_compressed_detections_v00.csv", stringsAsFactors = F)
  
  df <- bind_rows(mrdf2012, mrdf2013, mrdf2014, mrdf2015)
  
  
  source("functions/dates_and_times_fun.R")
  df <- dates_and_times_fun(df)
  
  df$id <- sapply(strsplit(df$startunqdetecid, split = "-"), "[",4)
  
  #new_df <- df
  rm(mrdf2012,mrdf2013,mrdf2014,mrdf2015, df)
  return(df) # may not even need this
}
