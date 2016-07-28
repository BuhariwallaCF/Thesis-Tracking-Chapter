### Working with suspect detections from OTNsandbox script
#2014-10-01 

suspect.df <- read.csv("~/Desktop/OTNsandbox/data/tc_lc_vue_db_2014_09_25_utf8_suspect_v00.csv", stringsAsFactors = FALSE) # output file from White-Mihoff detection filtering 

suspect.df$id <- as.character(unlist(strsplit(suspect.df$catalognumber, split = "-"))[3*(1:length(suspect.df$catalognumber))]) # break this into something more manageable (based on previous r code of mine)

trash <- suspect.df # backup

suspect.df <- suspect.df[suspect.df$id %in% tag.id,] ## filter out my tags 

trash <- trash[!trash$id %in% tag.id,] ## TAG ID is from ACOUSTIC CLEANING script 


