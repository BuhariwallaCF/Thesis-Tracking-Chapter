#2016-06-19 
# False filtering - C Buhariwalla
# from output of white mihoff filtering tool 

require(lubridate)
require(dplyr)
require(qdap)

df <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2012_suspect_v00.csv", stringsAsFactors = FALSE)
non.suspect.2012 <- df[(df$stn1 == df$stn2 & df$stn2 == df$stn3|df$stn1 == "ALBERT BRIDGE"|df$stn1 == df$stn2|df$stn2==df$stn3),]
df <- anti_join(df, non.suspect.2012)
non.suspect.2012 <- bind_rows(non.suspect.2012, df) ## after manually checking the detections, there is nothing that'd I'd consider suspect

df2 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2013_suspect_v00.csv", stringsAsFactors = FALSE)
non.suspect.2013 <- df2[(df2$stn1 == df2$stn2 & df2$stn2 == df2$stn3|df2$stn1 == "ALBERT BRIDGE"|df2$stn1 == df2$stn2|df2$stn2==df2$stn3),]
df2 <- anti_join(df2, non.suspect.2013)
suspect.2013 <- c("HFX-A69-1303-33160-170125","BSB-A69-1303-33158-335779","BSB-A69-1303-48411-538919")
#non.suspect.2013 <- bind_rows(non.suspect.2013, df2[!df2$suspect_detection %in% suspect.2013,]) 
false.13 <- df2[df2$suspect_detection %in% suspect.2013,]
false.13$prev_interval <- hms(false.13$prev_interval)
false.13$next_interval <- hms(false.13$next_interval)

df3 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2014_suspect_v00.csv", stringsAsFactors = FALSE)
non.suspect.2014 <- df3[(df3$stn1 == df3$stn2 & df3$stn2 == df3$stn3|df3$stn1 == "ALBERT BRIDGE"|df3$stn1 == df3$stn2|df3$stn2==df3$stn3),]
df3 <- anti_join(df3, non.suspect.2014) 
suspect.2014 <- NULL
non.suspect.2014 <- bind_rows(non.suspect.2014, df3)


df4 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2015_suspect_v00.csv", stringsAsFactors = FALSE)
non.suspect.2015 <- df4[(df4$stn1 == df4$stn2 & df4$stn2 == df4$stn3|df4$stn1 == "ALBERT BRIDGE"|df4$stn2 == "ALBERT BRIDGE"|df4$stn1 == df4$stn2|df4$stn2==df4$stn3),]
df4 <- anti_join(df4, non.suspect.2015) 
df4 <- df4[-grep("-PH", df4$stn1),]
df4 <- df4[-grep("-PH", df4$stn3),]
## df4 is clear after this filtering 
non.suspect.2015 <- bind_rows(non.suspect.2015, df4)


### now check for type B error (tag id produced that is not there)

df.1 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2012_suspect_v01.csv", stringsAsFactors = FALSE)
df.1$prev_interval <- hms(df.1$prev_interval)
df.1$next_interval <- hms(df.1$next_interval)
df.1 <- na.omit(df.1[(df.1$prev_interval < seconds(50)|df.1$next_interval < seconds(50)),])


df.2 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2013_suspect_v01.csv", stringsAsFactors = FALSE)
df.2$prev_interval <- hms(df.2$prev_interval)
df.2$next_interval <- hms(df.2$next_interval)
df.2 <- na.omit(df.2[(df.2$prev_interval < seconds(50)|df.2$next_interval < seconds(50)),])

df.3 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2014_suspect_v01.csv", stringsAsFactors = FALSE)
df.3$prev_interval <- hms(df.3$prev_interval)
df.3$next_interval <- hms(df.3$next_interval)
df.3 <- na.omit(df.3[(df.3$prev_interval < seconds(50)|df.3$next_interval < seconds(50)),])


df.4 <- read.csv("~/Desktop/Data/R/Data Files/False Filtering/bsb_matched_detections_2015_suspect_v01.csv", stringsAsFactors = FALSE)
df.4$prev_interval <- hms(df.4$prev_interval)
df.4$next_interval <- hms(df.4$next_interval)
df.4 <- na.omit(df.4[(df.4$prev_interval < seconds(50)|df.4$next_interval < seconds(50)),])
df.4 <- df.4[-grep("-PH", df.4$stn1),] # get rid of Pictou Harbour
df.4 <- df.4[-grep("-PH", df.4$stn3),] # get rid of Pictou Harbour


false_2013 <- bind_rows(df.2, false.13)

bsb_mr_false_detections <- bind_rows(false_2013, df.1, df.3, df.4)
## use antijoin to get rid of the false detections from the matched detection files 

out <- "~/Desktop/Data/R/Data Files/"
#setwd(out)
#dir(out)
#mcsv_r(file.path(bsb_matched_2012.csv, bsb_matched_2013.csv,bsb_matched_2014.csv, bsb_matched_2015.csv)

files <- dir(out, full.name = TRUE)
det.I <- files[grep("bsb_matched", files)]
det.II <- lapply(det.I, "read.csv", header = TRUE, stringsAsFactors = FALSE, dec = ".") ## will delete the header
matched.df <- do.call("rbind", det.II) ## combine all csv files into one df

## there are differences in the number of detections in the matched detection file (OTN) vs the original df from VUE

### First, check for duplicate detections in the mr.df
#trash <- distinct(mr.df)
#length(trash$date) == length(mr.df$date) # == TRUE

## need to take out release information & pictou harbour, from df
matched.df <- matched.df[!(matched.df$tagname == "release"|matched.df$station %in% c("ALBERT BRIDGE","MARION BRIDGE", "PICTOU HARBOUR, NS")),]
matched.df <- matched.df[-grep(c("-PH"), matched.df$station),]
matched.df <- matched.df[-grep(c("HFX"), matched.df$station),]

### join matched.df with mr.df based on id & detection date/time
matched.df$id <- unlist(strsplit(matched.df$tagname, split = "-"))[3*(1:length(matched.df$tagname))]
matched.df$date <- ymd_hms(matched.df$datecollected)


#length(mr.df$date) - length(matched.df$date)
# = 353003

## now find out what detections are missing

mr.df$id <-  as.character(mr.df$id)

lost.df <- anti_join(mr.df, matched.df, by=c("id","date"))
