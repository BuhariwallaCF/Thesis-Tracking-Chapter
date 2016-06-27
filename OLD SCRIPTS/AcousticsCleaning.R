#### Cleaning script for MR datafiles
## 2014-09-25 

#### Code from Overwintering Script ####
#### reading in and cleaning up data frame ####
#df.1 <- read.csv("~/Desktop/Data/Vemco/VUE/Database export/2014-07-16/VueExport_depthTags_2014-07-16.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")
#df.2 <- read.csv("~/Desktop/Data/Vemco/VUE/Database export/2014-07-16/VueExport_longLifeTags_2014-07-16.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")

# newer iterations

df <- read.csv("~/Desktop/Data/vm - shared/VUE/Exports/VueExport_timeCorrectedDB_2014-09-25.csv", stringsAsFactors = FALSE, header = TRUE, dec = ".")


df <- df[-c(4,5,9,10)]

names(df) <-c("date","vr2","tag", "depth", "unit", "station")

df$id <- as.character(unlist(strsplit(df$tag, split = "-"))[3*(1:length(df$tag))]) # seperate tags into id's ### MAYBE REMOVE THE FACTOR LEVEL HERE AND ADD IT SOMEWHERE ELSE.. WHEN I DELETE ALL THE EXTRANEOUS TAGS
df$vr2 <- as.character(unlist(strsplit(df$vr2, split = "-"))[2*(1:length(df$vr2))]) # seperate vr2s to sn

#clean up workspace 
#remove(df.df.a, df.df.b, files, files2, vr2, vr2.01, vr2.01.b, vr2.b, df.df)

#create backup
df.base <- df

tags <- read.csv("~/Desktop/Data/OTN/Cape Breton/Metadata/Tagging/combined/BSB_tagging.csv", stringsAsFactors = FALSE, skip = 4, header = TRUE, sep = ",", dec = "." )# create a list of tags from a csv file
tag.id <- as.list(as.character(na.omit(tags$TAG_ID_CODE)))
remove(tags)

df <- df[df$id %in% tag.id,] # subset the df df based on my tag id's ## eliminated 2144 detections
# difference = 23917 detections that aren't my tags -> length(df.base$date)-length(df$date)


# sensor conversion == y= mx + b == .43970x -1.7587
df$depth <- 0.43970*df$depth-1.7587 #need to set to 1 decimal place? 
df$unit <- "m"

df$date <- ymd_hms(df$date, tz = "UTC")
df$day <- day(df$date)
df$month <- month(df$date)
df$year <- year(df$date)

df.base <- df

#### Renaming the stations ####
df$station <- ifelse(df$vr2 == "110142", "001",
                     ifelse(df$vr2 == "112793" & df$date >= ymd_hms("2014-05-07 23:43:46"), "001",
                            ifelse(df$vr2 == "112788", "002", 
                                   ifelse(df$vr2 == "113681" & df$date <= ymd_hms("2013-07-31 00:00:00"), "003",
                                          ifelse(df$vr2 == "113681" & df$date >= ymd_hms("2013-08-01 23:20:00"), "004",
                                                 ifelse(df$vr2 == "112789" & df$date <= "2013-07-23 18:15:00", "004",
                                                        ifelse(df$vr2 == "113678" & df$date < ymd_hms("2014-05-08 19:30:00"), "005",
                                                               ifelse(df$vr2 == "105998" & df$date < ymd_hms("2013-11-09 20:52:00"), "006",
                                                                      ifelse(df$vr2 == "112794", "007",
                                                                             ifelse(df$vr2 == "112790", "008",
                                                                                    ifelse(df$vr2 == "112793" & df$date < ymd_hms("2014-05-07 23:40:00"), "009",
                                                                                           ifelse(df$vr2 == "112792" & df$date < ymd_hms("2014-05-08 20:00:00"), "010",
                                                                                                  ifelse(df$vr2 == "113682", "011",
                                                                                                         ifelse(df$vr2 == "112791" & df$date < ymd_hms("2013-05-19 11:10:00"), "011.5",
                                                                                                                ifelse(df$vr2 == "112791" & df$date > ymd_hms("2013-06-12 20:57:00"), "011.5",
                                                                                                                       ifelse(df$vr2 == "113684" & df$date < ymd_hms("2014-05-08 20:30:00"), "012",
                                                                                                                              ifelse(df$vr2 == "105998" & df$date >= ymd_hms("2013-11-09 20:52:00"), "011.5",
                                                                                                                                     ifelse(df$vr2 == "113693", "013",
                                                                                                                                            ifelse(df$vr2 == "105999" & df$date <= ymd_hms("2013-07-23 18:03:00"), "013.5",
                                                                                                                                                   ifelse(df$vr2 == "105999" & df$date > ymd_hms("2013-07-23 18:03:00"), "NA",
                                                                                                                                                          ifelse(df$vr2 == "113690", "014",
                                                                                                                                                                 ifelse(df$vr2 == "113678" & df$date > ymd_hms("2014-05-08 19:30:00"), "015",
                                                                                                                                                                        ifelse(df$vr2 == "112792" & df$date > ymd_hms("2014-05-08 20:00:00"), "016",
                                                                                                                                                                               ifelse(df$vr2 == "113684" & df$date > ymd_hms("2014-05-08 20:30:00"), "017",
                                                                                                                                                                                      ifelse(df$vr2 == "112791" & df$date >= ymd_hms("2013-05-19 11:20:00") & df$date <= ymd_hms("2013-06-12 20:50:00"), "018",
                                                                                                                                                                                             ifelse(df$vr2 == "105998" & ymd_hms("2013-06-01 03:50:00") >= df$date & df$date < ymd_hms("2013-06-12 21:20:00"), "019", "ERROR"
                                                                                                                                                                                             ))))))))))))))))))))))))))                  


df <- df[!df$station == "NA",] ## this removes the range test detections out of the mix (used tag 33162 during range test)

df <- arrange(df, date)

##### put in OTN column names for filtering purposes #####

df$unqdetecid <- seq(1:length(df$date))
names(df)[1] <- "datecollected" # from date to date collected
names(df)[3] <- "catalognumber"

df$datecollected <- as.character(df$datecollected)

write.csv(df, file = "~/Desktop/OTNsandbox/Data/TC_LC_VUE_DB_2014-09-25.csv", row.names = FALSE) # time corrected, lightly cleaned, vue database

a <- "~/Desktop/Data/R/Workspace"
b <- "~/Desktop/Data/R/acoustics"

setwd(a)
#### suspect detection ####
# I need to identify suspect detections: 








#TRASH FROM OVERWINTERING trying to get it to work


leave.mod <- function(){
  last.date <- function(x){tail(x,1)} # function to give me the last value of a vector/df 
  first.date <- function(x){head(x, 1)}
  
  df <- df[df$id == id,]
  
  descent <- df[df$date >= ymd("2014-01-01") & df$station %in% outside.array,]
  
  
  first.out <- first.date(descent)
  
  last.ow <- last.date(df[which(df$date < first.out$date & df$date > ymd("2014-01-01")),]) # last overwintering date -- defined by detection on outside array
  
  last.ow$albert.first <- descent$date[match(("007"), descent$station)]
  
  last.ow$mig.dur <- as.duration(last.ow$albert.first - last.ow$date)
  
  last.ow$fst.out.s <- first.out$station
  last.ow$fst.out.d <- first.out$date
  
  return(last.ow)
}
