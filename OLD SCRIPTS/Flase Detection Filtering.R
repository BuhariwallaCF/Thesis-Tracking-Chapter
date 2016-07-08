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

######### MARTA
files <- dir("raw data/", full.name = TRUE)
det.I <- files[grep("bsb_matched2", files)] ## just 'matched' == previous versions of otn files
det.II <- lapply(det.I, "read.csv", header = TRUE, stringsAsFactors = FALSE, dec = ".") ## will delete the header
matched.df <- do.call("rbind", det.II) ## combine all csv files into one df

## there are differences in the number of detections in the matched detection file (OTN) vs the original df from VUE

matched.df %>% group_by(yearcollected) %>% summarise(length(station))

## need to take out release information & pictou harbour, from df
matched.df <- matched.df[!(matched.df$tagname == "release"|matched.df$station %in% c("ALBERT BRIDGE","MARION BRIDGE", "PICTOU HARBOUR, NS")),]

matched.df <- matched.df[-grep(c("HFX"), matched.df$station),] # this is the same shit marta should have

matched.df %>% group_by(yearcollected) %>% summarise(length(station))

matched.df <- matched.df[-grep(c("-PH"), matched.df$station),]


### join matched.df with mr.df based on id & detection date/time
matched.df$id <- unlist(strsplit(matched.df$tagname, split = "-"))[3*(1:length(matched.df$tagname))]
matched.df$date <- ymd_hms(matched.df$datecollected)
matched.df <- arrange(matched.df, date)

matched.df %>% group_by(yearcollected) %>% summarise(length(station))
mrdf %>% group_by(year) %>% summarise(length(date))

#length(mr.df$date) - length(matched.df$date)
# = 353003

## now find out what detections are missing

# this wont work because of clocks not maching in time. could be an issue with applying clock drift correction
lost.df <- anti_join(mrdf, matched.df, by=c("id","date"))
lost.df2 <- anti_join(matched.df, mrdf, by = c("id", "date"))
lost.df2 <- lost.df2[c(8,9,12,34,35,36)]

## try to get a new file from matched that can be easily looked at
matdf <- matched.df[c(8,9,12,34,35,36)]


## see why there were no detcetions at View(head(matched.df %>% filter(date <= ymd_hms("2014-10-28 18:30:00"), receiver == 113677), 50)),
#     but there were at head(mrdf %>% filter(date <= ymd_hms("2015-01-28 18:30:00"), receiver == 113677))

head(filter(matdf, receiver == 113677))
head(filter(matdf, receiver == 113677))
head(filter(mrdf, receiver == 113677, date >= ymd("2015-01-01")))

tail(filter(matdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 113677))
tail(filter(mrdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 113677))

tail(filter(matdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 112794))
tail(filter(mrdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 112794))

head(filter(matdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 112794))# clock is 7 seconds ahead
head(filter(mrdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 112794)) # clock is 7 seconds behind

mr794 <- filter(mrdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 112794)
ma794 <- filter(matdf, date <= ymd_hms("2015-01-28 18:30:00"), receiver == 112794)

(ggplot(mr794, aes(date, id)) + geom_point())
(ggplot(ma794, aes(date, id)) + geom_point())

mr794 <- filter(mrdf, receiver == 112794)
ma794 <- filter(matdf, receiver == 112794)

### # SORTING SHIT OUT WITH CLOCK ISSUES

trash1 <- arrange(anti_join(mrdf, matdf, by = c("id", "date")), date)
trash2 <- arrange(anti_join(matdf, mrdf, by = c("id", "date")), date)

problems <- trash1 %>% group_by(receiver) %>% summarise(start = min(date), end = max(date))

head(filter(trash1, receiver == 110141))
head(filter(trash2, receiver == 110141))
problems$delay_start[problems$receiver == 110141] <- seconds(1) # mrdf > matched 
tail(filter(trash1, receiver == 110141))
tail(filter(trash2, receiver == 110141))
problems$delay_end[problems$receiver == 110141] <- seconds(1) #mrdf > matched


head(filter(trash1, receiver == 112793))
head(filter(trash2, receiver == 112793))
problems$delay_start[problems$receiver == 112793] <- seconds(56) #mrdf > matched
tail(filter(trash1, receiver == 112793))
tail(filter(trash2, receiver == 112793))
problems$delay_end[problems$receiver == 112793] <- seconds(76) #mrdf > matched
     
head(filter(trash1, receiver == 112794))
head(filter(trash2, receiver == 112794))
problems$delay_start[problems$receiver == 112794] <- seconds(7) # mrdf > matched        
tail(filter(trash1, receiver == 112794))
tail(filter(trash2, receiver == 112794))
problems$delay_end[problems$receiver == 112794] <- seconds(11) # mrdf > matched

####
head(filter(trash1, receiver == 113677))
head(filter(trash2, receiver == 113677))
problems$delay_start[problems$receiver == 113677] <- seconds(1) #mrdf > matched 
tail(filter(trash1, receiver == 113677))
tail(filter(trash2, receiver == 113677))
problems$delay_end[problems$receiver == 113677] <- seconds(1) #mrdf > matched


head(filter(trash1, receiver == 113689))
head(filter(trash2, receiver == 113689))
problems$delay_start[problems$receiver == 113689] <- seconds(1) # mrdf > matched        
tail(filter(trash1, receiver == 113689))
tail(filter(trash2, receiver == 113689))
problems$delay_end[problems$receiver == 113689] <- seconds(1) # mrdf > matched   


head(filter(trash1, receiver == 113691))
head(filter(trash2, receiver == 113691))
problems$delay_start[problems$receiver == 113691] <- seconds(1) # mrdf > matched        
tail(filter(trash1, receiver == 113691))
tail(filter(trash2, receiver == 113691))
problems$delay_end[problems$receiver == 113691] <- seconds(1) # mrdf > matched   


head(filter(trash1, receiver == 113693))
head(filter(trash2, receiver == 113693))
problems$delay_start[problems$receiver == 113693] <- seconds(74) # mrdf > matched        
tail(filter(trash1, receiver == 113693))
tail(filter(trash2, receiver == 113693))
problems$delay_end[problems$receiver == 113693] <- seconds(98) #mrdf > matched 

## add number of detects

num_detects <- trash1 %>% group_by(receiver) %>% summarise(detections = length(ddate))

problems <- full_join(problems, num_detects, by = "receiver")
summarise(problems, sum(detections))
