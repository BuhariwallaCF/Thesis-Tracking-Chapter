# 2016-07-11 
### Resolving detection issues with MAR-TA
## see emails from last two weeks between me and OTN data centre



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
