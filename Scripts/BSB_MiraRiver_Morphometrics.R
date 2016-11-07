#2016-10-25 - Colin F. Buhariwalla
#Morphometric tables for thesis
# 1) summary of all fish caught and released in the Mira river - file in raw data
# 2) table of acoustic data, tagging date, id, fl, tl, wt, tag model, life expectency, actual drop dead date - OTN metadata

require(dplyr)
require(ggplot2)
source("functions/dates_and_times_fun.R")

mdf <- read.csv("raw data/CapeBretonBass_morph.csv", stringsAsFactors = F) # morph df
names(mdf) <- tolower(names(mdf))
mdf <- mdf %>% filter(system == "Mira River") %>% mutate(ddate = ymd(paste(year, month, day, sep = "-")))
numerize <- c("fl", "tl", "wt", "uid")
mdf[,numerize] <- lapply(numerize, function(x)as.numeric(mdf[,x])) # need to have the class numeric. NA's for missing values

mdf <- arrange(mdf, ddate, uid)
mdf <- mdf %>% group_by(uid) %>% slice(1)
mdf <- mdf[1:64,] # the last row is a fish I picked up that was dead. 
mdf <- mdf[mdf$id != "5741",] # this tag was a mistake, should not have been deployed. was not used in acoustic analysis... remove it
mdf %>% filter(recap != "y") %>% group_by(year) %>% summarise(n = n(), mean.fl = mean(fl), sd.fl = sd(fl), mean.tl = mean(na.omit(tl)), sd.tl = sd(na.omit(tl)), min.tl = min(na.omit(tl)), 
                                     max.tl = max(na.omit(tl)), mean.wt = mean(na.omit(wt)), sd.wt = sd(na.omit(wt)), min.wt = min(na.omit(wt)), max.wt=max(na.omit(wt)),
                                     n.acoustic = length(which(acoustic_tag == "y"& mort =="n")))


ggplot(mdf, aes(fl)) + geom_histogram(binwidth = 5) + facet_grid(.~acoustic_tag)

### need to add ages to that, see book to figure out which fish still need to be aged (page 105?)


#2) 
acdf <- mdf %>% filter(acoustic_tag == "y", mort == "n") #acoustics df
agedf <- read.csv("~/Desktop/Data/R/Data Files/2013_bass_ages.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", dec = ".", na.strings = "")## need to include ages in there
names(agedf) <- tolower(names(agedf)) # clean it up for a second
acdf <- left_join(acdf, agedf, by = "uid") # left join will keep all the values in 1st df and values in the 2nd df that match the "by" input 

acdf <- acdf %>% ungroup() %>% select(id, fl, tl, wt, age)
acdf$id <- as.numeric(acdf$id)

tag.df <- read.csv("data/tagdf.csv", stringsAsFactors = FALSE)
tag.df <- dates_and_times_fun(tag.df)
tag.df <- full_join(tag.df, acdf, by = "id")

tagdf<- tag.df %>% transmute(Acoustic_tag = id, External_tag = comments, Date_tagged = ddate, Est_battery_end_date = ddate + days(as.numeric(substr(est_tag_life, 1, 3))),
                             last_detection_date = substr(recov_date, 1, 10), FL = fl, TL = tl, Weight = wt, Age = age.y) # age.y since the otn metadata has a column for age... 
              
tagdf <- arrange(tagdf, Date_tagged)


###### test for normality
x <- mdf %>% ungroup() %>% filter(site == "Albert Bridge", acoustic_tag == "y", mort != "y") %>% select(fl)
y <- mdf %>% ungroup() %>% filter(site == "Albert Bridge", acoustic_tag == "n", mort != "y") %>% select(fl)
z <- mdf %>% ungroup() %>% filter(site == "Albert Bridge") %>% select(fl)

## at alpha level 0.05
shapiro.test(y$fl) # = p 0.016 < alpha thus reject the null hypothesis of a normal distrib untagged fish
shapiro.test(x$fl) # = p 0.021 < alpha thus reject the null hypothesis of a normal distrib # taggd fish 

var(y$fl) # 107.83 unqeual var
var(x$fl) # 224.74 unequal var. 

# need to go to man whitney u test? no this assumes normality. Standard one sided t-test. 
#we assume that the parent population is from a normal distribution, which, in the absence of fishing pressure, is an OK assumption
# however, there may be variability in year class success which would cause bimodal distribs. Sampling method also induces bias. 

# of fish that were processed and released at albert bridge: 
# H0: There is no difference in mean size of tagged fish is <= mean size of untagged fish. 
# Ha: Mean size of tagged fish is greater than untagged fish
t.test(x$fl, y$fl, alternative = "greater") # welch correction for unequal variances; 

#data:  x$fl and y$fl
#t = 4.6787, df = 53.443, p-value = 1.004e-05
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  10.07563      Inf
#sample estimates:
#  mean of x mean of y 
#65.90323  50.21481 

# since the t = 4.679 and t(0.5)(53) = 1.674, reject H0. 
