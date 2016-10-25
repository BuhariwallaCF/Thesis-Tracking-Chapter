# 2016-06-27 Colin F Buhariwalla
# Acoustic Tag Metadata Cleaning function 

# input file must be that of an OTN metadata file submitted to the OTN 

tag_cleaning_fun <- function(data){
  require(lubridate)
  data <- data[1:length(data[grep("VEMCO",data$TAG_MANUFACTURER)]),] ## remove the NA's introduced during the import
  data <- data[,-c(33:48)] # see below for detailed names of columns we're deleting 
  
  # set up date and time within the data dataframe
  data$date <- gsub("T", " ", data$UTC_RELEASE_DATE_TIME,) # this allows you to convert date/time md to posix
  data$ddate <-  ymd(substr(data$date, 1, 10))
  data$date <- ymd_hms(data$date, tz = "UTC")
  #data$id <- as.factor(data$TAG_ID_CODE) # don't know why this has to be factor (I think it's for plotting)
  data$id <- data$TAG_ID_CODE
  
  names(data) <- tolower(names(data))
  return(data)
}


#[1] "ANIMAL_ID"                                        "TAG_TYPE"                                         "TAG_MANUFACTURER"                                
#[4] "TAG_MODEL"                                        "TAG_SERIAL_NUMBER"                                "TAG_ID_CODE"                                     
#[7] "TAG_CODE_SPACE"                                   "TAG_IMPLANT_TYPE"                                 "TAG_ACTIVATION_DATE"                             
#[10] "EST_TAG_LIFE"                                     "TAGGER"                                           "TAG_OWNER_PI"                                    
#[13] "TAG_OWNER_ORGANIZATION"                           "COMMON_NAME_E"                                    "SCIENTIFIC_NAME"                                 
#[16] "CAPTURE_LOCATION"                                 "CAPTURE_LATITUDE"                                 "CAPTURE_LONGITUDE"                               
#[19] "WILD_OR_HATCHERY"                                 "STOCK"                                            "LENGTH..cm."                                     
#[22] "WEIGHT..kg."                                      "LENGTH_TYPE"                                      "AGE"                                             
#[25] "SEX"                                              "DNA_SAMPLE_TAKEN"                                 "TREATMENT_TYPE"                                  
#[28] "RELEASE_GROUP"                                    "RELEASE_LOCATION"                                 "RELEASE_LATITUDE"                                
#[31] "RELEASE_LONGITUDE"                                "UTC_RELEASE_DATE_TIME"                            "CAPTURE_DEPTH..m."                               
#[34] "TEMPERATURE_CHANGE..degrees.C."                   "HOLDING_TEMPERATURE..degrees.C."                  "SURGERY_LOCATION"                                
#[37] "DATE_OF_SURGERY"                                  "SURGERY_LATITUDE"                                 "SURGERY_LONGITUDE"                               
#[40] "SEDATIVE"                                         "SEDATIVE_CONCENTRATION..ppm."                     "ANAESTHETIC"                                     
#[43] "BUFFER"                                           "ANAESTHETIC_CONCENTRATION..ppm."                  "BUFFER_CONCENTRATION_IN_ANAESTHETIC..ppm."       
#[46] "ANAESTHETIC_CONCENTRATION_IN_RECIRCULATION..ppm." "BUFFER_CONCENTRATION_IN_RECIRCULATION..ppm."      "DISSOLVED_OXYGEN..ppm."                          
#[49] "COMMENTS"