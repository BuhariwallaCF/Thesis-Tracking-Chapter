#2016-08-12 Colin F Buhariwalla
# Read in Multiple CSV files 

multiple_csv_import_bind_fun <- function(input.folder.path, key.in.filename){ 
  #input.folder = "string"
  #name.new.df = df
  #key.in.filename = "string"
  require(readr)
  require(dplyr)
  allfiles <- dir(input.folder.path, full.name = T) # get all file names in the directory you're pointing at
  requiredfiles <- allfiles[grep(key.in.filename, allfiles)] # 
  df <- lapply(requiredfiles, function(x) read.csv(x, stringsAsFactors = F, colClasses = "character")) %>% bind_rows() # This and The below do the same thing
return(df)
  }