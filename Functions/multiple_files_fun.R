# 2016-06-28 Colin F Buhariwalla
# Read in a bunch of files into one CSV
#     - adapted from Phil Taylor's 2012 'R' course at Acadia U

multiple_files_fun <- function(file.path.string, common.string.in.file.name, output.df.name){
  
files <- dir(file.path.string, full.name = TRUE)
det.I <- files[grep(common.string.in.file.name, files)]
det.II <- lapply(det.I, "read.csv", header = TRUE, stringsAsFactors = FALSE, dec = ".") ## will delete the header
output.df.name <- do.call("rbind", det.II) ## combine all csv files into one df

return(output.df.name)

rm(det.I, det.II, files)

}