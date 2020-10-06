#NEON soil temperature data - combining files
#Import - all 40 SOMMOS NEON sites 
#Updated 08-15-20

#NEON
library(neonUtilities)

#Soil temp data###########################################

#lists files in working directory or by path
file.list = list.files(pattern="*.RData")
file.names = substr(file.list, 1, nchar(file.list)-6)
myfiles = lapply(file.list, readRDS)

#Extract ST_30_minute from each, write new csv file to wd or path
extract_dat = function(x,name){
  dat = x$ST_30_minute
  write.csv(dat, paste0("Dat-",name, ".csv", sep="")) 
}

for (i in 1:length(myfiles)){
  extract_dat(x=myfiles[[i]], 
           name=file.names[[i]])
}

#Extract sensor positions from each, write new csv file to wd or path
extract_pos = function(x,name){
  pos = x$sensor_positions_00041
  write.csv(pos, paste0("Pos-",name, ".csv", sep="")) 
}

for (i in 1:length(myfiles)){
  extract_dat(x=myfiles[[i]], 
              name=file.names[[i]])
}
