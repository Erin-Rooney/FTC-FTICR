library(neonUtilities)

# Angela Possinger Script
# July 22 2020


Temp_07_08_2019 = loadByProduct(dpID="DP1.00041.001",
                                site=c("HEAL","BONA","BARR","TOOL"),
                                package="basic",
                                startdate="2019-07",
                                enddate="2019-08",
                                avg=30)

Temp_07_08_2019_5cores = loadByProduct(dpID="DP1.00041.001",
                                       site=c("HEAL","BONA","BARR","TOOL"),
                                       package="basic",
                                       startdate="2019-07",
                                       enddate="2019-08",
                                       nCores=5,
                                       avg=30)
str(Temp_07_08_2019_5cores) 

Data=Temp_07_08_2019_5cores$ST_30_minute
Metadata=Temp_07_08_2019_5cores$variables

save(Temp_07_08_2019_5cores, file="processed/Temp_07_08_2019_5coresER.RData")


# step 2. info about data ----
# list the column names
names(Temp_07_08_2019_5cores)

# structure of the columns
# this tells you if a variable is numeric or categorical,
# how many factors, etc.
str(Temp_07_08_2019_5cores)

# change the format of the columns
Temp_07_08_2019_5cores$soilTempMean = as.character(Temp_07_08_2019_5cores$soilTempMean)

Temp_07_08_2019_5cores$newcolumn = as.character(Temp_07_08_2019_5cores$siteID)

Temp_07_08_2019_5cores$startDateTime = as.character(Temp_07_08_2019_5cores$startDateTime)
Temp_07_08_2019_5cores$endDateTime = as.character(Temp_07_08_2019_5cores$endDateTime)
Temp_07_08_2019_5cores$`soilTempMean` = as.factor(Temp_07_08_2019_5cores$`soilTempMean`)

data_csv$wsoc_mg_L = data_csv$Water.Soluble.Organic.Carbon..mg.L.

# list the first 6 entries of the data table
head(Temp_07_08_2019_5cores)

# find the dimensions of the data table (rows and columns)
dim(Temp_07_08_2019_5cores)

write.csv(Temp_07_08_2019_5cores, "processed/Temp_07_08_2019_5cores.csv")



