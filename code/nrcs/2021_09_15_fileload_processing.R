#E Rooney
#NRCS Data
#Sept 15 2021

#pre steps
#Recreate file structure: processed/nrcs and raw/nrcs

# Step 0. Packages

source("code/nrcs/0-packages.R")


# Step 1. load files ----

Y2014 = read_excel("raw/nrcs/Toolik_14ave.xlsx")
Y2015 = read_excel("raw/nrcs/Toolik_15ave.xlsx")
Y2016 = read_excel("raw/nrcs/Toolik_16.xlsx") %>% dplyr::rename('HOUR' = 'N')
Y2017 = read_excel("raw/nrcs/Toolik_17.xlsx") %>% dplyr::rename('HOUR' = 'N')
Y2018 = read_excel("raw/nrcs/Toolik_18.xlsx")



# Step 2. process file ----
#turning this into a function

soiltemp_processing = function(data){

  data %>%
  #change column names, units are in celsius and cm
  dplyr::rename('vegtemp_7' = 'MRC...15',
                'soiltemp_0.6' = 'MRC...16',
                'soiltemp_8.7' = 'MRC...17',
                'soiltemp_16' = 'MRC...18',
                'soiltemp_23.6' = 'MRC...19',
                'soiltemp_31.2' = 'MRC...20',
                'soiltemp_38.7' = 'MRC...21',
                'soiltemp_46.3' = 'MRC...22',
                'soiltemp_61.6' = 'MRC...23',
                'soiltemp_76.8' = 'MRC...24',
                'soiltemp_97.8' = 'MRC...25',
                ) %>% 
  select(ID, YEAR, DAY, HOUR, TIME, DATE, 'vegtemp_7', 'soiltemp_0.6',
         'soiltemp_8.7', 'soiltemp_16', 'soiltemp_23.6', 'soiltemp_31.2', 
         'soiltemp_38.7', 'soiltemp_46.3', 'soiltemp_61.6', 'soiltemp_76.8', 
         'soiltemp_97.8') %>% 
  #get rid of first two rows, they are garbage. Fortunately they have NA's
  #so we can omit them
  na.omit()

}

Y2014_processed = soiltemp_processing(Y2014)
Y2015_processed = soiltemp_processing(Y2015)
Y2016_processed = soiltemp_processing(Y2016)
Y2017_processed = soiltemp_processing(Y2017)
Y2018_processed = soiltemp_processing(Y2018)

#write csv and put it in the processed/nrcs folder


# Step 3. export/write csv file ----

write.csv(Y2014_processed, "processed/nrcs/toolik2014.csv")
write.csv(Y2015_processed, "processed/nrcs/toolik2015.csv")
write.csv(Y2016_processed, "processed/nrcs/toolik2016.csv")
write.csv(Y2017_processed, "processed/nrcs/toolik2017.csv")
write.csv(Y2018_processed, "processed/nrcs/toolik2018.csv")


# Step 4. bind csv files so all years are in one document
# should be soil temp only

filePaths <- list.files(path = "processed/nrcs",pattern = "*.csv", full.names = TRUE)

soil_temp <- do.call(rbind, lapply(filePaths, function(path) {
  df <- read_csv(path)
  df[["source"]] <- rep(path, nrow(df))
  df}))

#

# Step 5. 

soil_temp = read.csv("processed/nrcs/soil_temp.csv")

soil_temp_processed = 
  soil_temp %>% 
  select(-c(X, X1, source)) %>%
  dplyr::rename('0.6' = 'soiltemp_0.6',
                '8.7'= 'soiltemp_8.7',
                '16' = 'soiltemp_16',
                '23.6' = 'soiltemp_23.6',
                '31.2' = 'soiltemp_31.2',
                '38.7' = 'soiltemp_38.7',
                '46.3' = 'soiltemp_46.3',
                '61.6' = 'soiltemp_61.6',
                '76.8' = 'soiltemp_76.8',
                '97.8' = 'soiltemp_97.8',
  ) %>% 
  pivot_longer(-c(ID, YEAR, DAY, HOUR, TIME, DATE), 
                  names_to = "depth", values_to = "temp") 
  
  

# Step 6. export final csv/R.data for FTCQuant analysis
#couldn't get rdata to save

save(soil_temp_processed, file = "soil_temp.RData")
write.csv(soil_temp_processed, "processed/nrcs/soil_temp.csv")





