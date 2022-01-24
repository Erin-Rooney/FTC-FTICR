#carbon and nitrogen data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

xrd_data = read.csv("processed/xrd_data_fticr.csv") 

# 2. Process data---------------------------------
# remove n and separate sample IDs into multiple columns
# grepl for canopy and slope columns
# LDC and LDA are typos from xrd analysis input, should be LOA and LOC, fixed with recode

xrd_deselect =
  xrd_data %>% 
  select(-c(rwp, feldspar)) 

xrd_data_processed =
  xrd_deselect %>% 
  # mutate(quartz_stdev = stringi::stri_replace_all_fixed(quartz_stdev, "ñ",""),
  #        albite_stdev = stringi::stri_replace_all_fixed(albite_stdev, "ñ",""),
  #        #anorthite_stdev = stringi::stri_replace_all_fixed(anorthite_stdev, "ñ",""),
  #        microcline_stdev = stringi::stri_replace_all_fixed(microcline_stdev, "ñ",""),
  #        chlorite_stdev = stringi::stri_replace_all_fixed(chlorite_stdev, "ñ",""),
  #        mica_stdev = stringi::stri_replace_all_fixed(mica_stdev, "ñ",""),
  #        kaolinite_stdev = stringi::stri_replace_all_fixed(kaolinite_stdev, "ñ",""),
  #        hornblende_stdev = stringi::stri_replace_all_fixed(hornblende_stdev, "ñ","")) %>% 
  separate(sample, sep = " ", into = c("siterep", "depth")) %>% 
  separate(depth, sep = "-", into = c("upperdepth", "lowerdepth")) %>% 
  dplyr::mutate(site = case_when(grepl("H", siterep)~"Healy",
                                 grepl("T", siterep)~"Toolik"),
                rep = case_when(grepl("1", siterep)~"1",
                                grepl("2", siterep)~"2",
                                grepl("3", siterep)~"3")) 
