#carbon and nitrogen data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

cn_data = read.csv("processed/emsl_cn.csv") 

# 2. Process data---------------------------------
# remove n and separate sample IDs into multiple columns
# grepl for site and material columns

cn_data_processed =
  cn_data %>% 
  separate(name, sep = " ", into = c("siterep", "depth")) %>% 
  separate(depth, sep = "-", into = c("upperdepth", "lowerdepth")) %>% 
  dplyr::mutate(rep = case_when(grepl("1", siterep)~"1",
                                grepl("2", siterep)~"2",
                                grepl("3", siterep)~"3")) %>% 
  select(-c(siterep))

cn_data_summarized = 
  cn_data_processed %>% 
  group_by(site, material) %>% 
  dplyr::summarize(n_mean = round(mean(n_perc), 2),
                   n_se = round(sd(n_perc)/sqrt(n()),2),
                   c_mean = round(mean(c_perc), 2),
                   c_se = round(sd(c_perc)/sqrt(n()),2)) %>%
  # mutate(n_mean = n_mean*100,
  #        n_se = n_se*100,
  #        c_mean = c_mean*100,
  #        c_se = c_se*100) %>% 
  mutate(summary_n = paste(n_mean, "\u00b1", n_se)) %>% 
  mutate(summary_c = paste(c_mean, "\u00b1", c_se)) %>% 
  na.omit() %>% 
  select(-c(n_mean, n_se, c_mean, c_se))

cn_data_summarized %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(cn_data_summarized, "output/cn_data_summarized.csv", row.names = FALSE)



