# FTC-TRANSECT
# comparing GHG fluxes with FTC

# 1. load packages and files -----------------------------------------------------------

library(tidyverse)

ghg_data = read.csv("processed/ghg_depth.csv")
ftc_data = read.csv("processed/FTC_quant_inprocess.csv")

#
# 2. process files -----------------------------------------------------------

## 2.1 calculate mean ftc

ftc_avg = 
  ftc_data %>% 
  filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, year, core) %>% 
  dplyr::mutate(total = sum(def1)) %>% 
  ungroup() %>% 
  
  # now, incorporate the `total` data into `season`
  # spread the `season` columns, and then recombine with `total`
  spread(season, def1) %>% 
  gather(season, def1, total:winter) %>%
  # many NAs were introduced when forcing wide-form. remove all rows containing NAs
  na.omit() %>% 
  
  # now, calculate mean FTC per site/depth/season
  group_by(site, depth_cm, season) %>% 
  dplyr::summarise(ftc = as.integer(mean(def1))) %>% 
  ungroup() %>% 
  
  # create new columns for depth range
  # create bins of 5 cm depth increments
  mutate(depth_bins = cut_width(depth_cm, width = 5, center=2.5)) %>% 
  # now clean up
  # remove brackets of different types
  # I normally use the `stringr` package, but that doesn't like open brackets
  # so I use the `stringi` package for this. You'll have to install it first
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]",""),
         depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[",""),
         depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm))


## 2.2 clean ghg file for compatibility with ftc file

ghg_summary = 
  ghg_data %>% 
#  group_by(site, mid, trmt) %>% 
#  dplyr::summarise(co2_ug_g_oc = round(mean(gain_ug_g_oc),2)) %>% 
#  ungroup() %>% 
  mutate(site = recode(site, "healy" = "HEAL", "tool" = "TOOL"))

## 2.3 now combine the two files such that `mid` lies within the ftc depth range
ghg_ftc = 
  subset(merge(ghg_summary, ftc_avg), depth_start_cm <= mid & depth_stop_cm >= mid) 

#
# 3. plots -------------------------------------------------------------------

theme_set(theme_bw())

ghg_ftc %>% 
  filter(season=="total") %>% 
  ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_grid(season~mid, scales = "free_y")

ghg_ftc %>% 
  filter(season=="total") %>% 
  ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_grid(season~., scales = "free_y")

ghg_ftc %>% 
  filter(season=="total") %>% 
  ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
  geom_point(aes(shape = site), size = 2)+
  geom_smooth(method = "lm", se = F)+
  facet_grid(season~., scales = "free_y")

ghg_ftc %>% 
  filter(season=="total") %>% 
  ggplot(aes(x = ftc, y = gain_ug_g_oc, color = site))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_grid(season~site, scales = "free_x")
