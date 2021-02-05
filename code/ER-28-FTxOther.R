#FTC transect FT x other factors
#GHG CSV2 code 
#Erin C Rooney
#2 25 2021

# extract organic horizons
# connect depths to FT horizons

#KP Code Modified below

# 1. load packages and files -----------------------------------------------------------

library(tidyverse)
library(soilpalettes)

# ghg_data = read.csv("processed/ghg_depth.csv")
ftc_data = read.csv("processed/FTC_quant_inprocess.csv")
# neon_barr_csv = read.csv("processed/neon_barr_biogeochem.csv")
# neon_heal_csv = read.csv("processed/neon_heal_biogeochem.csv")
# neon_tool_csv = read.csv("processed/neon_tool_biogeochem.csv")
# neon_bona_csv = read.csv("processed/neon_bona_biogeochem.csv")

#
# 2. process files -----------------------------------------------------------

## 2.1 calculate mean ftc

ftc_avg = 
  ftc_data %>% 
  filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, year, core, depth_cm) %>% 
  #dplyr::mutate(total = sum(def1)) %>% 
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


## 2.2 combine and clean site file for compatibility with ftc file

# all_site = neon_barr_csv %>% 
#   bind_rows(neon_bona_csv, neon_heal_csv, neon_tool_csv) 
# 
# 
#  om_thickness = 
#    all_site %>% 
#    rename(site = siteID)
  #  group_by(site, mid, trmt) %>% 
  #  dplyr::summarise(co2_ug_g_oc = round(mean(gain_ug_g_oc),2)) %>% 
  #  ungroup() %>% 
  # mutate(site = recode(site, "healy" = "HEAL", "tool" = "TOOL")) 

# ghg_summary = ghg_summary %>% 
#   mutate(trmt = factor(trmt, levels = c("ftc", "control")))
# 
# ghg_summary = ghg_summary %>% 
#   mutate(day = factor(day, levels = c("day1", "day4", "day7", "day14")))

# ## 2.3 now combine the two files such that `mid` lies within the ftc depth range
# om_ftc = 
#   subset(merge(om_thickness, ftc_avg), depth_start_cm <= biogeoCenterDepth & depth_stop_cm >= biogeoCenterDepth) 

#
# 3. plots -------------------------------------------------------------------

# theme_set(theme_er())
# 
# ghg_ftc %>% 
#   filter(season=="total") %>% 
#   ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
#   geom_point()+
#   geom_smooth(method = "lm", se = F)+
#   facet_grid(season~day)
# #facet_grid(season~mid, scales = "free_y")
# 
# 
# ghg_ftc %>% 
#   filter(!season=="total") %>% 
#   #filter(day=="day1") %>% 
#   filter(site=="HEAL") %>% 
#   ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
#   geom_point()+
#   geom_smooth(method = "lm", se = F)+
#   theme_er()+
#   ggtitle("Healy")+
#   facet_wrap(day~.)+
#   scale_fill_manual(values = soil_palette("redox", 2)) +
#   scale_color_manual(values = soil_palette("redox", 2)) 
# #facet_grid(season~mid, scales = "free_y")
# 
# ghg_ftc %>% 
#   filter(season=="total") %>% 
#   ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
#   geom_point()+
#   geom_smooth(method = "lm", se = F)+
#   facet_grid(season~., scales = "free_y")
# 
# ghg_ftc %>% 
#   filter(season=="total") %>% 
#   ggplot(aes(x = ftc, y = gain_ug_g_oc, color = trmt))+
#   geom_point(aes(shape = site), size = 2)+
#   geom_smooth(method = "lm", se = F)+
#   facet_grid(season~., scales = "free_y")
# 
# ghg_ftc %>% 
#   filter(season=="total") %>% 
#   ggplot(aes(x = ftc, y = gain_ug_g_oc, color = site))+
#   geom_point()+
#   geom_smooth(method = "lm", se = F)+
#   facet_grid(season~site, scales = "free_x")

#ggplot geom_rect--------------------------------------------------

# 3. SET GGPLOT THEME -----------------------------------------------------

theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}


ftc_avg %>% 
  ggplot() +
  geom_rect(ymin = 'depth_start_cm', ymax = 'depth_stop_cm', xmin = as.numeric('site')-0.2, xmax= as.numeric('site')+0.2, fill = 'ftc')+
  facet_grid(.~season)+
  scale_y_reverse()+
  theme_kp()

