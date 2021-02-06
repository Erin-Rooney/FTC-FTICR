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
library(PNWColors)
library(nord)

# ghg_data = read.csv("processed/ghg_depth.csv")
ftc_data = read.csv("processed/FTC_quant_inprocess.csv")
ftc_full = read.csv("processed/final_dat2.csv")
neon_barr_csv = read.csv("processed/neon_barr_biogeochem.csv")
neon_heal_csv = read.csv("processed/neon_heal_biogeochem.csv")
neon_tool_csv = read.csv("processed/neon_tool_biogeochem.csv")
neon_bona_csv = read.csv("processed/neon_bona_biogeochem.csv")
site_data = read.csv("processed/SOMMOS_Site_11-25-19.csv")
climate_data = read.csv("processed/SOMMOS_SoilCoreDB - Climate_1961_1990.csv")

#
# 2. process files -----------------------------------------------------------


site_data %>% 
 filter(site %in% c("BONA", "BARR", "HEAL", "TOOL")) %>% 
  mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>% 
  ggplot()+
  geom_point(aes(x=site, y=PPT03, color= site), size =9)+
  theme_kp()+
  scale_color_nord("afternoon_prarie", 4)+
  labs(y = "precipitation")

climate_data %>% 
  filter(site %in% c("BONA", "BARR", "HEAL", "TOOL")) %>% 
  mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>% 
  ggplot()+
  geom_point(aes(x=site, y=Tmax01, color= site), size =9)+
  theme_kp()+
  scale_color_nord("afternoon_prarie", 4)+
  labs(y = "Temperature, C")




## 2.1 calculate mean ftc

ftc_avg = 
  ftc_full %>% 
  filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, year, core, depth_cm) %>% 
  dplyr::mutate(total = sum(Def1)) %>% 
  ungroup() %>% 
  
  # now, incorporate the `total` data into `season`
  # spread the `season` columns, and then recombine with `total`
  spread(season, Def1) %>% 
  gather(season, Def1, total:winter) %>%
  # many NAs were introduced when forcing wide-form. remove all rows containing NAs
  na.omit() %>% 
  
  # now, calculate mean FTC per site/depth/season
  group_by(site, depth_cm, season) %>% 
  dplyr::summarise(ftc = as.integer(mean(Def1))) %>% 
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


# 2.2 combine and clean site file for compatibility with ftc file

 all_site = neon_barr_csv %>%
   bind_rows(neon_bona_csv, neon_heal_csv, neon_tool_csv) 
   
   
om = all_site %>%
   select(siteID, plotID, horizonName, biogeoTopDepth, biogeoBottomDepth, biogeoCenterDepth, ctonRatio) %>% 
   mutate(major = case_when(grepl("O", horizonName)~"O"),
          sub = case_when(grepl("e", horizonName)~"e",
                            grepl("i", horizonName)~"i",
                            grepl("a", horizonName)~"a",
                            ))
 


  om_thickness =
    om %>%
    rename(site = siteID, mid = biogeoCenterDepth) %>% 
    group_by(plotID, mid) 
    # ungroup() %>%
   #mutate(site = recode(site, "healy" = "HEAL", "tool" = "TOOL"))
 # 
 # ghg_summary = ghg_summary %>%
 #   mutate(trmt = factor(trmt, levels = c("ftc", "control")))
 # 
 # ghg_summary = ghg_summary %>%
 #   mutate(day = factor(day, levels = c("day1", "day4", "day7", "day14")))

 ## 2.3 now combine the two files such that `mid` lies within the ftc depth range
 om_ftc =
   subset(merge(om_thickness, ftc_avg), depth_start_cm <= mid & depth_stop_cm >= mid)

#
# 3. plots -------------------------------------------------------------------

theme_set(theme_er())
 
om_ftc = om_ftc %>% 
   mutate(depth = depth_stop_cm - depth_start_cm)

om_ftc %>%
  filter(major=="O") %>%
  mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>% 
  ggplot(aes(x = site, y = depth_cm, fill = site))+
  geom_bar(position = "dodge", stat= "identity")+
  scale_y_reverse()+
  theme_kp()+
  scale_fill_nord("afternoon_prarie")


om_ftc %>%
  filter(major=="O") %>%
  mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>% 
  ggplot(aes(x = site, y = depth_cm, fill = site))+
  geom_point()+
  geom_boxplot(alpha = 0.8)+
  scale_y_reverse()+
  theme_kp()+
  scale_fill_nord("afternoon_prarie", 4)+
  labs(y = "depth, cm")


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
    theme(legend.position = "right",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="white",size=1.5, fill = NA),
          
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


ftc_avg_depth = ftc_avg %>% 
  mutate(depth = depth_stop_cm - depth_start_cm)

ftc_avg_depth %>% 
  filter(!season %in% "total") %>% 
  mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>% 
  ggplot(aes(y = depth, x = site, fill = ftc))+
  geom_bar(position = "stack", stat= "identity")+
  facet_grid(.~season)+
  scale_y_reverse()+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_kp()

