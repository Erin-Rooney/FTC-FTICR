# Erin Rooney
# Adapted from KP
# July 23 2020
# GHG FTC C12 and C13 data

# load data---------------------------------
#ghg_csv = read.csv("processed/ghg_ftc.csv")
ghg_csv2 = read.csv("processed/ghg_depth.csv")
ftc_dat = read.csv("processed/FTC_quant_inprocess.csv")
ftc_fulldat = read.csv("processed/final_dat2.csv")
sommos_oc = read.csv("processed/oc_sommos_neonoc.csv")
probe_loc = read.csv("processed/Probe Locations.csv")

# set data frames-----------------------------

library(dplyr)
library(ggplot2)
library(maps)
require(maps)
require(viridis)
library(tidyr)
str(ghg_csv2)
str(ftc_dat)
str(ftc_fulldat)
str(sommos_oc)
str(probe_loc)
levels(as.factor(ghg_csv2$trmt))
levels(as.factor(ftc_fulldat$site)) 
ftc_fulldat = ftc_fulldat %>% 
      mutate(site = factor (site, levels = c("HEAL", "BONA", "BARR", "TOOL")))
      
      
#mutate(TRT = factor(TRT, levels = c("CON", "FTC")))


# ggplot set up-----------------------------------
theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
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

library(ggplot2)
library(soilpalettes)
library(soilDB)
library(aqp)
library(sharpshootR)
library(sp)
library(ggridges)

# gain in C ggplot----------------------------------------
ggplot(ghg_csv2, aes(x=trmt, y=gain_ug_g_oc, fill=day)) + geom_boxplot() 


ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + geom_boxplot()

ggplot(ghg_csv2, aes(x=day, y=gain_ug_g_oc, fill=site)) + geom_boxplot()

ggplot(ghg_csv, aes(x=day, y=gain_ug_g_oc, fill=site)) + geom_boxplot()


# FT quant ggplots---------------------------------------

#boxplots
ggplot(ftc_dat, aes(x = def1, y = depth_cm, fill = site)) + geom_boxplot() + theme_er() +
  scale_fill_manual (values = soil_palette("gley", 2)) + facet_grid(season~.)

#dotplots all failed
ggplot() + geom_dotplot(data = ftc_dat, aes(y = depth_m, x = def1, color = site)) + theme_er() +
  scale_color_manual (values = soil_palette("gley", 2)) + facet_wrap(season~.)

ggplot(ftc_dat, aes(x=def1, y=depth_cm, fill=site))

#ridge and grid plots
ggplot(ftc_dat, aes(x = def1, y = depth_cm, color = season)) +
stat_bin2d( mapping = NULL, data = NULL, geom = "tile", position = "identity" + bins = 30 + binwidth = NULL + drop = TRUE)

ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + facet_grid(season~.)

ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + scale_x_continuous(limits = c(-5, 30)) + facet_grid(season~.)

ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + scale_x_continuous(limits = c(-5, 30)) + facet_grid(season~.)

ggplot(ftc_dat, aes(x = depth_cm, y = site, height = def1)) +
  geom_density_ridges(stat = "identity") + theme_er() + facet_grid(season~.)

#raster plots----------------------------------------------

ggplot(ftc_dat, aes(x = depth_cm, y = site, fill = def1)) +
  geom_raster(hjust = 0, vjust = 0) + theme_er() + facet_grid(season~.)

# bubble plot with depth on y axis---------------------------------
ftc_dat %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<70) %>%
  ggplot(aes(y = depth_cm, x = site, size = def1, color = def1))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse()+
  # scale_size_continuous()+
  scale_color_gradient(low = "blue", high = "red")+
  ggtitle("Freeze Thaw Cycle Frequency") +
  theme_er() +
  facet_grid(~season)

ftc_dat %>% 
  filter(duration==24 & mag.vec==1.5) %>% 
  ggplot(aes(y = depth_cm, x = site, size = def1, color = def1))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.3))+
  scale_y_reverse()+
  # scale_size_continuous()+
  scale_color_gradient(low = "blue", high = "red")+
  theme_er() +
  facet_grid(~season)

ftc_fulldat %>% 
  filter(duration==24 & mag.vec==1.5) %>% 
  ggplot(aes(y = depth_cm, x = site, size = Def1, color = Def1))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse() +
  ggtitle("Freeze Thaw Gradient Across Seasons") +
  scale_size_continuous(1) +
  scale_color_gradient(low = "blue", high = "red")+
  theme_er() +
  facet_grid(~season)


# bubble plot with depth on y axis--------------------------------------
ghg_csv2 %>% 
  filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = site, size = gain_ug_g_oc, color = gain_ug_g_oc))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse() +
  coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_gradient(low = "blue", high = "yellow")+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(~trmt)

sommos_oc %>% 
  filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = site, size = OC.g100g, color = OC.g100g))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse() +
  coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_gradient(low = "purple", high = "yellow")+
  ggtitle("Organic Carbon Content, g per 100g") +
  theme_er() 

# Map plots---------------------------------------------------------------

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("ggspatial")
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

mainstates <- map_data("state")

ggplot(data = mainstates) +
  geom_sf() 

ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )

usa <- subset(world, admin == "United States of America")
(mainland <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 
                                                                       730000)))
data(map.states)

ggplot(map.states, aes(long, lat,group=group)) + geom_polygon()




world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

ggplot(data = world) +
  geom_sf()

usa <- subset(world, admin == "United States of America")
(mainland <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 
                                                                       730000)))

(alaska <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 
                                                                       2500000), expand = FALSE, datum = NA))


# heatmap--------------------------------------------------------------
ftc_dat %>% 
  filter(depth_cm<100 ) %>% 
  ggplot(aes(y = depth_cm, x = site, fill = def1))+
  geom_tile()+
  scale_y_reverse()+
  coord_fixed(ratio=1/2)




ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


cars <- ggplot(ftc_dat, aes(depth_cm, factor(site)))

cars + stat_bin2d(aes(fill= after_stat(count)), binwidth = c(3,1)) + theme_er() + facet_grid(season~.)

# aov-------------------------------------------

ghg_aov1 = aov(data = ghg_csv2, gain_ug_g_oc ~ site)
summary(ghg_aov1)


ghg_aov2 = aov(data = ghg_csv2, gain_ug_g_oc ~ day)
summary(ghg_aov2)


ghg_aov3 = aov(data = ghg_csv2, gain_ug_g_oc ~ site*day)
summary(ghg_aov3)

# C/g soil ggplot-------------------------------------


ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("redox", 4)) 

###

ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("redox", 4)) 



# Old failed ggplots----------------------------------------

ggplot(ghg_csv2, aes(x = day, y = gain_ug_g_oc, color = day)) +
  geom_dotplot() +
  xlab("Day") +
  ylab("CO2 AVG") +
  ggtitle("12C")




a = ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot() 

a + theme_er + scale_fill_manual = soil_palette("redox, 4")

ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=TRT)) + geom_boxplot()
