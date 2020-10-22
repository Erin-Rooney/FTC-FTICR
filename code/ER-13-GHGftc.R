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

library(tidyverse)
#str(ghg_csv2)
#str(ftc_dat)
#str(ftc_fulldat)
#str(sommos_oc)
#str(probe_loc)

ftc_fulldat = ftc_fulldat %>% 
      mutate(site = factor (site, levels = c("HEAL", "BONA", "BARR", "TOOL")),
             site = recode (site, "HEAL" = "healy",
                            "BONA" = "caribou-poker",
                            "BARR" = "barrow",
                            "TOOL" = "toolik"))%>% 
      filter(!season == "activelayer" & !is.na(Def1)) %>%       
      mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")))  
    

             
             
ftc_actdat = ftc_fulldat %>% 
  filter(season=="activelayer" & !is.na(Def1)) %>% 
  mutate(site = factor (site, levels = c("healy", "caribou-poker", "barrow", "toolik"))) 



ftc_dat = ftc_dat %>% 
  filter(!season == "activelayer") %>% 
  mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")))


      
#mutate(TRT = factor(TRT, levels = c("CON", "FTC")))


# ggplot set up-----------------------------------
  theme_er <- function() {  # this for all the elements common across plots
    theme_bw() %+replace%
      theme(legend.position = "bottom",
            legend.key=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key.size = unit(1.5, 'lines'),
            panel.border = element_rect(color="black",size=2, fill = NA),
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
            axis.text = element_text(size = 12, color = "black"),
            axis.text.x.bottom = element_text (vjust = 0.5, hjust=1, angle = 90),
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


# theme_er2 <- function() {  # this for all the elements common across plots
#   theme_bw() %+replace%
#     theme(legend.position = "top",
#           legend.key=element_blank(),
#           legend.title = element_blank(),
#           legend.text = element_text(size = 12),
#           legend.key.size = unit(1.5, 'lines'),
#           panel.border = element_rect(color="black",size=2, fill = NA),
#           plot.title = element_text(hjust = 0.5, size = 14),
#           plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
#           axis.text = element_text(size = 12, color = "black"),
#           axis.title = element_text(size = 12, face = "bold", color = "black"),
#           # formatting for facets
#           panel.background = element_blank(),
#           strip.background = element_rect(colour="black", fill="black"), #facet formatting
#           panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
#           panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
#           strip.text.x = element_text(size=12, face="bold"), #facet labels
#           strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
#     )
# }

# library(ggplot2)
# library(soilpalettes)
# library(soilDB)
# library(aqp)
# library(sharpshootR)
# library(sp)
# library(ggridges)
#


# FT quant ggplots---------------------------------------

#boxplots
# ggplot(ftc_dat, aes(x = def1, y = depth_cm, fill = site)) + geom_boxplot() + theme_er() +
#   scale_fill_manual (values = soil_palette("gley", 2)) + facet_grid(season~.)
# 
# #dotplots all failed
# ggplot() + geom_dotplot(data = ftc_dat, aes(y = depth_m, x = def1, color = site)) + theme_er() +
#   scale_color_manual (values = soil_palette("gley", 2)) + facet_wrap(season~.)
# 
# ggplot(ftc_dat, aes(x=def1, y=depth_cm, fill=site))

#ridge and grid plots
# ggplot(ftc_dat, aes(x = def1, y = depth_cm, color = season)) +
# stat_bin2d( mapping = NULL, data = NULL, geom = "tile", position = "identity" + bins = 30 + binwidth = NULL + drop = TRUE)
# 
# ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
#   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
#   scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + facet_grid(season~.)
# 
# ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
#   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
#   scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + scale_x_continuous(limits = c(-5, 30)) + facet_grid(season~.)
# 
# ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
#   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
#   scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + scale_x_continuous(limits = c(-5, 30)) + facet_grid(season~.)
# 
# ggplot(ftc_dat, aes(x = depth_cm, y = site, height = def1)) +
#   geom_density_ridges(stat = "identity") + theme_er() + facet_grid(season~.)

#raster plots----------------------------------------------

# ggplot(ftc_dat, aes(x = depth_cm, y = site, fill = def1)) +
#   geom_raster(hjust = 0, vjust = 0) + theme_er() + facet_grid(season~.)

# bubble plot with depth on y axis---------------------------------


ftc_fulldat %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, size = as.character(Def1), color = as.character(Def1)))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse()+
  # scale_size_continuous()+
 # scale_color_gradient(low = "blue", high = "pink")+
  scale_color_manual(values = (PNWColors::pnw_palette("Bay",7)))+
  labs(y = "depth, cm", x = "", legend_title = "My title")+
  #ggtitle("Freeze Thaw Cycle Frequency") +
  theme_er() +
  facet_grid(~season)


###########

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


#ftc_actdat = ftc_actdat %>% 
 # filter(!Def1==NA)

levels(as.factor(ftc_actdat$site)) 


ftc_actdatheal = ftc_actdat %>% 
  filter(site=="HEAL")

ftc_actdattool = ftc_actdat %>% 
  filter(site=="TOOL")

ftc_actdatheal %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, color = Def1))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2), size = 7)+
  scale_y_reverse()+
  annotate("segment", x = 1.5, xend = 0.5, y = 39, yend = 39, color = "pink", size= 2) +
  # scale_size_continuous()+
  scale_color_gradient(low = "light blue", high = "brown")+
  ggtitle("Healy") +
  theme_er()

ftc_actdattool %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, color = Def1))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2), size = 7)+
  scale_y_reverse()+
  annotate("segment", x = 1.5, xend = 0.5, y = 9, yend = 9, color = "pink", size= 2) +
  # scale_size_continuous()+
  scale_color_gradient(low = "light blue", high = "brown")+
  ggtitle("Toolik") +
  theme_er()

ftc_actdat_subset = 
  ftc_actdat %>% 
  filter(site %in% c("HEAL", "TOOL"))

ftc_fulldat_subset2 = 
  ftc_fulldat %>% 
  filter(site %in% c("TOOL"))

ftc_actdat_subset %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, color = as.character(Def1)))+
  geom_point(position = position_jitter(width = 0.2), size = 7)+
  scale_y_reverse()+
  annotate("segment", x = 0.7, xend = 1.3, y = 9, yend = 9, color = "pink", size= 2) +
  annotate("segment", x = 1.5, xend = 2.5, y = 10, yend = 10, color = "pink", size= 2) +
  #scale_color_gradient(low = "light blue", high = "brown")+
  annotate("text", label = "organic soil\n(5 cm)", x = 1.5, y = 25, size = 4)+
  annotate("text", label = "active layer", x = 1.5, y = 6, size = 4)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Sunset2")))+
  #annotate("text", label = "perm", x = 1.5, y = 6, size = 4)+
  #  annotate("rect", xmax = 0.5, xmin = 1.5, ymax = 5, ymin = 25, 
  #           fill = "red", alpha = 0.5, color = "black", size = 3)+
  theme_er()

ftc_fulldat_subset2 %>% 
  filter(Def1 > 0) %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, color = as.character(Def1)))+
  geom_point(position = position_jitter(width = 0.2), size = 4.5)+
  geom_point(data = ftc_fulldat_subset2 %>% filter(Def1 == 0 &duration==24 & mag.vec==1.5 & depth_cm<100),
             position = position_jitter(width = 0.2), size = 2, color = "black")+
  scale_y_reverse()+
  #annotate("segment", x = 0.7, xend = 1.3, y = 50, yend = 50, color = "pink", size= 2) +
  annotate("segment", x = 0.7, xend = 1.3, y = 19, yend = 19, color = "pink", size= 2) +
  #scale_color_gradient(low = "light blue", high = "brown")+
  annotate("text", label = "organic soil\n(0-20 cm)", x = 1.3, y = 13, size = 4)+
  annotate("text", label = "upper mineral\n(25-50 cm)", x = 1.3, y = 40, size = 4)+
  annotate("text", label = "lower mineral\n(50-70 cm)", x = 1.3, y = 55, size = 4)+
  annotate("text", label = "aggregates 1-2", x = 0.63, y = 35, size = 4)+
  annotate("text", label = "aggregates 3-4", x = 0.63, y = 26, size = 4)+
  annotate("text", label = "aggregates 5-6", x = 0.63, y = 39, size = 4)+
  annotate(
    geom = "curve", x = 0.72, y = 37, xend = 0.8, yend = 44, 
   curvature = 0.3, arrow = arrow(length = unit(2, "mm")))+ 
  annotate(
    geom = "curve", x = 0.72, y = 28, xend = 0.8, yend = 34, 
    curvature = 0.3, arrow = arrow(length = unit(2, "mm")))+
  annotate(
    geom = "curve", x = 0.72, y = 41, xend = 0.8, yend = 47, 
    curvature = 0.3, arrow = arrow(length = unit(2, "mm")))+ 
  scale_color_manual(values = rev(PNWColors::pnw_palette("Sunset2")))+
  labs(
    title = "Freeze/Thaw Soil Profile from Toolik, Alaska", 
    y = "depth, cm",
    x = "",
    guides(fill=guide_legend(title= "Freeze/Thaw")))+
    #legend.title = "Freeze/Thaw Cycles during Maximum Thaw",
  #annotate("text", label = "perm", x = 1.5, y = 6, size = 4)+
  #  annotate("rect", xmax = 0.5, xmin = 1.5, ymax = 5, ymin = 25, 
  #           fill = "red", alpha = 0.5, color = "black", size = 3)+
  theme_er()


ftc_fulldat_subset %>% 
  filter(Def1 > 0) %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, color = as.character(Def1)))+
  geom_point(position = position_jitter(width = 0.2), size = 4.5)+
  geom_point(data = ftc_actdat_subset %>% filter(Def1 == 0 &duration==24 & mag.vec==1.5 & depth_cm<100),
             position = position_jitter(width = 0.2), size = 2, color = "black")+
  scale_y_reverse()+
  annotate("segment", x = 0.7, xend = 1.3, y = 50, yend = 50, color = "pink", size= 2) +
  annotate("segment", x = 1.7, xend = 2.3, y = 19, yend = 19, color = "pink", size= 2) +
  #scale_color_gradient(low = "light blue", high = "brown")+
  annotate("text", label = "organic soil\n(0-30 cm)", x = 1.5, y = 13, size = 4)+
  annotate("text", label = "upper mineral\n(25-50 cm)", x = 1.5, y = 43, size = 4)+
  annotate("text", label = "lower mineral\n(50-70 cm)", x = 1.5, y = 68, size = 4)+
  #annotate("text", label = "active layer", x = 1.5, y = 6, size = 4)+
  #annotate("text", label = "black = no ftc", x = 1.5, y = 50, size = 4)+
  #annotate(
  #geom = "curve", x = 0.5, y = 28, xend = 0.8, yend = 34, 
  #curvature = -0.5, arrow = arrow(length = unit(2, "mm")))+               
  scale_color_manual(values = rev(PNWColors::pnw_palette("Sunset2")))+
  labs(
    title = "Site Characteristics", 
    y = "depth, cm",
    guides(fill=guide_legend(title= "Freeze/Thaw")))+
  #legend.title = "Freeze/Thaw Cycles during Maximum Thaw",
  #annotate("text", label = "perm", x = 1.5, y = 6, size = 4)+
  #  annotate("rect", xmax = 0.5, xmin = 1.5, ymax = 5, ymin = 25, 
  #           fill = "red", alpha = 0.5, color = "black", size = 3)+
  theme_er()


ftc_fulldat %>% 
  filter(Def1 > 0) %>% 
  filter(duration==24 & mag.vec==1.5) %>%
  ggplot(aes(y = depth_cm, x = site, color = as.character(Def1)))+
  geom_point(position = position_jitter(width = 0.2), size = 4.5)+
  geom_point(data = ftc_actdat %>% filter(Def1 == 0 & duration==24 & mag.vec==1.5),
             position = position_jitter(width = 0.2), size = 2, color = "black")+
  scale_y_reverse()+
  #annotate("segment", x = 0.7, xend = 1.3, y = 50, yend = 50, color = "pink", size= 2) +
  #annotate("segment", x = 1.7, xend = 2.3, y = 19, yend = 19, color = "pink", size= 2) +
  #scale_color_gradient(low = "light blue", high = "brown")+
  #annotate("text", label = "organic soil\n(0-30 cm)", x = 1.5, y = 13, size = 4)+
  #annotate("text", label = "upper mineral\n(25-50 cm)", x = 1.5, y = 43, size = 4)+
  #annotate("text", label = "lower mineral\n(50-70 cm)", x = 1.5, y = 68, size = 4)+
  #annotate("text", label = "active layer", x = 1.5, y = 6, size = 4)+
  #annotate("text", label = "black = no ftc", x = 1.5, y = 50, size = 4)+
  # annotate(
  #geom = "curve", x = 2, y = 35, xend = 1, yend = 27, 
  # curvature = -0.5, arrow = arrow(length = unit(2, "mm")))+               
  scale_color_manual(values = rev(PNWColors::pnw_palette("Starfish")))+
  labs(
    title = "Site Characteristics", 
    y = "depth, cm",
    guides(fill=guide_legend(title= "Freeze/Thaw")))+
  #legend.title = "Freeze/Thaw Cycles during Maximum Thaw",
  #annotate("text", label = "perm", x = 1.5, y = 6, size = 4)+
  #  annotate("rect", xmax = 0.5, xmin = 1.5, ymax = 5, ymin = 25, 
  #           fill = "red", alpha = 0.5, color = "black", size = 3)+
  theme_er()

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

# gain in C ggplot----------------------------------------
ggplot(ghg_csv2, aes(x=trmt, y=gain_ug_g_oc, fill=day)) + geom_boxplot() 


ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + geom_boxplot()

ggplot(ghg_csv2, aes(x=day, y=gain_ug_g_oc, fill=site)) + geom_boxplot()

ggplot(ghg_csv, aes(x=day, y=gain_ug_g_oc, fill=site)) + geom_boxplot()

# heatmap--------------------------------------------------------------
ftc_actdat %>% 
  filter(depth_cm<100 ) %>%
  ggplot(aes(y = depth_cm, x = site, fill = Def1))+
  geom_tile()+
  scale_y_reverse()+
  coord_fixed(ratio=1/4)

ftc_actdat %>% 
  filter(depth_cm<100 ) %>% 
  ggplot(aes(y = depth_cm, x = site, fill = Def1))+
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
