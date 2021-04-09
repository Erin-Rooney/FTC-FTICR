# Erin Rooney
# Adapted from KP
# July 23 2020
# GHG FTC C12 and C13 data

# load data---------------------------------
ghg_csv2 = read.csv("processed/ghg_depth.csv")
#ftc_dat = read.csv("processed/FTC_quant_inprocess.csv")
ftc_dat = read.csv("processed/final_dat2.csv")
sommos_oc = read.csv("processed/oc_sommos_neonoc.csv")
probe_loc = read.csv("processed/Probe Locations.csv")

# set data frames-----------------------------

library(tidyverse)
library(forcats) 
library(nord)
library(PNWColors)
library(soilpalettes)

#rename site codes and set levels, exclude active layer, set season levels
#this data is for a full profile that does not need active layer (september) isolated

ftc_fulldat = ftc_dat %>% 
      mutate(site = factor (site, levels = c("HEAL", "BONA", "BARR", "TOOL")),
             site = recode (site, "HEAL" = "Healy",
                            "BONA" = "Caribou-poker",
                            "BARR" = "Barrow",
                            "TOOL" = "Toolik"))%>% 
      filter(!season == "activelayer" & !is.na(Def1)) %>%       
      mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")))  
    

#next, isolate active layer, recode site names, and set levels
#this data is specifically for active layer plots
             
ftc_actdat = ftc_dat %>% 
  filter(season=="activelayer" & !is.na(Def1)) %>% 
  mutate(site = recode (site, "HEAL" = "Healy",
                        "BONA" = "Caribou-poker",
                        "BARR" = "Barrow",
                        "TOOL" = "Toolik"))%>% 
  mutate(site = factor (site, levels = c("healy", "caribou-poker", "barrow", "toolik"))) 


#subset. why do this instead of filtering?

ftc_actdat_subset = 
  ftc_actdat %>% 
  filter(site %in% c("healy", "toolik"))

ftc_fulldat_subset2 = 
  ftc_fulldat %>% 
  filter(site %in% c("Toolik")) 


# averages for geom_bar-------------------------------------------

#FT
ftc_avg = 
  ftc_fulldat %>% 
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


ftc_avg = ftc_avg %>% 
  mutate(depth = depth_stop_cm - depth_start_cm)


#GHG
# calculate mean ghg

ghg_avg = 
  ghg_csv2 %>% 
  #filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, horizon, trmt, mid) %>% 
  dplyr::mutate(total = sum(gain_ug_g_oc)) %>% 
  ungroup() %>% 
  
  # now, incorporate the `total` data into `season`
  # spread the `season` columns, and then recombine with `total`
  #spread(season, Def1) %>% 
  #wgather(season, Def1, total:winter) %>%
  # many NAs were introduced when forcing wide-form. remove all rows containing NAs
  na.omit() %>% 
  
  # now, calculate mean FTC per site/depth/season
  group_by(site, horizon, trmt, mid) %>% 
  dplyr::summarise(gain_ug_g_oc = as.integer(mean(gain_ug_g_oc))) %>% 
  ungroup() %>% 
  
  # create new columns for depth range
  # create bins of 5 cm depth increments
  mutate(depth_bins = cut_width(mid, width = 5, center=2.5)) %>% 
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


# ggplot set up-----------------------------------
  theme_er1 <- function() {  # this for all the elements common across plots
    theme_bw() %+replace%
      theme(legend.position = "bottom",
            #legend.key=element_blank(),
            #legend.title = element_blank(),
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

theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "bottom",
          #legend.key=element_blank(),
          #legend.title = element_blank(),
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



# FT quant bubble plot with depth on y axis---------------------------------

ftc_fulldat %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, size = as.character(Def1), color = as.character(Def1)))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse()+
  # scale_size_continuous()+
 # scale_color_gradient(low = "blue", high = "pink")+
  #scale_color_manual(values = (PNWColors::pnw_palette("Bay",7)))+
  annotate("rect", xmax = 4.45, xmin = 3.55, ymax = 100, ymin = -0.5, 
           fill = "yellow", alpha = 0.1, color = "gray55", size = 0.5) +
  labs(y = "depth, cm", x = "", color = "Freeze/Thaw Cycles", size = "Freeze/Thaw Cycles")+
  #ggtitle("Freeze Thaw Cycle Frequency") +
  theme_er1() +
  facet_grid(~season)

# next plot

ftc_fulldat %>% 
  filter(duration==1 & mag.vec==0.1 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, size = fct_reorder(as.character(Def1), Def1), color = fct_reorder(as.character(Def1), Def1)))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse()+
  # scale_size_continuous()+
  # scale_color_gradient(low = "blue", high = "pink")+
  #scale_color_manual(values = (PNWColors::pnw_palette("Starfish",100)))+
  annotate("rect", xmax = 4.45, xmin = 3.55, ymax = 100, ymin = -0.5, 
           fill = "yellow", alpha = 0.1, color = "gray55", size = 0.5) +
  labs(y = "depth, cm", x = "", color = "Freeze/Thaw Cycles", size = "Freeze/Thaw Cycles")+
  #ggtitle("Freeze Thaw Cycle Frequency") +
  theme_er1() +
  facet_grid(~season)


# next plot

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
  
# next plot

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

#next plot

ftc_fulldat_subset2 %>% 
  filter(Def1 > 0) %>% 
  filter(duration==24 & mag.vec==1.5 & depth_cm<100) %>%
  ggplot(aes(y = depth_cm, x = site, color = as.character(Def1)))+
  geom_point(position = position_jitter(width = 0.2), size = 4.5)+
  geom_point(data = ftc_fulldat_subset2 %>% filter(Def1 == 0 &duration==24 & mag.vec==1.5 & depth_cm<100),
             position = position_jitter(width = 0.2), size = 2, color = "black")+
  scale_y_reverse()+
  #annotate("segment", x = 0.7, xend = 1.3, y = 50, yend = 50, color = "pink", size= 2) +
  annotate("segment", x = 0.7, xend = 1.3, y = 19, yend = 19, color = "red", alpha = 0.4, size= 1.5) +
  #scale_color_gradient(low = "light blue", high = "brown")+
  annotate("text", label = "organic soil\n(0-20 cm)", x = 1.4, y = 13, size = 4)+
  annotate("text", label = "upper mineral\n(25-50 cm)", x = 1.4, y = 40, size = 4)+
  annotate("text", label = "lower mineral\n(50-70 cm)", x = 1.4, y = 55, size = 4)+
  annotate("text", label = "Core B \n 40-50 cm", x = 0.61, y = 37, size = 4)+
  annotate("text", label = "Core A \n 28-38 cm", x = 0.65, y = 23, size = 4)+
  annotate("text", label = "Core C \n 41-50 cm", x = 0.58, y = 53, size = 4)+
  annotate(
    geom = "curve", x = 0.68, y = 41, xend = 0.8, yend = 44, 
   curvature = 0.3, arrow = arrow(length = unit(2, "mm")))+ 
  annotate(
    geom = "curve", x = 0.72, y = 28, xend = 0.8, yend = 34, 
    curvature = 0.25, arrow = arrow(length = unit(2, "mm")))+
  annotate(
    geom = "curve", x = 0.64, y = 51, xend = 0.8, yend = 47, 
    curvature = -0.3, arrow = arrow(length = unit(2, "mm")))+ 
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  labs(
    title = "Freeze/Thaw Soil Profile \n Toolik, Alaska \n ", 
    y = "depth, cm",
    x = "", 
    color = "Freeze/Thaw Cycles")+
    #legend.title = "Freeze/Thaw Cycles during Maximum Thaw",
  #annotate("text", label = "perm", x = 1.5, y = 6, size = 4)+
  #  annotate("rect", xmax = 0.5, xmin = 1.5, ymax = 5, ymin = 25, 
  #           fill = "red", alpha = 0.5, color = "black", size = 4)+
  theme_er()





# same as above but this time with geom_bar



# misplaced code

ftc_avg %>% 
  filter(!season %in% "total",
         site %in% "Toolik") %>%
  #mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>%
  #mutate(season = factor(season, levels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(y = depth, x = site, fill = ftc))+
  geom_bar(position = "stack", stat= "identity")+
  #annotate("text", label = "organic soil\n(0-20 cm)", x = 1, y = 5, size = 3.5, color = "white")+
  #annotate("text", label = "upper mineral\n(25-50 cm)", x = 1, y = 25, size = 3.5, color = "white")+
  #annotate("text", label = "lower mineral\n(50-70 cm)", x = 1, y = 40, size = 3.5, color = "black")+
  scale_y_reverse()+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Sunset2")))+  
  theme_er()+
  facet_wrap(.~season)

ftc_avg %>% 
  filter(!season %in% "total",
         site %in% "Toolik") %>%
  #mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>%
  #mutate(season = factor(season, levels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(y = depth_cm, x = ftc, color = season, size = 4))+
  geom_line(aes(group = season))+
  #annotate("text", label = "organic soil\n(0-20 cm)", x = 1, y = 5, size = 3.5, color = "white")+
  #annotate("text", label = "upper mineral\n(25-50 cm)", x = 1, y = 25, size = 3.5, color = "white")+
  #annotate("text", label = "lower mineral\n(50-70 cm)", x = 1, y = 40, size = 3.5, color = "black")+
  scale_y_reverse()+
  #scale_color_gradientn(colors = (PNWColors::pnw_palette("Sunset2")))+  
  theme_er()
  #facet_wrap(.~season)

pal=PNWColors::pnw_palette("Anemone",100)

##############changing code to switch to geom_rect to correct depth error

ftc_avg_seasnum = 
  ftc_avg %>% 
  mutate(season = factor(season)) %>% 
  mutate(seas_num = recode(season, "fall" = 1,
                           "winter" = 2,
                           "spring" = 3,
                           "summer" = 4,
                           "total" = 5))
                           
                           
ftc_avg_seasnum %>% 
  filter(seas_num < 5,
         site %in% "Toolik",
         depth_cm<60) %>%
  ggplot(aes(ymin = depth_start_cm, ymax = depth_stop_cm, xmin = seas_num, xmax = seas_num, fill = ftc))+
  geom_rect()+
  scale_y_reverse()+
  annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
           linetype = 2) +
  scale_fill_gradientn(colors = (pnw_palette("Sunset2", 3)))+
  labs(
    y = "depth, cm",
    x = "", 
    color = "Freeze/Thaw Cycles")+
   theme_er()


ftc_avg_seasnum %>%
  filter(seas_num < 5, site == "Toolik", depth_start_cm < 60) %>% 
  ggplot()+
  geom_rect(aes(xmin = seas_num -0.2, xmax = seas_num + 0.2, 
                ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.character(ftc)))+
  scale_y_reverse()+
  scale_x_continuous(breaks = 1:4,
                     labels = c("fall", "winter", "spring", "summer"))+
  annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
           linetype = 2) +
  scale_fill_gradientn(values = (pnw_palette("Sunset2")))+
  labs(
    y = "depth, cm",
    x = "", 
    color = "Freeze/Thaw Cycles")+
  theme_er()


#



ftc_fulldat %>% 
  filter(duration==1 & mag.vec==0.1 & depth_cm<150) %>%
  ggplot(aes(y = depth_cm, x = Def1, color = season))+
  #geom_point()+
  stat_summary(fun = "mean", geom = "line", size=3, alpha = 0.75)+
  #geom_point(data = ftc_fulldat_subset2 %>% filter(Def1 == 0 &duration==24 & mag.vec==1.5 & depth_cm<100),
             #position = position_jitter(width = 0.2), size = 2, color = "black")+
  scale_y_reverse()+
  #annotate("segment", x = 0.7, xend = 1.3, y = 50, yend = 50, color = "pink", size= 2) +
  # annotate("segment", x = 0.7, xend = 1.3, y = 19, yend = 19, color = "red", alpha = 0.4, size= 1.5) +
  # annotate("text", label = "organic soil\n(0-20 cm)", x = 1.4, y = 13, size = 4)+
  # annotate("text", label = "upper mineral\n(25-50 cm)", x = 1.4, y = 40, size = 4)+
  # annotate("text", label = "lower mineral\n(50-70 cm)", x = 1.4, y = 55, size = 4)+
  # annotate("text", label = "Core B \n 40-50 cm", x = 0.61, y = 37, size = 4)+
  # annotate("text", label = "Core A \n 28-38 cm", x = 0.65, y = 23, size = 4)+
  # annotate("text", label = "Core C \n 41-50 cm", x = 0.58, y = 53, size = 4)+
  scale_color_manual(values = (PNWColors::pnw_palette("Sailboat", 4)))+
  labs(
    title = "Freeze/Thaw Soil Profile \n Toolik, Alaska \n ", 
    y = "depth, cm",
    x = "Freeze/Thaw Cycles", 
    color = "Season")+
  theme_er()+
  facet_grid(.~site)




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

# GHG bubble plot with depth on y axis--------------------------------------
ghg_csv2 %>% 
  filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = site, size = gain_ug_g_oc, color = gain_ug_g_oc))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.05))+
  scale_y_reverse() +
  coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_gradient(low = "blue", high = "yellow")+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(day~trmt)



# Okay so here I am taking the averages that I got above and just adding a column for depth

ghg_corr = ghg_avg %>% 
  mutate(depth = depth_stop_cm - depth_start_cm)

# Okay now this is a figure. It's not working well and has issues, but it is a barplot for the ghg respiration data

ghg_corr %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = depth, x = site, fill = gain_ug_g_oc))+
  #geom_jitter()+
  geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_fill_gradient(low = "blue", high = "yellow")+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(~trmt)

# okay so I need to try this same data but in a line plot. What's great about the following plots
# is that I figured out how to align them vertically. 

#I don't know if I actually need to load these

library(reshape2)
library(plotly)

#And here's the plot. But it's using the averages so I don't have data by day.

ghg_avg %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = gain_ug_g_oc, color = trmt))+
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(.~site)


# loading another package I think I actually need this one.

library(plyr)

#Okay now I am creating a new data frame that has some statistical summaries

ghg_summary = ddply(ghg_csv2, c("site", "trmt", "mid"), summarise,
      N    = length(gain_ug_g_oc),
      mean = mean(gain_ug_g_oc),
      sd   = sd(gain_ug_g_oc),
      se   = sd / sqrt(N)
)

# okay here's a second summary because I wanted to include the day info in the statistical summaries

ghg_summary2 = ddply(ghg_csv2, c("site", "trmt", "mid", "day"), summarise,
                     N    = length(gain_ug_g_oc),
                     mean = mean(gain_ug_g_oc),
                     sd   = sd(gain_ug_g_oc),
                     se   = sd / sqrt(N)
)

#okay this is supposed to be helpful for adding error bars to my line/points but it's not working
pd <- position_dodge(0.1)

#alright here's a figure. no error bars. but it does have sd info in the data frame but it looks real weird when I put it in the plot
#important to note, all days are grouped in this plot/data frame

ghg_summary %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = mean, color = trmt))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(.~site)

ghg_summary2 %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = gain_ug_g_oc, color = day))+
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(site~trmt)

#and now a plot that separates out days. Not sure why error bars aren't showing up :(


ghg_csv2 %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = gain_ug_g_oc, color = trmt, group = trmt))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.1, 
  #                  )) +
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(day~site)


ghg_csv2 %>% 
  mutate(day = factor(day, levels = c('day1', 'day4', 'day7', 'day14'))) %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = gain_ug_g_oc, color = day, group = day))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.1, 
  #                  )) +
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 4)))+
  ggtitle("Respiration (ug per g OC)") +
  labs(y = "depth, cm",
       x = 'OC ug/g soil',
       title = "Respiration")+
  theme_er() +
  facet_grid(trmt~site)

#and now we're out of the ghg and into the ftc.

ftc_avg_depth %>% 
  filter(!season %in% "total") %>% 
  mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL"))) %>%
  mutate(season = factor(season, levels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(y = depth, x = site, fill = ftc))+
  geom_bar(position = "stack", stat= "identity")+
  facet_grid(.~season)+
  scale_y_reverse()+
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  theme_kp()


#and here's a sommos figure


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

# ggplot(ghg_csv2, aes(x = day, y = gain_ug_g_oc, color = day)) +
#   geom_dotplot() +
#   xlab("Day") +
#   ylab("CO2 AVG") +
#   ggtitle("12C")
# 
# 
# 
# 
# a = ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot() 
# 
# a + theme_er + scale_fill_manual = soil_palette("redox, 4")
# 
# ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=TRT)) + geom_boxplot()
