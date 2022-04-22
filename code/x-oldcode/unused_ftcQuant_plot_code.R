#rename site codes and set levels, exclude active layer, set season levels
#this data is for a full profile that does not need active layer (september) isolated

# ftc_fulldat = ftc_dat %>% 
#       mutate(site = factor (site, levels = c("HEAL", "BONA", "BARR", "TOOL")),
#              site = recode (site, "HEAL" = "Healy",
#                             "BONA" = "Caribou-poker",
#                             "BARR" = "Barrow",
#                             "TOOL" = "Toolik"))%>% 
#       filter(!season == "activelayer" & !is.na(Def1)) %>%       
#       mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")))  

ftc_fulldat = ftc_dat %>% 
  mutate(site = recode (site, "TOOL" = "Toolik"))%>% 
  filter(!is.na(Def1)) %>%       
  mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer")))  

#next, isolate active layer, recode site names, and set levels
#this data is specifically for active layer plots

# ftc_actdat = ftc_dat %>% 
#   filter(season=="activelayer" & !is.na(Def1)) %>% 
#   mutate(site = recode (site, "HEAL" = "Healy",
#                         "BONA" = "Caribou-poker",
#                         "BARR" = "Barrow",
#                         "TOOL" = "Toolik"))%>% 
#   mutate(site = factor (site, levels = c("healy", "caribou-poker", "barrow", "toolik"))) 


#subset. why do this instead of filtering?

# ftc_actdat_subset = 
#   ftc_actdat %>% 
#   filter(site %in% c("healy", "toolik"))

ftc_fulldat_subset2 = 
  ftc_fulldat %>% 
  filter(site %in% c("Toolik")) 


# averages for geom_bar-------------------------------------------

#FT
ftc_avg = 
  ftc_fulldat %>% 
  mutate(depth_cm = depth_m*100) %>% 
  #filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, year, core, depth_cm) %>% 
  dplyr::mutate(total = sum(Def1)) %>% 
  ungroup() %>% 
  
  # now, incorporate the `total` data into `season`
  # spread the `season` columns, and then recombine with `total`
  spread(season, Def1) %>% 
  gather(season, Def1, total:summer) %>%
  # many NAs were introduced when forcing wide-form. remove all rows containing NAs
  na.omit() %>% 
  
  # now, calculate mean FTC per site/depth/season
  group_by(site, depth_cm, season) %>% 
  dplyr::summarise(ftc = as.integer(mean(Def1))) %>% 
  ungroup() %>% 
  
  # bin top 10 cm into 5-cm bins, and the rest into 10-cm bins
  
  mutate(depth_bins1 = case_when(depth_cm <= 10 ~ cut_width(depth_cm, width = 5, center=2.5)),
         depth_bins2 = case_when(depth_cm > 10 ~ cut_width(depth_cm, width = 10, center=5)),
         depth_bins = paste0(depth_bins1, depth_bins2),
         depth_bins = str_remove(depth_bins, "NA")) %>% 
  dplyr::select(-depth_bins1, -depth_bins2) %>% 
  
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


ftc_avg2 = ftc_avg %>% 
  mutate(depth = depth_stop_cm - depth_start_cm)

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
  #facet_grid(~season)+
  NULL

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

ftc_avg2 %>% 
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
  geom_rect(aes(xmin = seas_num -0.35, xmax = seas_num + 0.35, 
                ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.numeric(ftc)))+
  scale_y_reverse()+
  scale_x_continuous(breaks = 1:4,
                     labels = c("fall", "winter", "spring", "summer"))+
  annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
           linetype = 2) +
  scale_fill_gradientn(colors = (pnw_palette("Sunset2", 6
  )))+
  labs(
    y = "depth, cm",
    x = "",  
    fill = "freeze-thaw cycles, count")+
  
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
