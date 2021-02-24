# Feb-24-2021
# FTICR
# Water Analysis
# Van Krevelin

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

fticr_data_water = read.csv("fticr_data_water.csv")
fticr_meta_water = read.csv("fticr_meta_water.csv")
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)
### ^^ the files above have aliph as well as aromatic for the same sample, which can be confusing/misleading
### create an index combining them

fticr_water = 
  fticr_data_water %>% 
  select(ID, formula, Site, Trtmt, Material) 

fticr_data_water_summarized = 
  fticr_water %>% 
  distinct(Site, Trtmt, Material, formula) %>% mutate(presence = 1)

# van krevelen plots_water------------------------------------------------------

fticr_water_hcoc =
  fticr_data_water_summarized %>% 
  left_join(fticr_meta_water) %>% 
  dplyr::select(formula, Site, Trtmt, Material, HC, OC)




# fticr_water = 
#   fticr_data_water %>% 
#   select(formula, Site, Trtmt, Material) 
# 
# fticr_water_trt = 
#   fticr_water %>% 
#   distinct(Site, Trtmt, Material, formula) %>% 
#   left_join(fticr_water_nosc_trt) %>% 
#   mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))
# 
fticr_water_hcoc %>% 
  filter(Trtmt %in% "CON") %>% 
  ggplot(aes(x=OC, y=HC, color = OC))+
  geom_point(alpha = 0.2, size = 1)+
  #stat_ellipse(show.legend = F)+
  #stat_ellipse()+
  facet_grid(Material ~ Site)+
  #geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  #geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  #geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  labs(title = "Water extracted FTICR-MS",
       x = "O:C",
       y = "H:C",
       color = "NOSC")+
  theme_er()+
  scale_color_gradientn(colors = pnw_palette("Starfish"))

fticr_water_hcoc %>% 
  ggplot(aes(x=OC, y=HC, color = Site))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  facet_grid(Material ~.)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("Water extracted FTICR-MS")+
  theme_er() +
  scale_color_manual (values = soil_palette("redox", 2))

fticr_water_hcoc %>% 
  ggplot(aes(x=OC, y=HC, color = Site))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  facet_grid(Material ~ Trtmt)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  theme_bw()


fticr_water_hcoc %>% 
  ggplot(aes(x=OC, y=HC, color = Trtmt))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  facet_grid(Material ~.)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("Water extracted FTICR-MS")+
  facet_grid(Material ~ Site)+
  theme_er() +
  scale_color_manual (values = soil_palette("redox", 2))

## calculate peaks lost/gained ---- 

# this does only unique loss/gain by CON vs FTC
fticr_water_ftc_loss = 
  fticr_data_water_summarized %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, Site, Material) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  filter(n == 1) %>% 
  mutate(loss_gain = if_else(Trtmt == "CON", "lost", "gained")) %>% 
  left_join(meta_hcoc_water) %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

fticr_water_ftc_loss_common = 
  fticr_data_water_summarized %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, Site, Material) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  # filter(n == 1) %>% 
  mutate(loss_gain = case_when(n == 2 ~ "common",
                               (n == 1 & Trtmt == "CON") ~ "lost",
                               (n == 1 & Trtmt == "FTC") ~ "gained")) %>% 
  left_join(meta_hcoc_water) %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

# plot only lost/gained
fticr_water_ftc_loss %>% 
  ggplot(aes(x = OC, y = HC, color = loss_gain))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("Water extracted FTICR-MS")+
  facet_grid(Material ~ Site)+
  theme_er() +
  scale_color_manual (values = rev(soil_palette("redox", 2)))

# plot common as well as lost/gained
fticr_water_ftc_loss_common %>% 
  filter(loss_gain == "common") %>% 
  ggplot()+
  geom_point(aes(x = OC, y = HC), color = "grey80", alpha = 0.2, size = 1)+
  geom_point(data = fticr_water_ftc_loss_common %>% filter(loss_gain != "common"), 
             aes(x = OC, y = HC, color = loss_gain), alpha = 0.2, size = 1)+
  #geom_point(alpha = 0.2, size = 1)+
  #stat_ellipse(show.legend = F)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("Water extracted FTICR-MS")+
  labs(caption = "grey = common to both")+
  facet_grid(Material ~ Site)+
  theme_er() +
  scale_color_manual (values = rev(soil_palette("redox", 2)))
