# Feb-24-2021
# FTICR
# Water Analysis
# NOSC

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

fticr_data_water = read.csv("processed/fticr_data_water.csv")
fticr_meta_water = read.csv("processed/fticr_meta_water.csv")
meta_hcoc_water  = read.csv("processed/fticr_meta_hcoc_water.csv") %>% select(-Mass)

# 2. NOSC and AImod plots_water-------------------------------
fticr_water = 
  fticr_data_water %>% 
  select(ID, formula, Site, Trtmt, Material) 

fticr_data_water_summarized = 
  fticr_water %>% 
  distinct(Site, Trtmt, Material, formula) %>% mutate(presence = 1)


#3. NOSC--------------------------------

#merge _summarized with _meta

fticr_water_nosc = 
  fticr_data_water_summarized %>%
  left_join(fticr_meta_water) %>%
  dplyr::select(formula, NOSC, HC, OC, Class, Material, Trtmt, Site)

fticr_water_nosc =
  fticr_water_nosc %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

ggplot(fticr_water_nosc, aes(NOSC, color = Site, fill = Site)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  geom_boxplot(aes(y = 120), width = 20, fill = NA)+
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = rev(pnw_palette("Winter", 2))) +
  scale_color_manual(values = rev(pnw_palette("Winter", 2))) + 
  #scale_color_manual(values = rev(nord("afternoon_prarie", 2)))+
  #scale_fill_manual(values =rev(nord("afternoon_prarie", 2)))+
  # scale_color_manual(values = c("#e69b99", "#64a8a8"))+
  # scale_fill_manual(values = c("#e69b99", "#64a8a8"))+
  # scale_fill_nord("victory_bonds", 2)+
  # scale_color_nord("victory_bonds", 2)+
  labs(title = "NOSC, Water Extracted by Site",
       x = 'nominal oxidation state of carbon')+
  facet_grid(Material~Trtmt)+
  theme(legend.position = "bottom")

library(viridis)

fticr_water_nosc %>% 
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy")) %>% 
  filter(Trtmt == 'CON') %>% 
  ggplot(aes(NOSC, color = Site, fill = Site)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  geom_boxplot(aes(y = 120), width = 20, fill = NA)+
  facet_grid(Material ~ .) +
  theme_er() +
  scale_color_manual(values = c("#e69b99", "#64a8a8"))+
  scale_fill_manual(values = c("#e69b99", "#64a8a8"))+
  labs(x = 'nominal oxidation state of carbon')+
  facet_grid(Material~.)+
  theme(legend.position = "bottom")

# NOSC by compound class
fticr_water_nosc %>% 
  filter(Site == "HEAL") %>% 
  ggplot(aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  geom_boxplot(aes(y = 100), width = 20, fill = NA)+
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  ggtitle("NOSC, Water Extracted HEAL")+
  facet_grid(Material~Class)

fticr_water_nosc %>% 
  filter(Site == "TOOL") %>% 
  ggplot(aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  ggtitle("NOSC, Water Extracted TOOL")+
  facet_grid(Material~Class)

fticr_water_nosc %>% 
  filter(Trtmt == "FTC") %>% 
  ggplot(aes(NOSC, color = Site, fill = Site)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_nord("victory_bonds", 2)+
  scale_color_nord("victory_bonds", 2)+
  ggtitle("NOSC, Water Extracted FTC")+
  facet_grid(Material~Class)

fticr_water_nosc %>% 
  filter(Trtmt == "CON") %>% 
  ggplot(aes(NOSC, color = Site, fill = Site)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_nord("victory_bonds", 2)+
  scale_color_nord("victory_bonds", 2)+
  ggtitle("NOSC, Water Extracted CON")+
  facet_grid(Material~Class)


ggplot(fticr_water_nosc, aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.05) +
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual (values = soil_palette("eutrostox", 2)) +
  scale_color_manual(values = soil_palette("eutrostox", 2)) +
  ggtitle("NOSC, Water Extracted by Treatment")

ggplot(fticr_water_nosc, aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.1) +
  facet_grid(Material ~ Site) +
  theme_er() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Starfish", 2)))+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Starfish", 2)))+
  ggtitle("NOSC, Water Extracted")

ggplot(fticr_water_nosc, aes(NOSC, color = Site))+
  geom_histogram(aes(y = stat(count)/sum(count))) +
  scale_y_continuous(labels = scales::percent)

ggplot(fticr_water_nosc, aes(x = NOSC, color = Site, fill = Site))+
  geom_histogram(alpha = 0.5, position = "identity")+
  facet_grid(Material~.) + 
  theme_er() +
  scale_fill_manual (values = soil_palette("gley", 2)) +
  scale_color_manual (values = soil_palette("gley", 2)) +
  ggtitle("NOSC, Water Extracted")