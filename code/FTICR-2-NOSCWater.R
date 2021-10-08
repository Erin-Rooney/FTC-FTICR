# Feb-24-2021
# FTICR
# Water Analysis
# NOSC

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

fticr_data_water = read.csv("fticr_data_water.csv")
fticr_meta_water = read.csv("fticr_meta_water.csv")
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)

# 2. NOSC and AImod plots_water-------------------------------
fticr_water = 
  fticr_data_water %>% 
  select(Core, formula, Site, Trtmt, Material) 

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

# ggplot(fticr_water_nosc, aes(NOSC, color = Site, fill = Site)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   geom_boxplot(aes(y = 120), width = 20, fill = NA)+
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_manual(values = rev(pnw_palette("Winter", 2))) +
#   scale_color_manual(values = rev(pnw_palette("Winter", 2))) + 
#   #scale_color_manual(values = rev(nord("afternoon_prarie", 2)))+
#   #scale_fill_manual(values =rev(nord("afternoon_prarie", 2)))+
#   # scale_color_manual(values = c("#e69b99", "#64a8a8"))+
#   # scale_fill_manual(values = c("#e69b99", "#64a8a8"))+
#   # scale_fill_nord("victory_bonds", 2)+
#   # scale_color_nord("victory_bonds", 2)+
#   labs(title = "NOSC, Water Extracted by Site",
#        x = 'nominal oxidation state of carbon')+
#   facet_grid(Material~Trtmt)+
#   theme(legend.position = "bottom")
# 
# ggplot(fticr_water_nosc, aes(NOSC, color = Trtmt, fill = Trtmt)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   geom_boxplot(aes(y = 120), width = 20, fill = NA)+
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_manual(values = rev(pnw_palette("Mushroom", 2))) +
#   scale_color_manual(values = rev(pnw_palette("Mushroom", 2))) + 
#   #scale_color_manual(values = rev(nord("afternoon_prarie", 2)))+
#   #scale_fill_manual(values =rev(nord("afternoon_prarie", 2)))+
#   # scale_color_manual(values = c("#e69b99", "#64a8a8"))+
#   # scale_fill_manual(values = c("#e69b99", "#64a8a8"))+
#   # scale_fill_nord("victory_bonds", 2)+
#   # scale_color_nord("victory_bonds", 2)+
#   labs(title = "NOSC, Water Extracted by Site",
#        x = 'nominal oxidation state of carbon')+
#   facet_grid(Material~Site)+
#   theme(legend.position = "bottom")


library(viridis)

# fticr_water_nosc %>% 
#   mutate(Site = recode(Site, "TOOL" = "Toolik",
#                        "HEAL" = "Healy")) %>% 
#   filter(Trtmt == 'CON') %>% 
#   ggplot(aes(NOSC, color = Site, fill = Site)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   geom_boxplot(aes(y = 120), width = 20, fill = NA)+
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_color_manual(values = c("#e69b99", "#64a8a8"))+
#   scale_fill_manual(values = c("#e69b99", "#64a8a8"))+
#   labs(x = 'nominal oxidation state of carbon')+
#   facet_grid(Material~.)+
#   theme(legend.position = "bottom")

######




###########################################

#common-loss

fticr_nosc_water_ftc_loss = 
  fticr_data_water_summarized %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, Site, Material) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  filter(n == 1) %>% 
  mutate(loss_gain = if_else(Trtmt == "CON", "lost", "gained")) %>% 
  left_join(fticr_meta_water) %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

fticr_nosc_water_ftc_loss_common = 
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
  left_join(fticr_meta_water) %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

#Figure for manuscript

fticr_nosc_water_ftc_loss_common %>% 
  filter(loss_gain != 'common') %>% 
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy"),
         Trtmt = recode(Trtmt, "CON" = "lost",
                        "FTC" = "gained")) %>%
  mutate(Trtmt = factor (Trtmt, levels = c("lost", "gained")))%>%
  ggplot(aes(x = Trtmt, y = NOSC, fill = Trtmt)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  facet_wrap(~Site)+
  labs(x = "")+
  ylim(-2,2)+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Sailboat", 2)))+
  theme_er()+
  theme(legend.position = "bottom")

fticr_nosc_water_ftc_loss_common %>% 
  filter(loss_gain != 'common') %>% 
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy"),
         Trtmt = recode(Trtmt, "CON" = "lost",
                        "FTC" = "gained"),
  ) %>% 
  mutate(Trtmt = factor (Trtmt, levels = c("lost", "gained")))%>%
  ggplot(aes(x = Trtmt, y = NOSC, fill = Trtmt)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  facet_grid(Site~Class)+
  labs(x = "")+
  ylim(-2,2)+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Sailboat", 2)))+
  theme_er()+
  theme(legend.position = "none", 
        axis.text.x.bottom = element_text 
        (vjust = 0.5, hjust=0.6, angle = 0)
)

fticr_nosc_water_ftc_loss_common %>% 
  filter(loss_gain != 'common') %>% 
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy"),
         Trtmt = recode(Trtmt, "CON" = "lost",
                        "FTC" = "gained"),
         ) %>% 
  mutate(Trtmt = factor (Trtmt, levels = c("lost", "gained")))%>%
  ggplot(aes(x = Trtmt, y = NOSC, fill = Trtmt)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  #geom_point(alpha = 0.2, size = 1)+
  facet_grid(Material~Site)+
  ylim(-2,2)+
  labs(x = "")+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Sailboat", 2)))+
  theme_er()+
  theme(legend.position = "none")


#common-loss stats--------------------

nosc_uniqueonly = 
  fticr_nosc_water_ftc_loss_common %>% 
  filter(loss_gain != 'common')



library(nlme)
heal = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL'))
summary(heal)
print(heal)
anova(heal)

tool = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL'))
summary(tool)
print(tool)
anova(tool)


c = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                         Class == 'aliphatic'))
summary(c)
print(c)
anova(c)

d = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                   Class == 'aromatic'))
summary(d)
print(d)
anova(d)


e = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                         Class == 'condensed aromatic'))
summary(e)
print(e)
anova(e)


f = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                   Class == 'unsaturated/lignin'))
summary(f)
print(f)
anova(f)



g = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                   Class == 'aliphatic'))
summary(g)
print(g)
anova(g)


h = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                   Class == 'aromatic'))
summary(h)
print(h)
anova(h)

i = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                   Class == 'condensed aromatic'))
summary(i)
print(i)
anova(i)


j = lme(NOSC ~ Trtmt, random = ~1|Material, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                   Class == 'unsaturated/lignin'))

summary(j)
print(j)
anova(j)


healO = lme(NOSC ~ Trtmt, random = ~1|Class, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                   Material == 'Organic'))
summary(healO)
print(healO)
anova(healO)

healU = lme(NOSC ~ Trtmt, random = ~1|Class, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                Material == 'Upper Mineral'))
summary(healU)
print(healU)
anova(healU)

healL = lme(NOSC ~ Trtmt, random = ~1|Class, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'HEAL',
                                                                                                Material == 'Lower Mineral'))
summary(healL)
print(healL)
anova(healL)


toolO = lme(NOSC ~ Trtmt, random = ~1|Class, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                Material == 'Organic'))
summary(toolO)
print(toolO)
anova(toolO)

toolU = lme(NOSC ~ Trtmt, random = ~1|Class, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                Material == 'Upper Mineral'))
summary(toolU)
print(toolU)
anova(toolU)

toolL = lme(NOSC ~ Trtmt, random = ~1|Class, na.action = na.omit, data = nosc_uniqueonly %>% filter(Site == 'TOOL',
                                                                                                Material == 'Lower Mineral'))
summary(toolL)
print(toolL)
anova(toolL)


############
#counts

nosc_uniqueonly %>% 
  group_by(Site, Trtmt) %>% 
  dplyr::summarise(count = n())

fticr_nosc_water_ftc_loss_common %>% 
  group_by(Site, Trtmt) %>% 
  dplyr::summarise(count = n())

nosc_uniqueonly %>% 
  group_by(Site, Material, Trtmt) %>% 
  dplyr::summarise(count = n())


nosc_uniqueonly %>% 
  group_by(Site, Class, Trtmt) %>% 
  dplyr::summarise(count = n())

fticr_nosc_water_ftc_loss_common %>% 
  group_by(Site, Class, Trtmt) %>% 
  dplyr::summarise(count = n())
