# Feb-24-2021
# FTICR
# Water Analysis
# Relabund

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

## aromatic rel_abund

fticr_water_relabund = 
  fticr_data_water_summarized %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  group_by(Site, Trtmt, Material, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(Site, Trtmt, Material) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

fticr_water_relabund_arom = 
  fticr_water %>% 
  left_join(select(fticr_meta_water, formula, AImod, HC, OC), by = "formula") %>% 
  dplyr::mutate(aromatic_col = case_when(AImod>0.5 ~ "aromatic",
                                         (HC<2.0 & HC>1.5) ~ "aliphatic"),
                aromatic_col = if_else(is.na(aromatic_col), "other", aromatic_col)) %>% 
  ## create a column for group counts
  group_by(ID, Site, Trtmt, Material, aromatic_col) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(ID, Site, Trtmt, Material) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2))


## plot relabund of aromatic

fticr_water_relabund_arom %>% 
  filter(aromatic_col %in% "aromatic") %>% 
  ggplot(aes(x = Trtmt, y = relabund, color = Trtmt, shape = Trtmt))+
  #geom_boxplot()+
  geom_point()+
  facet_grid(Material ~ Site)+
  theme_er()

fticr_water_relabund_arom %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral"))) %>% 
  filter(aromatic_col %in% "aromatic") %>% 
  ggplot(aes(x = Site, y = relabund, color = Trtmt, shape = Trtmt, size = 4))+
  #geom_boxplot()+
  geom_point(position = position_dodge(width = 0.3))+
  facet_grid(Material ~ .)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Lake", 2)))+
  theme_er()
## potential idea: do ANOVA with x = site, and report p-values in the graph
## then do ANOVA with x = trtmt for each site, and report sig. as asterisks

# stats for aromatic peaks





# relative abundance ------------------------------------------------------
fticr_data_water_summarized = 
  fticr_water %>% 
  group_by(formula, Site, Trtmt, Material) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(presence = 1) %>% 
  dplyr::select(-n)

## NOTE: calculate relative abundance PER SAMPLE and then combine by treatment
fticr_water_relabund = 
  fticr_data_water %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  group_by(ID, Site, Trtmt, Material, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(ID, Site, Trtmt, Material) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))
## use this file (relabund by sample) for PCA and PERMANOVA

## but summarize for bar plots
fticr_water_relabund_summarized = 
  fticr_water_relabund %>% 
  group_by(Site, Trtmt, Material, Class) %>% 
  dplyr::summarise(relabund = mean(relabund))



# run anova to get statistical significance
# then use that to create the label file below

label = tribble(
  ~Site, ~Trtmt, ~Material, ~y, ~label,
  ### THIS IS ONLY AN EXAMPLE 
  "HEAL", "FTC", "Lower Mineral", 50, "*"
) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

# bar graph
fticr_water_relabund_summarized %>% 
  ggplot(aes(x = Trtmt, y = relabund))+
  geom_bar(aes(fill = Class), stat = "identity")+
  facet_grid(Material ~ Site)+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  scale_fill_manual(values = rev(pnw_palette("Sunset",4)))

