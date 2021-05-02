# Feb-24-2021
# FTICR
# Water Analysis
# Relabund

#load packages
source("code/FTICR-0-packages.R")

# 1. load files -----------------------------------------------------------

fticr_data_water = read.csv("fticr_data_water.csv") %>% select(ID, formula, Site, Trtmt, Material) 
fticr_meta_water = read.csv("fticr_meta_water.csv")
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)

#
# 2. calculate relabund ---------------------------------------------------
## 2a. relative abundance of compound classes for each core/replicate ----
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

## ^^ use this for PCA and PERMANOVA (and related stats)

## 2b. relabund summary by treatment ----
## use this for bar graphs, summary tables, etc.
## if doing just bar plots, you need just mean(). 
## if creating summary table, you need mean() and se()

fticr_water_relabund_summarized = 
  fticr_water_relabund %>% 
  group_by(Site, Trtmt, Material, Class) %>% 
  dplyr::summarise(relabundance = round(mean(relabund), 2),
                   se = round(sd(relabund)/sqrt(n()),2))

## 2c. aromatic relabund ----
## start with the first relabund (by cores) and recode to aromatic/not aromatic

fticr_water_relabund_arom = 
  fticr_water_relabund %>% 
  mutate(aromatic_col = case_when(grepl("aromatic", Class) ~ "aromatic",
                                  Class == "aliphatic" ~ "aliphatic"),
                aromatic_col = if_else(is.na(aromatic_col), "other", aromatic_col)) %>% 
  group_by(ID, Site, Trtmt, Material, aromatic_col) %>% 
  dplyr::summarize(relabund = sum(relabund)) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))
  
#


# 3. plots ----------------------------------------------------------------
## 3a. plot relabund of aromatic ----

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
  ggplot(aes(x = Site, y = relabund, color = Trtmt, shape = Trtmt))+
  #geom_boxplot()+
  geom_point(position = position_dodge(width = 0.3), size = 2)+
  facet_grid(Material ~ .)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Lake", 2)))+
  theme_er()
## potential idea: do ANOVA with x = site, and report p-values in the graph
## then do ANOVA with x = trtmt for each site, and report sig. as asterisks

# stats for aromatic peaks

# relabund bar plots ----
# bar graph
fticr_water_relabund_summarized %>% 
  ggplot(aes(x = Trtmt, y = relabundance))+
  geom_bar(aes(fill = Class), stat = "identity")+
  facet_grid(Material ~ Site)+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  scale_fill_manual(values = rev(pnw_palette("Sunset",4)))


# run anova to get statistical significance
# then use that to create the label file below

label = tribble(
  ~Site, ~Trtmt, ~Material, ~y, ~label,
  ### THIS IS ONLY AN EXAMPLE 
  "HEAL", "FTC", "Lower Mineral", 50, "*"
) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

## NEXT STEPS:
# full summary table for relabund (with HSD stats)
# asterisks for bar plots