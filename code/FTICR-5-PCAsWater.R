# Feb-24-2021
# FTICR
# Water Analysis
# PCAs and Permanovas

#load packages
source("code/FTICR-0-packages.R")

# 1. load files -----------------------------------------------------------

fticr_data_water = read.csv("fticr_data_water.csv") %>% select(ID, formula, Site, Trtmt, Material) 
fticr_meta_water = read.csv("fticr_meta_water.csv")
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)

## fticr_data_water contains peaks for each sample, i.e. each replicate



# 2. calculate relabund ---------------------------------------------------
## for PCA and PERMANOVA (and related stats), calculate relabund for each core

fticr_water_relabund = 
  fticr_data_water %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  group_by(ID, Site, Trtmt, Material, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(Site, Trtmt, Material) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

#
# 3. PCA ---------------------------------------------------------------------
## you will need relative abundance data for PCA 

## install the ggbiplot package from github
## install the miraKlein version, not vqv

## devtools::install_github("miraKlein/ggbiplot")
library(ggbiplot)


## 3a. all samples ---------------------------------------------------------
## step i. make wider
relabund_wide =
  fticr_water_relabund %>%
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts),
                Site, Trtmt, Material) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0) 

## step ii. split into numeric/factor dataframes, and run PCA on those
num = 
  relabund_wide %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_wide %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp$Material))+
  labs(title = "all samples, CON and FTC")+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Winter", 2)))+
  theme_er()+
  NULL


#
## 3b. CON only ---------------------------------------------------------
# use the format above for CON soils only
# append the grp, num, pca files with "_con" to distinguish from previous

num_con = 
  relabund_wide %>% 
  filter(Trtmt == "CON") %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp_con = 
  relabund_wide %>% 
  filter(Trtmt == "CON") %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  dplyr::mutate(row = row_number())

pca_con = prcomp(num_con, scale. = T)

ggbiplot(pca_con, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_con$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp_con$Material))+
  labs(title = "CON only")+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Winter", 2)))+
  theme_er()+
  NULL

#
## 3c. FTC only ---------------------------------------------------------
# use the format above for FTC soils only
# append the grp, num, pca files with "_ftc" to distinguish from previous

num_ftc = 
  relabund_wide %>% 
  filter(Trtmt == "FTC") %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp_ftc = 
  relabund_wide %>% 
  filter(Trtmt == "FTC") %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  dplyr::mutate(row = row_number())

pca_ftc = prcomp(num_ftc, scale. = T)

ggbiplot(pca_ftc, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_ftc$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp_ftc$Material))+
  labs(title = "FTC only")+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Winter", 2)))+
  theme_er()+
  NULL

#
## 3d. HEAL only ---------------------------------------------------------


#
## 3e. TOOL only ---------------------------------------------------------


#
# PERMANOVA ---------------------------------------------------------------
## use relabund_wide for PERMANOVA
## general format is: [all numeric columns] ~ [independent variables]
## broom::tidy summarizes the info from the model as a dataframe

## P-value tells you if a factor is significant
## R2 tells you the relative contribution of the factor to total variation
## e.g. R2 = 0.28 means "Material" accounted for 28 % of total variation

library(vegan)
# permanova_fticr_all = 
  adonis(relabund_wide %>% select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) ~ 
         (Site+Trtmt+Material)^2, 
       data = relabund_wide) 

# broom::tidy(permanova_fticr_all$aov.tab)
