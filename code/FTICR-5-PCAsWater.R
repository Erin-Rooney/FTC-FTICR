# Feb-24-2021
# FTICR
# Water Analysis
# PCAs

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




###################
###################
###################

# PCA ---------------------------------------------------------------------
## install the ggbiplot package from github
## install the miraKlein version, not 

## you will need relative abundance data for PCA 

devtools::install_github("miraKlein/ggbiplot")
library(ggbiplot)


## all samples ----
## first, make wider
relabund_pca =
  fticr_water_relabund %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  %>% 
  dplyr::select(-1)


num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Trtmt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=1,stroke=1, aes(color = groups))+
  xlim(-4,10)+
  ylim(-3.5,5)+
  NULL



# TOOL vs. HEAL (con, organic only) ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(Trtmt == "CON" & Material == "Organic") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  %>% 
  dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=1,stroke=1, aes(color = groups))+
  xlim(-4,10)+
  ylim(-3.5,5)+
  NULL


# HEAL con vs ft  ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(Site == "HEAL") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  %>% 
  dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = grp$Trtmt, 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp$Material))+
  xlim(-4,5)+
  ylim(-3.5,4)+
  NULL


# HEAL con vs ft ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(Site == "HEAL" & Material == "Lower Mineral") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  %>% 
  dplyr::select(-1)

num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = grp$Trtmt, 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp$Material))+
  xlim(-4,5)+
  ylim(-3.5,4)+
  NULL
