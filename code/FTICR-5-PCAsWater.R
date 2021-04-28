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
# CON ONLY
relabund_pca =
  fticr_water_relabund %>%
  filter(Trtmt == 'CON') %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts),
                Site, Trtmt, Material) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0) 
  #dplyr::select(-1)


num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp$Material))+
  xlim(-4,10)+
  ylim(-3.5,5)+
  ggtitle("Control")+
  NULL +
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Winter", 2)))
  
# FTC ONLY
relabund_pca =
  fticr_water_relabund %>%
  filter(Trtmt == 'FTC') %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts),
                Site, Trtmt, Material) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0) 
#dplyr::select(-1)


num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp$Material))+
  xlim(-4,10)+
  ylim(-3.5,5)+
  ggtitle("Freeze-Thaw")+
  NULL +
  theme_er()+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Winter", 2)))


# TOOL vs. HEAL (no filtering) ----
relabund_pca =
  fticr_water_relabund %>% 
  #filter(Material == "Organic") %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts)) %>% 
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0)  
  #dplyr::select(-1)

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
  geom_point(size=5,stroke=1, aes(color = groups, shape = grp$Trtmt))+
  xlim(-4,10)+
  ylim(-3.5,5)+
  NULL

ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=5,stroke=1, aes(color = grp$Material, shape = grp$Trtmt))+
  xlim(-4,10)+
  ylim(-3.5,5)+
  ggtitle("PCA, No filtering")+
  theme_er()+
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



# Tool con vs ft ----
relabund_pca =
  fticr_water_relabund %>% 
  filter(Site == "TOOL" & Material == "Lower Mineral") %>% 
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

