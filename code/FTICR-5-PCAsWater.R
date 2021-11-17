# Feb-24-2021
# FTICR
# Water Analysis
# PCAs and Permanovas

#load packages
source("code/FTICR-0-packages.R")

# 1. load files -----------------------------------------------------------

fticr_data_water = read.csv("processed/fticr_data_water.csv") %>% select(Core, formula, Site, Trtmt, Material) 
fticr_meta_water = read.csv("processed/fticr_meta_water.csv")
# meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)

## fticr_data_water contains peaks for each sample, i.e. each replicate



# 2. calculate relabund ---------------------------------------------------
## for PCA and PERMANOVA (and related stats), calculate relabund for each core

fticr_water_relabund = 
  fticr_data_water %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  group_by(Core, Site, Trtmt, Material, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(Core, Site, Trtmt, Material) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral"))) 


fticr_data_water_summarized = 
  fticr_data_water %>% 
  distinct(Core, Site, Trtmt, Material, formula) %>% mutate(presence = 1)

# van krevelen plots_water------------------------------------------------------

 

fticr_water_hcoc =
  fticr_data_water_summarized %>% 
  left_join(fticr_meta_water) %>% 
  dplyr::select(Core, Site, Trtmt, Material, HC, OC, NOSC) %>% 
  group_by(Core, Site, Trtmt, Material) %>% 
  dplyr::summarize(OC_mean = mean(OC),
                   HC_mean = mean(HC),
                   NOSC_median = median(NOSC)) %>% 
  ## create a column for total counts
  group_by(Core, Site, Trtmt, Material)

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

hcoc_wide =
  fticr_water_hcoc %>% 
  ungroup %>% 
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

###


ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$Site),
         alpha = 0,
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(fill = groups, color = groups, shape = interaction(grp$Material, grp$Trtmt)))+
  labs(shape = "", fill = "", color = "",
       caption = "solids = control")+
  scale_shape_manual(values = c(21, 22, 25, 1, 0, 6))+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))+
  ylim(-3,5)+
  xlim(-3,5)+
  theme_er()+
  guides(fill=guide_legend(override.aes=list(fill="black")))+
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

a = ggbiplot(pca_con, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_con$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = grp_con$Site, shape = grp_con$Material))+
  #labs(title = "Control only")+
  ylim(-4,4)+
  xlim(-4,4)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))+
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

b = ggbiplot(pca_ftc, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_con$Site), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = grp_con$Site, shape = grp_con$Material))+
  #labs(title = "Control only")+
  ylim(-4,4)+
  xlim(-4,4)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))+
  theme_er()+
  NULL

#
## 3d. HEAL only ---------------------------------------------------------

num_heal = 
  relabund_wide %>% 
  filter(Site == "HEAL") %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp_heal = 
  relabund_wide %>% 
  filter(Site == "HEAL") %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  mutate(Trtmt = recode(Trtmt, "FTC" = "freeze-thaw",
                        "CON" = "control")) %>% 
  dplyr::mutate(row = row_number())

pca_heal = prcomp(num_heal, scale. = T)

ggbiplot(pca_heal, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_heal$Trtmt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp_heal$Material))+
  #labs(title = "Healy only")+
  #scale_color_manual(values = PNWColors::pnw_palette("Moth", 4))+
  ylim(-7,7)+
  xlim(-7,7)+
  theme_er()+
  NULL

a = ggbiplot(pca_heal, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_heal$Trtmt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups))+
  #scale_color_manual(values = PNWColors::pnw_palette("Moth", 4))+
  ylim(-7,7)+
  xlim(-7,7)+
  theme_er()+
  NULL


#
## 3e. TOOL only ---------------------------------------------------------


num_tool = 
  relabund_wide %>% 
  filter(Site == "TOOL") %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) 

grp_tool = 
  relabund_wide %>% 
  filter(Site == "TOOL") %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                Site,Trtmt, Material) %>% 
  mutate(Trtmt = recode(Trtmt, "FTC" = "freeze-thaw",
                                 "CON" = "control")) %>% 
  dplyr::mutate(row = row_number())

pca_tool = prcomp(num_tool, scale. = T)

ggbiplot(pca_tool, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_tool$Trtmt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups, shape = grp_tool$Material))+
  #labs(title = "Toolik only")+
  ylim(-7,7)+
  xlim(-7,7)+
  #scale_color_manual(values = PNWColors::pnw_palette("Winter", 2))+
  theme_er()+
  NULL



b = ggbiplot(pca_tool, obs.scale = 1, var.scale = 1,
         groups = as.character(grp_tool$Trtmt), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=3,stroke=1, aes(color = groups))+
  #labs(title = "Toolik only")+
  ylim(-7,7)+
  xlim(-7,7)+
  #scale_color_manual(values = PNWColors::pnw_palette("Winter", 2))+
  theme_er()+
  NULL


library(cowplot)
library(patchwork)

a + b + plot_layout()+
  theme_er()  # sets a common legend 
#
# PERMANOVA ---------------------------------------------------------------
## use relabund_wide for PERMANOVA
## general format is: [all numeric columns] ~ [independent variables]
## broom::tidy summarizes the info from the model as a dataframe

## P-value tells you if a factor is significant
## R2 tells you the relative contribution of the factor to total variation
## e.g. R2 = 0.115 means "Material" accounted for 11.5 % of total variation

library(vegan)
# permanova_fticr_all = 
  adonis(relabund_wide %>% select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) ~ 
         (Site*Trtmt*Material), 
       data = relabund_wide) 
  

relabund_wide_mineral_only =
  relabund_wide %>% 
  filter(Material != "Organic")


adonis(relabund_wide_mineral_only %>% select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) ~ 
         (Site*Trtmt*Material), 
       data = relabund_wide_mineral_only) 
  
relabund_wide_controlonly =
    relabund_wide %>% 
    filter(Trtmt == "CON")
  
  
adonis(relabund_wide2 %>% select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) ~ 
           (Site*Material), 
         data = relabund_wide2) 
  
  # adonis(relabund_wideHEAL %>% select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) ~ 
  #          (Trtmt), 
  #        data = relabund_wideHEAL) 
  # 
  # adonis(relabund_wideTOOL %>% select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) ~ 
  #          (Trtmt), 
  #        data = relabund_wideTOOL) 

# b = broom::tidy(permanova_fticr_all$aov.tab)
  
  
# bray distance ----------------
  
  library(ape)
  
  bray_distance = vegdist(num_con, method="euclidean")
  principal_coordinates = pcoa(bray_distance)
  pcoa_plot = data.frame(principal_coordinates$vectors[,])
  pcoa_plot_merged = merge(pcoa_plot, grp_con, by="row.names")

  ####### Calculate percent variation explained by PC1, PC2
  
  PC1 <- 100*(principal_coordinates$values$Eigenvalues[1]/sum(principal_coordinates$values$Eigenvalues))
  PC2 <- 100*(principal_coordinates$values$Eigenvalues[2]/sum(principal_coordinates$values$Eigenvalues))
  PC3 <- 100*(principal_coordinates$values$Eigenvalues[3]/sum(principal_coordinates$values$Eigenvalues))

#everything below this point is a mess.
  
  grp = 
    relabund_wide_controlonly %>% 
    dplyr::mutate(grp = paste0(Site,"-", Material))
  #dplyr::select(row, grp)
  matrix = as.matrix(bray_distance)   

  matrix2 = 
    matrix %>% 
    melt() %>% 
    left_join(grp, by = c("Var1"="row")) %>% 
    #rename(grp1 = grp) %>% 
    left_join(grp, by = c("Var2"="row")) %>% 
    filter(grp.x==grp.y) %>% 
    group_by(grp.x,grp.y,sat_level.x, texture.x,treatment.x,treatment.y) %>% 
    dplyr::summarise(distance  =mean(value)) %>%
    ungroup %>% 
    dplyr::rename(sat_level = sat_level.x) %>% 
    dplyr::mutate(sat_level = if_else(treatment.x=="FM","FM",sat_level),
                  sat_level = factor(sat_level, levels = c(5,35,50,75,100,"FM")))    