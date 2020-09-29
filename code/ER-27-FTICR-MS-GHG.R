# Sept 28 2020
# FTICR MS Data

# Load libraries-------------------------------
library(tidyverse)
library(reshape2)

# Load data------------------------------------
report_water = read.csv("processed/Lybrand Alaska Sept 2019 Report_Colorcoded.csv")
report_chcl3 = read.csv("processed/Lybrand Alaska CHCl3 Sept 2019 Report_Colorcoded.csv")

# core key for fticr ------------------------------------------------------

fticr_key_ghg = read.csv("processed/fticr_ms_ghgmetadata.csv")
fticr_reps_ghg = 
  fticr_key_ghg %>% 
    rename(ID =`ï..ID`) %>%
    #remove extra spaces in Site names
    mutate(Site = str_replace(Site, " ", "")) %>% 
    # calculate reps per treatment grouping
    group_by(Site, Material, trmt) %>% 
    dplyr::mutate(reps = n()) %>% 
    # separate ID column into many
    separate(ID, sep = " ", into = c("FT", "ID")) %>% 
    # keep only the necessary columns
    select(ID, Site, trmt, Material, reps)

fticr_report_ghg_water = 
  report_water %>% 
  rename(Mass =`ï..Mass`) %>% 
  # filter appropriate mass range
  filter(Mass>200 & Mass<900) %>% 
  # remove isotopes
  filter(C13==0) %>% 
  # remove peaks without C assignment
  filter(C>0)


fticr_meta_ghg_water = 
  fticr_report_ghg_water %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(Mass:Candidates) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(-starts_with("FT")) %>% 
  # select only necessary columns
  dplyr::select(Mass, C, H, O, N, S, P, El_comp, Class) %>% 
  # create columns for indices
  dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                HC = round(H/C,2),
                OC = round(O/C,2)) %>% 
  # create column/s for formula
  # first, create columns for individual elements
  # then, combine
  dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                formula = str_replace_all(formula,"NA","")) %>% 
  dplyr::select(Mass, formula, El_comp, Class, HC, OC, AImod, NOSC, C:P)


# subset of meta for HC/OC only, for Van Krevelen diagrams
meta_hcoc_ghg_water = 
  fticr_meta_ghg_water %>% 
  dplyr::select(Mass, formula, HC, OC)

fticr_data_ghg_water = 
  fticr_report_ghg_water %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(-c(C:Candidates)) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(Mass,starts_with("FT")) %>% 
  melt(id = c("Mass"), value.name = "presence", variable.name = "CoreID") %>% 
  # convert intensities to presence==1/absence==0  
  dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
  # keep only peaks present
  filter(presence>0) %>% 
  left_join(dplyr::select(fticr_meta_ghg_water, Mass,formula), by = "Mass")  %>% 
  #left_join(corekey, by = "CoreID") %>% 
  # rearrange columns
  dplyr::select(-Mass,-formula, -presence,Mass,formula,presence) %>% 
  # separate COREID for easy left_join
  separate(CoreID, sep = "_", into = c("GHG_col", "ID", "W")) %>% 
  # filter only FT
  # filter(GHG_col == "GHG")
  filter(GHG_col %in% "GHG") %>% 
  left_join(fticr_reps_ghg, by = "ID") %>% 
  rename(max_reps = reps) %>% 
  group_by(Site, Material, trmt, formula) %>% 
  dplyr::mutate(formulareps = n()) %>% 
  # set up replication filter for 2/3 of max_rep
  ungroup() %>% 
  mutate(include = formulareps >= (2/3)*max_reps) %>% 
  
  ## mutate(include = formulareps > 1,
  ##        occurrence = case_when(formulareps == max_reps ~ "3/3",
  ##                               formulareps < max_reps & formulareps >= (2/3)*max_reps ~ "2/3+",
  ##                               formulareps >= (1/3)*max_reps ~ "1/3+",
  ##                               formulareps < (1/3)*max_reps ~ "exclude")) %>% 
  filter(include)




# now we want only peaks that are in 3 of the 5 replicates
# group by the treatment levels  
# group_by(treatment, sat_level,formula) %>% 
# dplyr::mutate(n = n(),
#              presence = mean(presence)) %>% 
# filter(n>2) 

meta_formula_ghg_water = 
  fticr_meta_ghg_water %>% 
  dplyr::select(Mass, formula) %>% 
  group_by(formula) %>% 
  dplyr::mutate(n = n())

# write csv output---------------------------------------------------------

write.csv(fticr_data_ghg_water,"fticr_data_ghg_water.csv", row.names = FALSE)
write.csv(fticr_meta_ghg_water,"fticr_meta_ghg_water.csv", row.names = FALSE)
write.csv(meta_hcoc_ghg_water,"fticr_meta_hcoc_ghg_water.csv", row.names = FALSE)

# load csv ouput-----------------------------------------------------------

fticr_data_ghg_water = read.csv("fticr_data_ghg_water.csv")
fticr_meta_ghg_water = read.csv("fticr_meta_ghg_water.csv")
meta_hcoc_ghg_water  = read.csv("fticr_meta_hcoc_ghg_water.csv") %>% select(-Mass)

# van krevelen plots_water------------------------------------------------------
fticr_ghg_water = 
  fticr_data_ghg_water %>% 
  select(ID, formula, Site, trmt, Material) 

fticr_ghg_water_trt = 
  fticr_ghg_water %>% 
  distinct(Site, trmt, Material, formula) %>% 
  left_join(meta_hcoc_ghg_water, by = "formula")

fticr_ghg_water_trt = fticr_ghg_water_trt %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

fticr_ghg_water_trt %>% 
  ggplot(aes(x=OC, y=HC, color = trmt))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  stat_ellipse()+
  facet_grid(Material ~ Site)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("Water extracted FTICR-MS")+
  theme_er()+
  scale_fill_manual (values = soil_palette("redox", 2))

fticr_ghg_water_trt %>% 
  ggplot(aes(x=OC, y=HC, color = Site))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  stat_ellipse()+
  facet_grid(Material ~.)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("Water extracted FTICR-MS")+
  theme_er() +
  scale_color_manual (values = soil_palette("redox", 2))

fticr_water_trt %>% 
  ggplot(aes(x=OC, y=HC, color = Site))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  facet_grid(Material ~ Trtmt)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  theme_bw()