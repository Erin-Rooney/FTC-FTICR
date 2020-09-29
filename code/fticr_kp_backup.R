# Sep-24-2020
# FTICR

# Load libraries-------------------------------
library(tidyverse)
library(reshape2)
library(soilpalettes)
theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

# Load data------------------------------------
report_water = read.csv("processed/Lybrand Alaska Sept 2019 Report_Colorcoded.csv")
report_chcl3 = read.csv("processed/Lybrand Alaska CHCl3 Sept 2019 Report_Colorcoded.csv")

# core key for fticr ------------------------------------------------------

fticr_key = read.csv("processed/fticr_ms_ftmetadata.csv")
fticr_reps = 
  fticr_key %>% 
  # remove extra spaces in Site names
  mutate(Site = str_replace(Site, " ", "")) %>% 
  # calculate reps per treatment grouping
  group_by(Site, Material, Trtmt) %>% 
  dplyr::mutate(reps = n()) %>% 
  # separate ID column into many
  separate(ID, sep = " ", into = c("FT", "ID")) %>% 
  # keep only the necessary columns
  select(ID, Site, Trtmt, Material, reps)



# Assemble reports WATER------------------------------
fticr_report_water = 
  report_water %>% 
  rename(Mass=`ï..Mass`) %>% 
  # filter appropriate mass range
  filter(Mass>200 & Mass<900) %>% 
  # remove isotopes
  filter(C13==0) %>% 
  # remove peaks without C assignment
  filter(C>0)


fticr_meta_water = 
  fticr_report_water %>% 
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
meta_hcoc_water = 
  fticr_meta_water %>% 
  dplyr::select(Mass, formula, HC, OC)

fticr_data_water = 
  fticr_report_water %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(-c(C:Candidates)) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(Mass,starts_with("FT")) %>% 
  melt(id = c("Mass"), value.name = "presence", variable.name = "CoreID") %>% 
  # convert intensities to presence==1/absence==0  
  dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
  # keep only peaks present
  filter(presence>0) %>% 
  left_join(dplyr::select(fticr_meta_water, Mass,formula), by = "Mass")  %>% 
  #left_join(corekey, by = "CoreID") %>% 
  # rearrange columns
  dplyr::select(-Mass,-formula, -presence,Mass,formula,presence) %>% 
  # separate COREID for easy left_join
  separate(CoreID, sep = "_", into = c("FT_col", "ID", "W")) %>% 
  # filter only FT
  # filter(FT_col == "FT")
  filter(FT_col %in% "FT") %>% 
  left_join(fticr_reps, by = "ID") %>% 
  rename(max_reps = reps) %>% 
  group_by(Site, Material, Trtmt, formula) %>% 
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

meta_formula_water = 
  fticr_meta_water %>% 
  dplyr::select(Mass, formula) %>% 
  group_by(formula) %>% 
  dplyr::mutate(n = n())


# Assemble reports chcl3--------------------------------
fticr_report_chcl3 =
  report_chcl3 %>%
  rename(Mass=`ï..Mass`) %>%
  filter(Mass>200 & Mass<900) %>%
  filter(C13==0) %>%
  filter(C>0)

# split report into meta and data

fticr_meta_chcl3 = 
  fticr_report_chcl3 %>% 
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
meta_hcoc_chcl3 = 
  fticr_meta_chcl3 %>% 
  dplyr::select(Mass, formula, HC, OC)


fticr_data_chcl3 = 
  fticr_report_chcl3 %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(-c(C:Candidates)) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(Mass,starts_with("FT")) %>% 
  melt(id = c("Mass"), value.name = "presence", variable.name = "CoreID") %>% 
  # convert intensities to presence==1/absence==0  
  dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
  # keep only peaks present
  filter(presence>0) %>% 
  left_join(dplyr::select(fticr_meta_chcl3, Mass,formula), by = "Mass")  %>% 
  #left_join(corekey, by = "CoreID") %>% 
  # rearrange columns
  dplyr::select(-Mass,-formula, -presence,Mass,formula,presence) %>% 
  separate(CoreID, sep = "_", into = c("FT_col", "ID", "W")) %>% 
  # filter only FT
  # filter(FT_col == "FT")
  filter(FT_col %in% "FT") %>% 
  left_join(fticr_reps, by = "ID") %>% 
  rename(max_reps = reps) %>% 
  group_by(Site, Material, Trtmt, formula) %>% 
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
  
  
  meta_formula_chcl3 = 
    fticr_meta_chcl3 %>% 
    dplyr::select(Mass, formula) %>% 
    group_by(formula) %>% 
    dplyr::mutate(n = n())

# Write reports csv output----------------------------------

## OUTPUTS
write.csv(fticr_data_water,"fticr_data_water.csv", row.names = FALSE)
write.csv(fticr_meta_water,"fticr_meta_water.csv", row.names = FALSE)
write.csv(meta_hcoc_water,"fticr_meta_hcoc_water.csv", row.names = FALSE)

write.csv(fticr_data_chcl3,"fticr_data_chcl3.csv", row.names = FALSE)
write.csv(fticr_meta_chcl3,"fticr_meta_chcl3.csv", row.names = FALSE)
write.csv(meta_hcoc_chcl3,"fticr_meta_hcoc_chcl3.csv", row.names = FALSE)

# Load files-----------------------------------

fticr_data_water = read.csv("fticr_data_water.csv")
fticr_meta_water = read.csv("fticr_meta_water.csv")
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)

fticr_data_chcl3 = read.csv("fticr_data_chcl3.csv")
fticr_meta_chcl3 = read.csv("fticr_meta_chcl3.csv")
meta_hcoc_chcl3  = read.csv("fticr_meta_hcoc_chcl3.csv") %>% select(-Mass)

# NOSC and AImod plots_water-------------------------------


fticr_meta_nosc_water =
fticr_meta_water %>% 
  select(NOSC, formula, Mass, Class, AImod, HC, OC)

fticr_water = 
  fticr_data_water %>% 
  select(ID, formula, Site, Trtmt, Material) 

fticr_water_nosc_trt = 
  fticr_water %>% 
  distinct(Site, Trtmt, Material, formula) %>% 
  left_join(fticr_meta_nosc_water, by = "formula")

fticr_water_nosc_trt = fticr_water_nosc_trt %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

#NOSC

ggplot(fticr_water_nosc_trt, aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(binwidth = 0.05) +
  facet_grid(Material ~ Site) +
  theme_er() +
  scale_color_manual (values = soil_palette("gley", 2)) +
  ggtitle("NOSC, Water Extracted by Treatment")

  
ggplot(fticr_water_nosc_trt, aes(NOSC, color = Site, fill = Site)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.05) +
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = soil_palette("redox", 2)) +
  scale_color_manual(values = soil_palette("redox", 2)) + 
  ggtitle("NOSC, Water Extracted by Site")

ggplot(fticr_water_nosc_trt, aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.05) +
  facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual (values = soil_palette("eutrostox", 2)) +
  scale_color_manual(values = soil_palette("eutrostox", 2)) +
  ggtitle("NOSC, Water Extracted by Treatment")

ggplot(fticr_water_nosc_trt, aes(NOSC, color = Trtmt, fill = Trtmt)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.05) +
  facet_grid(Material ~ Site) +
  theme_er() +
  scale_color_manual (values = soil_palette("redox", 2)) +
  scale_fill_manual (values = soil_palette("redox", 2)) +
  ggtitle("NOSC, Water Extracted")

ggplot(fticr_water_nosc_trt, aes(NOSC, color = Site))
  geom_histogram(aes(y = stat(count)/sum(count))) +
    scale_y_continuous(labels = scales::percent)
  
ggplot(fticr_water_nosc_trt, aes(x = NOSC, color = Site, fill = Site))+
    geom_histogram(alpha = 0.5, position = "identity")+
    facet_grid(Material~.) + 
    theme_er() +
    scale_fill_color (values = soil_palette("gley", 2)) +
    scale_color_manual (values = soil_palette("gley", 2)) +
    ggtitle("NOSC, Water Extracted")


# AImod-------------------------------------------
soil_aromatic =
  fticr_water_nosc_trt %>% 
  dplyr::select(formula, Site, Trtmt, Material, Mass, AImod, HC, OC)



soil_aromatic = soil_aromatic %>% 
  dplyr::mutate(aromatic_col = case_when(AImod>0.5 ~ "aromatic",
                                     (HC<2.0 & HC>1.5) ~ "aliphatic")) 
  soil_aromatic

soil_aromatic %>%
  drop_na %>% 
  group_by(Site, Trtmt, Material, aromatic_col) %>% 
  dplyr::summarize(counts = n())->
  soil_aromatic_counts

#ggplots

ggplot(soil_aromatic_counts, aes(x=Site, y=counts, fill=Trtmt)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("podzol", 4)) +
  facet_grid(Material ~.)

ggplot(soil_aromatic_counts, aes(x=Site, y=counts, fill=aromatic_col)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("alaquod", 4)) +
  facet_grid(Material ~ .)


# stats for aromatic peaks



# van krevelen plots_water------------------------------------------------------
fticr_water = 
  fticr_data_water %>% 
  select(ID, formula, Site, Trtmt, Material) 

fticr_water_trt = 
  fticr_water %>% 
  distinct(Site, Trtmt, Material, formula) %>% 
  left_join(meta_hcoc_water, by = "formula")

fticr_water_trt = fticr_water_trt %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

fticr_water_trt %>% 
  ggplot(aes(x=OC, y=HC, color = Trtmt))+
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

fticr_water_trt %>% 
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

# van krevelen plots_chcl3------------------------------------------------------

fticr_chcl3 = 
  fticr_data_chcl3 %>% 
  select(ID, formula, Site, Trtmt, Material) 

fticr_chcl3_trt = 
  fticr_chcl3 %>% 
  distinct(Site, Trtmt, Material, formula) %>% 
  left_join(meta_hcoc_chcl3, by = "formula")

fticr_chcl3_trt = fticr_chcl3_trt %>% 
  mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

fticr_chcl3_trt %>% 
  ggplot(aes(x=OC, y=HC, color = Trtmt))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  stat_ellipse()+
  facet_grid(Material ~ Site)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("CHCl3 extracted FTICR-MS")+
  theme_er()
  #scale_color_manual (values = soil_palette("gley", 2))

fticr_chcl3_trt %>% 
  ggplot(aes(x=OC, y=HC, color = Site))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  stat_ellipse()+
  facet_grid(Material ~.)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  ggtitle("CHCl3 extracted FTICR-MS")+
  theme_er() +
  scale_color_manual (values = soil_palette("redox", 2))

fticr_chcl3_trt %>% 
  ggplot(aes(x=OC, y=HC, color = Site))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  facet_grid(Material ~ Trtmt)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  theme_bw()
