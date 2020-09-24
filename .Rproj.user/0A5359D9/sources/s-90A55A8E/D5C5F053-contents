# Sep-24-2020
# FTICR

# Load libraries-------------------------------
library(tidyverse)
library(reshape2)

# Load data------------------------------------
report = read.csv("processed/Lybrand Alaska Sept 2019 Report_Colorcoded.csv")
report2 = read.csv("processed/Lybrand Alaska CHCl3 Sept 2019 Report_Colorcoded.csv")

# Assemble reports------------------------------
fticr_report = 
  report %>% 
  # filter appropriate mass range
  filter(Mass>200 & Mass<900) %>% 
  # remove isotopes
  filter(C13==0) %>% 
  # remove peaks without C assignment
  filter(C>0)

fticr_report2 =
  report %>%
  filter(Mass>200 & Mass<900) %>%
  filter(C13==0) %>%
  filter(C>0)

# split report into meta and data

fticr_meta = 
  fticr_report %>% 
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
meta_hcoc = 
  fticr_meta %>% 
  dplyr::select(Mass, formula, HC, OC)


fticr_data = 
  fticr_report %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(-c(C:Candidates)) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(Mass,starts_with("FT")) %>% 
  melt(id = c("Mass"), value.name = "presence", variable.name = "CoreID") %>% 
  # convert intensities to presence==1/absence==0  
  dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
  # keep only peaks present
  filter(presence>0) %>% 
  left_join(dplyr::select(fticr_meta, Mass,formula), by = "Mass")  %>% 
  #left_join(corekey, by = "CoreID") %>% 
  # rearrange columns
  dplyr::select(-Mass,-formula, -presence,Mass,formula,presence) %>% 
  # now we want only peaks that are in 3 of the 5 replicates
  # group by the treatment levels  
  # group_by(treatment, sat_level,formula) %>% 
  # dplyr::mutate(n = n(),
  #              presence = mean(presence)) %>% 
  # filter(n>2) 
  
  
  
  meta_formula = 
  fticr_meta %>% 
  dplyr::select(Mass, formula) %>% 
  group_by(formula) %>% 
  dplyr::mutate(n = n())

# Write reports csv output----------------------------------

## OUTPUTS
write.csv(fticr_data,"fticr_data.csv", row.names = FALSE)
write.csv(fticr_meta,"fticr_meta.csv", row.names = FALSE)
write.csv(meta_hcoc,"fticr_meta_hcoc.csv", row.names = FALSE)

# Load files-----------------------------------

fticr_data = read.csv("processed/fticr_data.csv")
fticr_meta = read.csv("processed/fticr_meta.csv")
meta_hcoc  = read.csv("processed/fticr_meta_hcoc.csv")

# Relative Abundance------------------------------


