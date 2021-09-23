# Feb-24-2021
# FTICR
# Water Analysis
# Relabund

#load packages
source("code/FTICR-0-packages.R")

# 1. load files -----------------------------------------------------------

fticr_data_water = read.csv("processed/fticr_data_water.csv") %>% select(ID, formula, Site, Trtmt, Material) 
fticr_meta_water = read.csv("processed/fticr_meta_water.csv")
#meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)

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


fticr_water_relabund_summarized3 = 
  fticr_water_relabund %>% 
  group_by(Site, Trtmt, Class) %>% 
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

## potential idea: do ANOVA with x = site, and report p-values in the graph
## then do ANOVA with x = trtmt for each site, and report sig. as asterisks

# stats for aromatic peaks

# relabund bar plots ----
# bar graph
fticr_water_relabund_summarized %>%
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy"),
         Trtmt = recode(Trtmt, "CON" = "control",
                        "FTC" = "freeze-thaw cycles")) %>% 
  ggplot(aes(x = Trtmt, y = relabundance))+
  labs(x = " ",
       y = "relative abundance, %")+
  geom_bar(aes(fill = Class), stat = "identity")+
  scale_fill_manual(values = rev(pnw_palette("Shuksan",4)))+
  facet_grid(Material ~ Site)+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL


fticr_water_relabund_summarized3 %>%
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy"),
         Trtmt = recode(Trtmt, "CON" = "control",
                        "FTC" = "freeze-thaw cycles")) %>%  
  ggplot(aes(x = Trtmt, y = relabundance))+
  labs(x = " ",
       y = "relative abundance, %")+
  geom_bar(aes(fill = Class), stat = "identity")+
  scale_fill_manual(values = rev(pnw_palette("Shuksan",4)))+
  facet_grid(. ~ Site)+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  theme(legend.position = "bottom")+
  NULL

# bar graph_control only
fticr_water_relabund_summarized %>% 
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy")) %>% 
  filter(Trtmt == "CON") %>% 
  ggplot(aes(x = Site, y = relabundance))+
  geom_bar(aes(fill = Class), stat = "identity")+
  scale_fill_manual(values = rev(pnw_palette("Sunset",4)))+
  facet_grid(Material ~ .)+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  theme(legend.position = "right")+
  NULL



# run anova to get statistical significance
# then use that to create the label file below

label = tribble(
  ~Site, ~Trtmt, ~Material, ~y, ~label,
  ### THIS IS ONLY AN EXAMPLE 
  "HEAL", "FTC", "Lower Mineral", 50, "*"
) %>% 
  mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))


#
# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

relabund_table = 
  fticr_water_relabund_summarized %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

relabund_table3 = 
  fticr_water_relabund_summarized3 %>%
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class

fit_aov = function(dat){
    
    aov(relabund ~ Trtmt, data = dat) %>% 
      broom::tidy() %>% # convert to clean dataframe
      rename(pvalue = `p.value`) %>% 
      filter(term == "Trtmt") %>% 
      mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    mutate(Trtmt = "FTC")   
  
  }
  
fit_aov2 = function(dat){
  
  aov(relabund ~ Site, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "Site") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk)  # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
       
  
}


fit_aov3 = function(dat){
  
  aov(relabund ~ Trtmt, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "Trtmt") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>%  # we need only the asterisk column
  # two steps below, we need to left-join. 
  # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    mutate(Trtmt = "FTC")   
  
  
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

relabund_asterisk2 = 
  fticr_water_relabund %>% 
  filter(Trtmt == "CON") %>% 
  group_by(Material, Class) %>% 
  do(fit_aov2(.))

relabund_asterisk = 
  fticr_water_relabund %>% 
  group_by(Site, Material, Class) %>% 
  do(fit_aov(.))

#hypothesis 1

relabund_asterisk3 = 
  fticr_water_relabund %>% 
  group_by(Site, Class) %>% 
  do(fit_aov3(.))

## step 4: combine the summarized values with the asterisks
relabund_table_with_asterisk = 
  relabund_table %>% 
  left_join(relabund_asterisk) %>%
  # combine the values with the asterisk notation
  mutate(value = paste(summary, asterisk),
  # this will also add " NA" for the blank cells
  # use str_remove to remove the string
         value = str_remove(value, " NA")) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "Trtmt", values_from = "value")



relabund_table_with_asterisk2 = 
  relabund_table %>% 
  left_join(relabund_asterisk2) %>%
  # combine the values with the asterisk notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         value = str_remove(value, " NA")) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "Site", values_from = "value")


#hypothesis 1

relabund_table_with_asterisk3 = 
  relabund_table3 %>% 
  left_join(relabund_asterisk3) %>%
  # combine the values with the asterisk notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         value = str_remove(value, " NA")) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "Site", values_from = "value")

relabund_table_with_asterisk2 %>% knitr::kable() # prints a somewhat clean table in the console

#hypothesis 1

relabund_table_with_asterisk3 %>% knitr::kable() # prints a somewhat clean table in the console



relabund_table_with_asterisk %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_table_with_asterisk, "output/trtmt_aovstats.csv", row.names = FALSE)

write.csv(relabund_table_with_asterisk2, "output/site_aovstats.csv", row.names = FALSE)

write.csv(relabund_table_with_asterisk3, "output/siteandtrtmt_aovstats.csv", row.names = FALSE)


## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class

fit_aov_site = function(dat){
  
 aov(relabund ~ Site, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "Site") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    mutate(Site = "HEAL") %>% 
    identity()
  
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

relabund_asterisk_sitecon = 
  fticr_water_relabund %>% 
  filter(Trtmt == "CON") %>% 
  group_by(Material, Class) %>% 
  do(fit_aov_site(.))

## step 4: combine the summarized values with the asterisks
relabund_table_with_asterisk_sitecon = 
  relabund_table %>% 
  left_join(relabund_asterisk_sitecon) %>%
  # combine the values with the asterisk notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         value = str_remove(value, " NA")) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "Site", values_from = "value")

relabund_table_with_asterisk_sitecon %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_table_with_asterisk_sitecon, "output/site_aovstats.csv", row.names = FALSE)

