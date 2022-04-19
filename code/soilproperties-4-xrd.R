#xrd data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

xrd_data = read.csv("processed/xrd_data_fticr.csv") 

# 2. Process data---------------------------------
# remove n and separate sample IDs into multiple columns
# grepl for canopy and slope columns
# LDC and LDA are typos from xrd analysis input, should be LOA and LOC, fixed with recode

xrd_deselect =
  xrd_data %>% 
  select(-c(rwp, feldspar)) 

xrd_data_processed =
  xrd_deselect %>% 
  # mutate(quartz_stdev = stringi::stri_replace_all_fixed(quartz_stdev, "ñ",""),
  #        albite_stdev = stringi::stri_replace_all_fixed(albite_stdev, "ñ",""),
  #        #anorthite_stdev = stringi::stri_replace_all_fixed(anorthite_stdev, "ñ",""),
  #        microcline_stdev = stringi::stri_replace_all_fixed(microcline_stdev, "ñ",""),
  #        chlorite_stdev = stringi::stri_replace_all_fixed(chlorite_stdev, "ñ",""),
  #        mica_stdev = stringi::stri_replace_all_fixed(mica_stdev, "ñ",""),
  #        kaolinite_stdev = stringi::stri_replace_all_fixed(kaolinite_stdev, "ñ",""),
  #        hornblende_stdev = stringi::stri_replace_all_fixed(hornblende_stdev, "ñ","")) %>% 
  separate(sample, sep = " ", into = c("siterep", "depth")) %>% 
  separate(depth, sep = "-", into = c("upperdepth", "lowerdepth")) %>% 
  dplyr::mutate(site = case_when(grepl("H", siterep)~"Healy",
                                     grepl("T", siterep)~"Toolik"),
                rep = case_when(grepl("1", siterep)~"1",
                                      grepl("2", siterep)~"2",
                                      grepl("3", siterep)~"3")) 

# 3. Analyse data and figures

xrd_data_tableanalysis =
  xrd_data_processed %>% 
  # dplyr::mutate(quartz = as.numeric(quartz),
  #               albite = as.numeric(albite),
  #               kaolinite = as.numeric(kaolinite),
  #               microcline = as.numeric(microcline),
  #               chlorite = as.numeric(chlorite),
  #               mica = as.numeric(mica),
  #               hornblende = as.numeric(hornblende),
  #               ankerite = as.numeric(ankerite),
  #               rwp = as.numeric(rwp),
  #               feldspar_decimal = as.numeric(feldspar_decimal),
  # ) %>% 
  #mutate(feldspar = (feldspar_decimal * 100)) %>% 
  # select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev,
  #           chlorite_stdev, mica_stdev, hornblende_stdev, ankerite_stdev, feldspar_decimal)) %>% 
  pivot_longer(cols = c(quartz, albite, kaolinite, microcline, chlorite, mica, hornblende,
                        ), names_to = "mineral", values_to = "abundance") %>% 
  #group_by(slopepos, covertype, morph) %>% 
  group_by(site, material, mineral) %>% 
  dplyr::summarize(mean = round(mean(abundance), 3),
                   se = round(sd(abundance)/sqrt(n()),3)) %>% 
  mutate(mean = mean*100,
         se = se*100) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  na.omit() 

#dplyr::select(-mean, -se)


xrd_stats = 
  xrd_data_processed %>% 
  filter(material != "organic") %>% 
  dplyr::mutate(quartz = as.numeric(quartz),
                albite = as.numeric(albite),
                kaolinite = as.numeric(kaolinite),
                microcline = as.numeric(microcline),
                chlorite = as.numeric(chlorite),
                mica = as.numeric(mica),
                hornblende = as.numeric(hornblende),
  ) %>% 
  #select(-c(rwp)) %>% 
  #mutate(feldspar = (feldspar_decimal * 100)) %>% 
  # select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev,
  #           chlorite_stdev, mica_stdev, hornblende_stdev, ankerite_stdev, feldspar_decimal)) %>% 
  pivot_longer(cols = c(quartz, albite, kaolinite, microcline, chlorite, mica, hornblende,
                        ), names_to = "mineral", values_to = "abundance") %>% 
  na.omit() %>% 
  select(-c(siterep, upperdepth, lowerdepth, rep)) %>% 
  ungroup()


xrd_forfig =
xrd_data_tableanalysis %>% 
  # mutate(slopepos = recode(slopepos, "low_backslope" = 'low backslope')) %>% 
  mutate(material = factor(material, levels = c("organic", "upper mineral", "lower mineral"))) %>%
  filter(material != "organic") 

xrd_data_tableanalysis %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_data_tableanalysis, "output/xrd_data_tableanalysis.csv", row.names = FALSE)

xrd_sitefig = 
  xrd_forfig %>% 
  filter(mineral != "feldspar") %>% 
  ggplot(aes(x = mineral, y = mean, fill = site))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=(mean-se/2),ymax=(mean+se/2)),width=.2,position=position_dodge(.9))+
  coord_flip() +
  labs(y = "abundance",
       x = "")+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 2)))+
  theme_er()+
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
  )+
  facet_grid(.~material)+
  NULL


# xrd_cover =
#   xrd_data_tableanalysis %>% 
#   mutate(slopepos = recode(slopepos, "low_backslope" = 'low backslope')) %>% 
#   mutate(slopepos = factor(slopepos, levels = c("backslope", "low backslope", "footslope"))) %>%
#   ggplot(aes(x = mineral, y = mean, fill = slopepos))+
#   geom_bar(stat = "identity", position = position_dodge())+
#   geom_errorbar(aes(ymin=(mean-se/2),ymax=(mean+se/2)),width=.2,position=position_dodge(.9))+
#   coord_flip() +
#   labs(y = "abundance",
#        x = "")+
#   scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
#   theme_er()+
#   theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
#   )+
#   facet_grid(.~covertype)+
#   NULL

ggsave("output/xrd_site.tiff", plot = xrd_sitefig, height = 6, width = 10)
ggsave("output/xrd_site.jpeg", plot = xrd_sitefig, height = 6, width = 10)
ggsave("output/xrd_slope.tiff", plot = xrd_slope, height = 6, width = 10)
ggsave("output/xrd_slope.jpeg", plot = xrd_slope, height = 6, width = 10)

# xrd_data_processed %>% 
#   ggplot(aes(y = quartz, x = covertype, fill = covertype))+
#   geom_col(horizontal = TRUE, width = 0.7)+
#   scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 3)))+
#   theme_er()+
#   theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA) 
#   )+
#   coord_flip() +
#   facet_grid(.~slopepos)+
#   NULL


# xrd_data_processed %>% 
#     ggplot(aes(y = quartz, x = covertype, fill = slopepos))+
#     geom_col(width = 0.7)+
#     scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 3)))+
#     theme_er()+
#     theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
#     )+
#     NULL

# 5. XRD stats ------------------------

library(nlme)
library(agricolae)

#-----------------------------
quartz_up = aov(abundance ~ site, data = xrd_stats %>% filter(material %in% "upper mineral" & 
                                                      mineral %in% "quartz"))
summary(quartz_up)
kaolinite_up = aov(abundance ~ site, data = xrd_stats %>% filter(material %in% "upper mineral" & 
                                                                mineral %in% "kaolinite"))
summary(kaolinite_up)
chlorite_up = aov(abundance ~ site, data = xrd_stats %>% filter(material %in% "upper mineral" & 
                                                                mineral %in% "chlorite"))
summary(chlorite_up)
chlorite_low = aov(abundance ~ site, data = xrd_stats %>% filter(material %in% "lower mineral" & 
                                                                  mineral %in% "chlorite"))
summary(chlorite_low)
#------------------------------

quartz = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "quartz"))
summary(quartz)

kaolinite = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "kaolinite"))
summary(kaolinite)

chlorite = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "chlorite"))
summary(chlorite)

albite = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "albite"))
summary(albite)

microcline = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "microcline"))
summary(microcline)

mica = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "mica"))
summary(mica)

hornblende = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "hornblende"))
summary(hornblende)

feldspar = aov(abundance ~ site*material, data = xrd_stats %>% filter(mineral %in% "microcline"))
summary(feldspar)


fit_aov = function(dat){
  
  aov(abundance ~ site, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "site") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    force()
  
}

# fit_hsd = function(dat){
#   a = aov(abundance ~ site, data = dat)
#   h = HSD.test(a, "site")
#   h$groups %>% mutate(site = row.names(.)) %>% 
#     rename(label = site) %>%  
#     dplyr::select(site, label)
# }

## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

xrd_aov_all = 
  xrd_stats %>% 
  group_by(material, mineral) %>% 
  do(fit_aov(.))


xrd_table_with_aov_site = 
  xrd_data_tableanalysis %>% 
  left_join(xrd_aov_all) %>%
  # combine the values with the label notation
  mutate(value1 = paste(summary, asterisk)
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -label) %>% 
  pivot_wider(names_from = "site", values_from = "value") %>% 
  force()

xrd_table_with_hsd_site %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_table_with_hsd_site, "output/xrd_table_with_hsd_site.csv", row.names = FALSE)

xrd_hsd_all %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_hsd_all, "output/xrd_hsd_all.csv", row.names = FALSE)

######################



# cover type comparison

fit_aov = function(dat){
  
  aov(abundance ~ covertype, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "covertype") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    #mutate(cover_type = "Open")   
    force()
  
}


fit_hsd = function(dat){
  a = aov(abundance ~ covertype, data = dat)
  h = HSD.test(a, "covertype")
  h$groups %>% mutate(covertype = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(covertype, label)
}

## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

xrd_hsd_covertype = 
  xrd_stats %>% 
  #filter(cover_type == "Open") %>% 
  group_by(slopepos, mineral) %>% 
  do(fit_hsd(.))

# xrd_table_with_hsd_covertype = 
#   xrd_data_tableanalysis %>% 
#   left_join(xrd_hsd_all) %>%
#   # combine the values with the label notation
#   mutate(value = paste(summary, label),
#          # this will also add " NA" for the blank cells
#          # use str_remove to remove the string
#          #value = str_remove(value, " NA")
#   ) %>% 
#   dplyr::select(-summary, -label) %>% 
#   pivot_wider(names_from = "slopepos", values_from = "value") %>% 
#   force()

# xrd_table_with_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console
# 
# write.csv(xrd_table_with_hsd_covertype, "output/xrd_table_with_hsd_covertype.csv", row.names = FALSE)

xrd_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_hsd_covertype, "output/xrd_hsd_covertype.csv", row.names = FALSE)