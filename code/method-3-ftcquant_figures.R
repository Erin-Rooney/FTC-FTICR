#FTC Quant
#Erin C Rooney
#6 17 2021
# updated 7 29 2021

#trying to fix some issues and use
#for NSF postdoc proposal

# extract organic horizons
# connect depths to FT horizons

#KP Code Modified below

source("code/0-method-packages.R")



# Load data------------------------------------------



ftc_full = read.csv("raw/FTC_1.5_4.csv")


## 2.1 calculate mean ftc

# ftc_avg = 
#   ftc_full %>% 
#   filter(!season %in% "activelayer") %>% 
#   ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
#   ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
#   
#   # create a `total` ftc column for annual total ftc
#   group_by(site, year, core, depth_cm) %>% 
#   dplyr::mutate(total = sum(Def1)) %>% 
#   ungroup() %>% 
#   
#   # now, incorporate the `total` data into `season`
#   # spread the `season` columns, and then recombine with `total`
#   spread(season, Def1) %>% 
#   gather(season, Def1, total:winter) %>%
#   # many NAs were introduced when forcing wide-form. remove all rows containing NAs
#   na.omit() %>% 
#   
#   # now, calculate mean FTC per site/depth/season
#   group_by(site, depth_cm, season) %>% 
#   dplyr::summarise(ftc = as.integer(mean(Def1))) %>% 
#   ungroup() %>% 
#   
#   # create new columns for depth range
#   # create bins of 5 cm depth increments
#   mutate(depth_bins = cut_width(depth_cm, width = 5, center=2.5)) %>% 
#   # now clean up
#   # remove brackets of different types
#   # I normally use the `stringr` package, but that doesn't like open brackets
#   # so I use the `stringi` package for this. You'll have to install it first
#   mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]",""),
#          depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[",""),
#          depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
#   # now separate this into two different columns
#   separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
#   mutate(depth_start_cm = as.integer(depth_start_cm),
#          depth_stop_cm = as.integer(depth_stop_cm))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ftc_avg_depth = ftc_avg %>% 
#   mutate(depth = depth_stop_cm - depth_start_cm)
# 
# ftc_avg_depth %>% 
#   filter(!season %in% "total") %>% 
#   mutate(site = factor(site, levels = c("BARR", "TOOL", "BONA", "HEAL")),
#          site = recode(site, "BARR" = "Barrow",
#                        "TOOL" = "Toolik",
#                        "BONA" = "Caribou Poker",
#                        "HEAL" = "Healy")) %>% 
#   ggplot(aes(y = depth, x = season, fill = ftc))+
#   geom_bar(position = "stack", stat= "identity")+
#   facet_grid(.~site)+
#   scale_y_reverse()+
#   ylim(100,0)+
#   scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
#   theme_kp()+
#   theme(legend.position = "right")+
#   NULL
# 
# 
# ftc_avg_depth %>% 
#   filter(!season %in% "total") %>% 
#   mutate(site = factor(site, levels = c("BARR", 'TOOL', "BONA", "HEAL"))) %>%
#   mutate(site = recode(site, "BARR" = "Barrow",
#                        "TOOL" = "Toolik",
#                        "BONA" = "Caribou Poker",
#                        "HEAL" = "Healy")) %>%  
#   mutate(season = factor(season, levels = c("spring", 'summer', "winter", "fall"))) %>%
#   ggplot(aes(y = depth, x = season, fill = ftc))+
#   geom_bar(position = "stack", stat= "identity")+
#   labs(y = "depth, cm")+
#   facet_grid(.~site)+
#   scale_y_reverse()+
#   ylim(50,0)+
#   scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
#   theme_kp()


#########################

ftc_fulldat = ftc_full %>% 
  #filter(season != "activelayer") %>% 
  mutate(site = recode (site, "TOOL" = "Toolik", 
                        "HEAL" = "Healy",
                        "BARR" = "Utqiaġvik",
                        "BONA" = "Caribou Poker"))%>% 
  filter(!is.na(Def1)) %>%       
  mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer"))) %>% 
  mutate(site = factor(site, levels = c("Utqiaġvik", "Toolik", "Caribou Poker", "Healy"))) 




ftc_mean = 
  ftc_fulldat %>%
  filter(site == "Toolik") %>% 
  #filter(mag.vec == 1.5 & duration == 4) %>% 
  mutate(depth_cm = depth_m*(-100)) %>% 
  #filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, year, core, depth_cm) %>% 
  dplyr::mutate(total = sum(Def1)) %>% 
  ungroup() %>% 
  
  # now, incorporate the `total` data into `season`
  # spread the `season` columns, and then recombine with `total`
  spread(season, Def1) %>% 
  gather(season, Def1, total:summer) %>%
  # many NAs were introduced when forcing wide-form. remove all rows containing NAs
  na.omit() %>% 
  
  # now, calculate mean FTC per site/depth/season
  # changed to sum and added in year in group by.
  group_by(site, depth_cm, season) %>% 
  dplyr::summarise(ftc = round(mean(Def1))) %>%
  ungroup() %>% 
  
  # bin top 10 cm into 5-cm bins, and the rest into 10-cm bins
  
  mutate(depth_bins1 = case_when(depth_cm <= 10 ~ cut_width(depth_cm, width = 5, center=2.5)),
         depth_bins2 = case_when(depth_cm > 10 ~ cut_width(depth_cm, width = 10, center=5)),
         depth_bins = paste0(depth_bins1, depth_bins2)) %>% 
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "NA","")) %>% 
  
  dplyr::select(-depth_bins1, -depth_bins2) %>% 
  
  # now clean up
  # remove brackets of different types
  # I normally use the `stringr` package, but that doesn't like open brackets
  # so I use the `stringi` package for this. You'll have to install it first
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]",""),
         depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[",""),
         depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(depth_start_cm = as.integer(depth_start_cm),
         depth_stop_cm = as.integer(depth_stop_cm)) %>% 
  mutate(depth2 = depth_stop_cm - depth_start_cm)

#Recode seasons as numbers

ftc_mean_seasnum = 
  ftc_mean %>% 
  mutate(season = factor(season)) %>% 
  mutate(seas_num = recode(season, "fall" = 1,
                           "winter" = 2,
                           "spring" = 3,
                           "summer" = 4,
                           "total" = 5))

#manuscript 

# library(nlme)
# l = lme(Def1 ~ site * depth_m, random = ~1|season, na.action = na.omit, data = ftc_fulldat) 
# anova(l)
# 
# lb = lme(Def1 ~ season, random = ~1|site, na.action = na.omit, data = ftc_fulldat) 
# anova(lb)


# ftc_mean_seasnum %>%
#   filter(seas_num < 5) %>% 
#   ggplot()+
#   geom_rect(aes(xmin = seas_num -0.4, xmax = seas_num + 0.4, 
#                 ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.numeric(ftc)))+
#   scale_y_reverse()+
#   scale_x_continuous(breaks = 1:4,
#                      labels = c("fall", "winter", "spring", "summer"))+
#   # annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
#   #          linetype = 2) +
#   scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
#   labs(
#     y = "depth, cm",
#     x = "season",  
#     fill = "freeze-thaw cycles, count")+
#   facet_grid(.~site)+
#   theme_kpnone()

#####################################3
####ftc max instead of mean-----------------

ftc_depth_bins = 
  ftc_fulldat %>%
  mutate(depth_cm = depth_m*(-100)) %>% 
  
  # bin top 10 cm into 5-cm bins, and the rest into 10-cm bins  
  mutate(depth_bins1 = case_when(depth_cm <= 10 ~ cut_width(depth_cm, width = 5, center=2.5)),
         depth_bins2 = case_when(depth_cm > 10 ~ cut_width(depth_cm, width = 10, center=5)),
         depth_bins = paste0(depth_bins1, depth_bins2),
         depth_bins = str_remove(depth_bins, "NA")) %>% 
  dplyr::select(-depth_bins1, -depth_bins2) %>% 
  
  # now clean up
  # remove brackets of different types
  mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]",""),
         depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[",""),
         depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
  # now separate this into two different columns
  separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm"))

ftc_max = 
  ftc_depth_bins %>% 
  group_by(site_pos, depth_start_cm, depth_stop_cm, season) %>% 
  dplyr::summarise(max_ftc = max(Def1))

# now add 0 where missing values

ftc_max_gapfilled = 
  ftc_max %>% 
  #filter(mag.vec == 1.5 & dur.vec == 4) %>% 
  mutate(depth = paste0(depth_start_cm, "-", depth_stop_cm)) %>% 
  ungroup() %>% 
  dplyr::select(-depth_start_cm, -depth_stop_cm) %>% 
  mutate_all(as.character) %>% 
  # pivot_wider(names_from = "site_pos", values_from = "max_ftc") %>% 
  # pivot_longer(-c(season, year, core, mag.vec, dur.vec, depth), names_to = "site_pos", values_to = "max_ftc") %>% 
  pivot_wider(names_from = "depth", values_from = "max_ftc") %>% 
  pivot_longer(-c(season, site_pos), names_to = "depth", values_to = "max_ftc") %>% 
  replace(is.na(.), "0") %>% 
  separate(depth, sep = "-", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  mutate(max_ftc = as.numeric(max_ftc),
         depth_start_cm = as.numeric(depth_start_cm),
         depth_stop_cm = as.numeric(depth_stop_cm)) %>% 
  # recode seasons
  mutate(season = factor(season)) %>% 
  mutate(seas_num = recode(season, "fall" = 1,
                           "winter" = 2,
                           "spring" = 3,
                           "summer" = 4,
                           "total" = 5))



ftc_max_nogapfill = 
  ftc_depth_bins %>% 
  group_by(site_pos, depth_start_cm, depth_stop_cm, season) %>% 
  dplyr::summarise(max_ftc = max(Def1)) %>% 
  mutate(max_ftc = as.numeric(max_ftc),
       depth_start_cm = as.numeric(depth_start_cm),
       depth_stop_cm = as.numeric(depth_stop_cm)) %>% 
  # recode seasons
  mutate(season = factor(season)) %>% 
  mutate(seas_num = recode(season, "fall" = 1,
                           "winter" = 2,
                           "spring" = 3,
                           "summer" = 4,
                           "total" = 5))


# ftc_max = 
#   ftc_fulldat %>%
#   #filter(mag.vec == 1 & duration == 24) %>% 
#   mutate(depth_cm = depth_m*(-100)) %>% 
#   #filter(!season %in% "activelayer") %>% 
#   ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
#   ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
#   
#   # create a `total` ftc column for annual total ftc
#   group_by(site, year, core, depth_cm) %>% 
#   dplyr::mutate(total = max(Def1)) %>% 
#   ungroup() %>% 
#   
#   # now, incorporate the `total` data into `season`
#   # spread the `season` columns, and then recombine with `total`
#   spread(season, Def1) %>% 
#   gather(season, Def1, total:summer) %>%
#   # many NAs were introduced when forcing wide-form. remove all rows containing NAs
#   na.omit() %>% 
#   
#   # now, calculate mean FTC per site/depth/season
#   # changed to sum and added in year in group by.
#   group_by(site, depth_cm, season) %>% 
#   dplyr::summarise(ftc = round(max(Def1))) %>%
#   ungroup() %>% 
#   
#   # bin top 10 cm into 5-cm bins, and the rest into 10-cm bins
#   
#   mutate(depth_bins1 = case_when(depth_cm <= 10 ~ cut_width(depth_cm, width = 5, center=2.5)),
#          depth_bins2 = case_when(depth_cm > 10 ~ cut_width(depth_cm, width = 10, center=5)),
#          depth_bins = paste0(depth_bins1, depth_bins2),
#          depth_bins = str_remove(depth_bins, "NA")) %>% 
#   dplyr::select(-depth_bins1, -depth_bins2) %>% 
#   
#   # now clean up
#   # remove brackets of different types
#   # I normally use the `stringr` package, but that doesn't like open brackets
#   # so I use the `stringi` package for this. You'll have to install it first
#   mutate(depth_bins = stringi::stri_replace_all_fixed(depth_bins, "]",""),
#          depth_bins = stringi::stri_replace_all_fixed(depth_bins, "[",""),
#          depth_bins = stringi::stri_replace_all_fixed(depth_bins, "(","")) %>% 
#   # now separate this into two different columns
#   separate(depth_bins, sep = ",", into = c("depth_start_cm", "depth_stop_cm")) %>% 
#   mutate(depth_start_cm = as.integer(depth_start_cm),
#          depth_stop_cm = as.integer(depth_stop_cm)) %>% 
#   mutate(depth2 = depth_stop_cm - depth_start_cm)

#Recode seasons as numbers

# ftc_max_NA =
#   ftc_max %>%
#   #pivot_wider(id_cols = site, depth_cm, depth_start_cm, depth_stop_cm, depth2) %>%
#   pivot_wider(id_cols = c(site, season, depth_start_cm, depth_stop_cm, depth2), names_from = depth_cm, values_from = ftc) %>% 
#   pivot_longer(cols = c(site, season, depth_start_cm, depth_stop_cm, depth2), names_to = depth_cm, values_to = ftc)
# 
# ftc_max_seasnum = 
#   ftc_max %>% 
#   mutate(season = factor(season)) %>% 
#   mutate(seas_num = recode(season, "fall" = 1,
#                            "winter" = 2,
#                            "spring" = 3,
#                            "summer" = 4,
#                            "total" = 5))

#

# ftc_max_nogapfill %>%
#   filter(seas_num < 5) %>% 
#   ggplot()+
#   geom_rect(aes(xmin = seas_num -0.4, xmax = seas_num + 0.4, 
#                 ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.numeric(max_ftc)))+
#   ylim(80,0)+
#   scale_x_continuous(breaks = 1:4,
#                      labels = c("fall", "winter", "spring", "summer"))+
#   # annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
#   #          linetype = 2) +
#   scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
#   labs(
#     y = "depth, cm",
#     x = "season",  
#     fill = "freeze-thaw cycles, count")+
#   facet_grid(.~site_pos)+
#   theme_kpnone()


ftc_max_gapfilled %>%
  mutate(site_pos = recode (site_pos, "TOOL" = "Toolik", 
                        "HEAL" = "Healy",
                        "BARR" = "Utqiaġvik",
                        "BONA" = "Caribou Poker"))%>% 
  #mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer"))) %>% 
  mutate(site_pos = factor(site_pos, levels = c("Utqiaġvik", "Toolik", "Caribou Poker", "Healy"))) %>% 
  filter(seas_num < 5) %>% 
  ggplot()+
  geom_rect(aes(xmin = seas_num -0.4, xmax = seas_num + 0.4, 
                ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.numeric(max_ftc)))+
  ylim(80,0)+
  scale_x_continuous(breaks = 1:4,
                     labels = c("fall", "winter", "spring", "summer"))+
  # annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
  #          linetype = 2) +
  scale_fill_gradientn(colors = (PNWColors::pnw_palette("Bay")))+  
  labs(
    y = "depth, cm",
    x = "season",  
    fill = "freeze-thaw cycles, count")+
  facet_grid(.~site_pos)+
  theme_kpnone()

#NEW COLOR SCHEME

ftc_max_gapfilled_2site =
  ftc_max_gapfilled %>% 
  filter(seas_num < 5 & site_pos != c("BARR", 'BONA')) %>%  
  mutate(site_pos = recode (site_pos, "TOOL" = "Toolik", 
                            "HEAL" = "Healy"),
         site_pos = factor(site_pos, levels = c("Utqiaġvik", "Toolik", "Caribou Poker", "Healy"))) %>% 
  na.omit()
  
  
ftc_max_gapfilled_2site %>%
  mutate(site_pos = recode (site_pos, "TOOL" = "Toolik", 
                            "HEAL" = "Healy",
                            "BARR" = "Utqiaġvik",
                            "BONA" = "Caribou Poker"))%>% 
  #mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer"))) %>% 
  ggplot()+
  geom_rect(aes(xmin = seas_num -0.4, xmax = seas_num + 0.4, 
                ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.numeric(max_ftc)))+
  ylim(80,0)+
  scale_x_continuous(breaks = 1:4,
                     labels = c("fall", "winter", "spring", "summer"))+
  scale_fill_gradientn(colors = rev(PNWColors::pnw_palette("Winter")))+
  labs(
    y = "depth, cm",
    x = "",  
    fill = "freeze-thaw cycles (count)")+
  facet_grid(.~site_pos)+
  theme_kpnone()+
  theme(legend.position = "bottom")





###

max_ftc_tabledata =
  ftc_max %>% 
  select(site, depth_start_cm, season, ftc) %>% 
  pivot_wider(id_cols = c(site, depth_start_cm), names_from = season, values_from = ftc) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(site = recode (site, "2" = "Toolik", 
                        "4" = "Healy",
                        "1" = "Utqiaġvik",
                        "3" = "Caribou Poker"))
  
  
knitr::kable(max_ftc_tabledata)


write.csv(max_ftc_tabledata, "processed/max_ftc_tabledata.csv", row.names = FALSE)
  


max_ftc_tabledata_wider =
  ftc_max %>% 
  mutate(site_pos = recode (site_pos, "2" = "Toolik", 
                        "4" = "Healy",
                        "1" = "Utqiaġvik",
                        "3" = "Caribou Poker")) %>% 
  mutate(max_ftc = as.numeric(max_ftc),
         depth_start_cm = as.numeric(depth_start_cm),
         depth_stop_cm = as.numeric(depth_stop_cm)) 

max_ftc_table2 =
  max_ftc_tabledata_wider %>% 
  mutate(depth = paste0(depth_start_cm, "-", depth_stop_cm, " cm")) %>% 
  ungroup() %>% 
  dplyr::select(-depth_start_cm, -depth_stop_cm) %>% 
  pivot_wider(names_from = "depth", values_from = "max_ftc") %>% 
  pivot_longer(-c(season, site_pos), names_to = "depth", values_to = "max_ftc") %>% 
  replace(is.na(.), 0) %>% 
  # separate(depth, sep = "-", into = c("depth_start_cm", "depth_stop_cm")) %>% 
  # mutate(depth_start_cm = as.numeric(depth_start_cm),
  #        depth_stop_cm = as.numeric(depth_stop_cm)) %>%  
  pivot_wider(names_from = "season", values_from = "max_ftc")


knitr::kable(max_ftc_table2)

write.csv(max_ftc_table2, "processed/max_ftc_table2.csv", row.names = FALSE)


max_ftc_tabledata_longer =
  max_ftc_tabledata_wider %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  #select(site, depth_cm, depth_start_cm, depth_stop_cm, depth2, season, ) %>% 
  dplyr::mutate(site = factor(site)) %>% 
  mutate(season = factor(season)) %>% 
  mutate(seas_num = recode(season, "fall" = 1,
                           "winter" = 2,
                           "spring" = 3,
                           "summer" = 4,
                           "total" = 5)) %>% 
  pivot_longer(cols = c(site, season, depth_start_cm, depth_stop_cm, depth2), names_to = 'depth_cm', values_to = 'ftc')


