# E Rooney, A Possinger, K Patel
# 4 12 2021

ftc_dat = read.csv("processed/combined_TOOL.csv")

library(tidyverse)
library(forcats) 
library(nord)
library(PNWColors)
library(soilpalettes)


# ggplot set up-----------------------------------
theme_er1 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "bottom",
          #legend.key=element_blank(),
          #legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.text.x.bottom = element_text (vjust = 0.5, hjust=1, angle = 90),
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

theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "bottom",
          #legend.key=element_blank(),
          #legend.title = element_blank(),
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


#rename site codes and set levels, exclude active layer, set season levels

ftc_fulldat = ftc_dat %>% 
  mutate(site = recode (site, "TOOL" = "Toolik"))%>% 
  filter(!is.na(Def1)) %>%       
  mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer")))  

# averages for geom_bar-------------------------------------------

#FT
ftc_sum = 
  ftc_fulldat %>% 
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
  group_by(site, depth_cm, season) %>% 
  dplyr::summarise(ftc = as.integer(sum(Def1))) %>% 
  ungroup() %>% 
  
  # bin top 10 cm into 5-cm bins, and the rest into 10-cm bins
  
  mutate(depth_bins1 = case_when(depth_cm <= 10 ~ cut_width(depth_cm, width = 5, center=2.5)),
         depth_bins2 = case_when(depth_cm > 10 ~ cut_width(depth_cm, width = 10, center=5)),
         depth_bins = paste0(depth_bins1, depth_bins2),
         depth_bins = str_remove(depth_bins, "NA")) %>% 
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

ftc_sum_seasnum = 
  ftc_sum %>% 
  mutate(season = factor(season)) %>% 
  mutate(seas_num = recode(season, "fall" = 1,
                           "winter" = 2,
                           "spring" = 3,
                           "summer" = 4,
                           "total" = 5))

#

ftc_sum_seasnum %>%
  filter(seas_num < 5) %>% 
  ggplot()+
  geom_rect(aes(xmin = seas_num -0.35, xmax = seas_num + 0.35, 
                ymin = depth_start_cm, ymax = depth_stop_cm, fill = as.numeric(ftc)))+
  scale_y_reverse()+
  scale_x_continuous(breaks = 1:4,
                     labels = c("fall", "winter", "spring", "summer"))+
  annotate("segment", x = 0, xend = 4.9, y = 10, yend = 10, color = "black", size= 1.5,
           linetype = 2) +
  scale_fill_gradientn(colors = (pnw_palette("Sunset2", 6
  )))+
  labs(
    y = "depth, cm",
    x = "",  
    fill = "freeze-thaw cycles, count")+
  
  theme_er()

