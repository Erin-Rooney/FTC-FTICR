# Erin Rooney
# Adapted from KP
# July 23 2020
# GHG FTC C12 and C13 data

# load data---------------------------------
ghg_csv2 = read.csv("processed/ghg_depth.csv")
#ftc_dat = read.csv("processed/FTC_quant_inprocess.csv")
#ftc_dat = read.csv("processed/final_dat2.csv")
ftc_dat = read.csv("processed/combined_TOOL.csv")
#sommos_oc = read.csv("processed/oc_sommos_neonoc.csv")
emsl_oc = read.csv("processed/CN_emsl_forghg.csv")
probe_loc = read.csv("processed/Probe Locations.csv")

# set data frames-----------------------------

library(tidyverse)
library(forcats) 
library(nord)
library(PNWColors)
library(soilpalettes)




#GHG
# calculate mean ghg

ghg_avg = 
  ghg_csv2 %>% 
  #filter(!season %in% "activelayer") %>% 
  ## NOTE: USE ADDITIONAL FILTERS AS NEEDED. I SEE MULTIPLE ENTRIES IN YEAR, MAG.VEC, DURATION, ETC.
  ## FOR NOW, I AM COMBINING ACROSS ALL THOSE VARIABLES, KEEPING ONLY DEPTH, SEASON, SITE AS GROUPING VARIABLES
  
  # create a `total` ftc column for annual total ftc
  group_by(site, horizon, trmt, mid) %>% 
  dplyr::mutate(total = sum(gain_co2_ug_g_oc)) %>% 
  ungroup() %>% 
  
  # now, incorporate the `total` data into `season`
  # spread the `season` columns, and then recombine with `total`
  #spread(season, Def1) %>% 
  #wgather(season, Def1, total:winter) %>%
  # many NAs were introduced when forcing wide-form. remove all rows containing NAs
  na.omit() %>% 
  
  # now, calculate mean FTC per site/depth/season
  group_by(site, horizon, trmt, mid) %>% 
  dplyr::summarise(gain_ug_g_oc = as.integer(mean(gain_co2_ug_g_oc))) %>% 
  ungroup() %>% 
  
  # create new columns for depth range
  # create bins of 5 cm depth increments
  mutate(depth_bins = cut_width(mid, width = 5, center=2.5)) %>% 
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
         depth_stop_cm = as.integer(depth_stop_cm))


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



# GHG bubble plot with depth on y axis--------------------------------------
ghg_csv2 %>% 
  filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = site, size = gain_co2_ug_g_oc, color = gain_co2_ug_g_oc))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.05))+
  scale_y_reverse() +
  coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_gradient(low = "blue", high = "yellow")+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(day~trmt)



# Okay so here I am taking the averages that I got above and just adding a column for depth

ghg_corr = ghg_avg %>% 
  mutate(depth = depth_stop_cm - depth_start_cm)

# Okay now this is a figure. It's not working well and has issues, but it is a barplot for the ghg respiration data

ghg_corr %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = depth, x = site, fill = gain_ug_g_oc))+
  #geom_jitter()+
  geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_fill_gradient(low = "blue", high = "yellow")+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(~trmt)

# okay so I need to try this same data but in a line plot. What's great about the following plots
# is that I figured out how to align them vertically. 

#I don't know if I actually need to load these

library(reshape2)
library(plotly)

#And here's the plot. But it's using the averages so I don't have data by day.

ghg_avg %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = gain_ug_g_oc, color = trmt))+
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(.~site)


# loading another package I think I actually need this one.

library(plyr)

#Okay now I am creating a new data frame that has some statistical summaries

ghg_summary = ddply(ghg_csv2, c("site", "trmt", "mid"), summarise,
      N    = length(gain_co2_ug_g_oc),
      mean = mean(gain_co2_ug_g_oc),
      sd   = sd(gain_co2_ug_g_oc),
      se   = sd / sqrt(N)
)

# okay here's a second summary because I wanted to include the day info in the statistical summaries

ghg_summary2 = ddply(ghg_csv2, c("site", "trmt", "mid", "day"), summarise,
                     N    = length(gain_co2_ug_g_oc),
                     mean = mean(gain_co2_ug_g_oc),
                     sd   = sd(gain_co2_ug_g_oc),
                     se   = sd / sqrt(N)
)

#okay this is supposed to be helpful for adding error bars to my line/points but it's not working
pd <- position_dodge(0.1)

#alright here's a figure. no error bars. but it does have sd info in the data frame but it looks real weird when I put it in the plot
#important to note, all days are grouped in this plot/data frame

ghg_summary %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = mean, color = trmt))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(.~site)

ghg_summary2 %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = mean, color = day))+
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 4)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(site~trmt)

#and now a plot that separates out days. Not sure why error bars aren't showing up :(


ghg_csv2 %>% 
  #filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = gain_co2_ug_g_oc, color = trmt, group = trmt))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.1, 
  #                  )) +
  geom_point()+
  geom_line(orientation = "y")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  #coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 2)))+
  ggtitle("Respiration (ug per g OC)") +
  theme_er() +
  facet_grid(day~site)


ghg = ghg_csv2 %>% 
  mutate(day = factor(day, levels = c('day1', 'day4', 'day7', 'day14'))) %>%
  mutate(trmt = recode(trmt, 'ftc' = "freeze-thaw"),
         site = recode(site, "healy" = "Healy",
                       "tool" = "Toolik")) %>% 
  #filter(mid > 0) %>% 
  ggplot()+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se, width=.1, 
  #                  )) +
  geom_point(aes(y = mid, x = gain_co2_ug_g_oc, group = day, fill = day), shape = 21, size = 3.5, alpha = 0.6, color = 'black')+
  geom_line(aes(y = mid, x = gain_co2_ug_g_oc, group = day, color = day), orientation = "y", linetype = "dashed")+
  #geom_jitter()+
  #geom_bar(position = "stack", stat= "identity")+
  scale_y_reverse() +
  scale_fill_manual(values = (PNWColors::pnw_palette("Sunset2", 4)))+
  scale_color_manual(values = (PNWColors::pnw_palette("Sunset2", 4)))+
  labs(y = "depth, cm",
       x = 'gain in CO2 μg per g TC',
       fill = "",
       color = "")+
  theme_er() +
  facet_grid(trmt~site)+
  theme(panel.border = element_rect(color="white",size=0.5, fill = NA))

ggsave("output/ghg.tiff", plot = ghg, height = 10, width = 6)
ggsave("output/ghg.jpeg", plot = ghg, height = 10, width = 6)



sommos_oc %>% 
  filter(mid > 0) %>% 
  ggplot(aes(y = mid, x = site, size = OC.g100g, color = OC.g100g))+
  #geom_jitter()+
  geom_point(position = position_jitter(width = 0.2))+
  scale_y_reverse() +
  coord_cartesian(ylim = c(70,0)) +
  # scale_size_continuous()
  scale_color_gradient(low = "purple", high = "yellow")+
  ggtitle("Organic Carbon Content, g per 100g") +
  theme_er() 

# gain in C ggplot----------------------------------------
ggplot(ghg_csv2, aes(x=trmt, y=gain_ug_g_oc, fill=day)) + geom_boxplot() 


ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + geom_boxplot()

ggplot(ghg_csv2, aes(x=day, y=gain_ug_g_oc, fill=site)) + geom_boxplot()

ggplot(ghg_csv, aes(x=day, y=gain_ug_g_oc, fill=site)) + geom_boxplot()

# heatmap--------------------------------------------------------------
ftc_actdat %>% 
  filter(depth_cm<100 ) %>%
  ggplot(aes(y = depth_cm, x = site, fill = Def1))+
  geom_tile()+
  scale_y_reverse()+
  coord_fixed(ratio=1/4)

ftc_actdat %>% 
  filter(depth_cm<100 ) %>% 
  ggplot(aes(y = depth_cm, x = site, fill = Def1))+
  geom_tile()+
  scale_y_reverse()+
  coord_fixed(ratio=1/2)


ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


cars <- ggplot(ftc_dat, aes(depth_cm, factor(site)))

cars + stat_bin2d(aes(fill= after_stat(count)), binwidth = c(3,1)) + theme_er() + facet_grid(season~.)

# aov-------------------------------------------

ghg_aov1 = aov(data = ghg_csv2, gain_ug_g_oc ~ site)
summary(ghg_aov1)


ghg_aov2 = aov(data = ghg_csv2, gain_ug_g_oc ~ day)
summary(ghg_aov2)


ghg_aov3 = aov(data = ghg_csv2, gain_ug_g_oc ~ site*day)
summary(ghg_aov3)

# C/g soil ggplot-------------------------------------


ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("redox", 4)) 

###

ggplot(ghg_csv2, aes(x=site, y=gain_ug_g_oc, fill=day)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("redox", 4)) 

