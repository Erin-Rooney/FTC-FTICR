# Erin Rooney
# Adapted from KP
# July 23 2020
# GHG FTC C12 and C13 data

# load data---------------------------------
ghg_csv = read.csv("processed/ghg_ftc.csv")
ghg_csv2 = read.csv("processed/ghg_depth.csv")
ftc_dat = read.csv("processed/FTC_quant_inprocess.csv")

###
library(dplyr)
library(tidyr)
str(ghg_csv2)
str(ftc_dat)
levels(as.factor(ghg_csv2$trmt))
#mutate(TRT = factor(TRT, levels = c("CON", "FTC")))

#

str(ghg_csv2)

# set data frames-----------------------------

#I don't know what I'm doing here.

toolik = ghg_csv$Site=="TOOL"
healy = ghg_csv$Site=="HEALY"
control = ghg_csv$TRT=="CON"
ftc = ghg_csv$TRT=="FTC"
DAY
Day_1 = ghg_csv$DAY=="1"
Day_4 = ghg_csv$DAY=="4"
Day_7 = ghg_csv$DAY=="7"
Day_14 = ghg_csv$DAY=="14"

# ggplot set up-----------------------------------
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

library(ggplot2)
library(soilpalettes)
library(soilDB)
library(aqp)
library(sharpshootR)
library(sp)
library(ggridges)

# gain in C ggplot----------------------------------------
ggplot(ghg_csv, aes(x=TRT, y=gain_ug_per_gOC, fill=DAY)) + geom_boxplot() 


ggplot(ghg_csv, aes(x=Site, y=gain_ug_per_gOC, fill=DAY)) + geom_boxplot()

ggplot(ghg_csv, aes(x=DAY, y=gain_ug_per_gOC, fill=Site)) + geom_boxplot()

ggplot(ghg_csv, aes(x=DAY, y=gain_ug_per_gOC, fill=TRT)) + geom_boxplot()


# FT quant ggplots---------------------------------------

#boxplots
ggplot(ftc_dat, aes(x = def1, y = depth_cm, fill = site)) + geom_boxplot() + theme_er() +
  scale_fill_manual (values = soil_palette("gley", 2)) + facet_grid(season~.)

#dotplots failed
ggplot() + geom_dotplot(data = ftc_dat, aes(y = depth_m, x = Def1, color = site)) + theme_er() +
  scale_color_manual (values = soil_palette("gley", 2)) + facet_wrap(season~.)

ggplot(ftc_dat, aes(x=def1, y=depth_cm, fill=site))

#ridge and grid plots
ggplot(ftc_dat, aes(x = def1, y = depth_cm, color = season)) +
stat_bin2d( mapping = NULL, data = NULL, geom = "tile", position = "identity" + bins = 30 + binwidth = NULL + drop = TRUE)

ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + facet_grid(season~.)

ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + scale_x_continuous(limits = c(-5, 30)) + facet_grid(season~.)

ggplot(ftc_dat, aes(x = def1, y = site, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probablity", direction = -1) + theme_er() + scale_x_continuous(limits = c(-5, 30)) + facet_grid(season~.)

#raster plots

ggplot(ftc_dat, aes(x = depth_cm, y = site, fill = def1)) +
  geom_raster(hjust = 0, vjust = 0) + theme_er() + facet_grid(season~.)

cars <- ggplot(ftc_dat, aes(depth_cm, factor(site)))

cars + stat_bin2d(aes(fill= after_stat(count)), binwidth = c(3,1)) + theme_er() + facet_grid(season~.)

ggplot(ftc_dat, aes(x = depth_cm, y = site, height = def1)) +
  geom_density_ridges(stat = "identity") + theme_er() + facet_grid(season~.)


# aov-------------------------------------------

ghg_aov1 = aov(data = ghg_csv, gain_ug_per_gOC ~ Site)
summary(ghg_aov1)


ghg_aov2 = aov(data = ghg_csv, gain_ug_per_gOC ~ DAY)
summary(ghg_aov2)


ghg_aov3 = aov(data = ghg_csv, gain_ug_per_gOC ~ Site*DAY)
summary(ghg_aov3)

# C/g soil ggplot-------------------------------------


ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("redox", 4)) 

###

ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + 
  geom_boxplot() + theme_er() + 
  scale_fill_manual (values = soil_palette("redox", 4)) 

ggplot(ghg_csv, aes(x = DAY, y = ug_12C_g_soil, color = Site)) +
  geom_dotplot() +
  xlab("Day") +
  ylab("CO2 AVG") +
  ggtitle("12C")




a = ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot() 

a + theme_er + scale_fill_manual = soil_palette("redox, 4")

ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=TRT)) + geom_boxplot()
