# Angela Possinger
# ER Modifications
# 9 28 2020
# Alaska Map and Neon Sites

#Plot US Map 

library(Rmisc)
library(dplyr)
library(ggplot2)
library(ggforce)
library(ggrepel)
library(ggmap)
library(usmap)
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

#FTC Quantification dataset 
FTC_dat = readRDS("processed/final_dat.RData")

#SOMMOS site-level data (latitude and longitude)
site_dat = read.csv("processed/SOMMOS_Site_11-25-19.csv")

#Subset permafrost sites only 
permafrost_sites = subset(site_dat, site == "HEAL" | site == "BARR" | site == "BONA" | site == "TOOL")
dim(permafrost_sites) #should by 4 x 262

#Merge with FTC_dat
FTC_site = FTC_dat %>% right_join(permafrost_sites, by=c("site"))
head(FTC_site)

#Plot US map needs data in specific format: first and second column must be named "lon" and "lat" to use the usmap_transform function. 
#For variables that differ within sites (e.g. FTC), needs just one value, so can calculate average over years etc, or use just one FTC measurement 

#Relevant data
plot_dat = subset(FTC_site, mag.vec == 1.5)
plot_dat=FTC_site[,c("longitude.dec_deg","latitude.dec_deg","site","Def1","season","core","depth_pos","year")]

#Here can calculate what you want to average over (in this example, average over everything but site) 
FTC_example = summarySE(plot_dat, measurevar = "Def1", groupvars = c("longitude.dec_deg", "latitude.dec_deg","site"), na.rm=TRUE)

#To use usmap_transform, the first two columns need to be named "lon" and "lat"

colnames(FTC_example)=c("lon","lat","site","N","Def1","sd","se","ci")
plot_dat_transformed <- usmap_transform(FTC_example)

#This is giving a warning related to the projection, because checking the projection used in the function gives this warning: 
usmap_crs() 
# Warning message:
#   In showSRID(uprojargs, format = "PROJ", multiline = "NO") :
#   Discarded datum unknown in CRS definition

plot_usmap(include=c("AK")) +
  geom_point(aes(x=lon.1, y=lat.1, fill=Def1), pch=21, data=plot_dat_transformed, size=3, color="black", show.legend=TRUE) + 
  geom_label_repel(aes(x=lon.1, y=lat.1, label=site), data=plot_dat_transformed, size=4.5, point.padding = 0.2) + 
  labs(fill="Freeze-thaw cycles (#)") + 
  scale_fill_viridis_c(option="plasma", direction=-1, guide = guide_colourbar(barwidth=10, barheight=1, direction = "horizontal", reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +  
  theme(legend.position=c(0.1,-0.3),
        plot.margin = unit(c(2,2,2,2), "cm"), 
        legend.title = element_text(size=12, color="black"), 
        legend.text = element_text(size=10, color="black"))

plot_usmap(include=c("AK")) +
  geom_point(aes(x=lon.1, y=lat.1, fill=Def1), pch=21, data=plot_dat_transformed, size=3, color="black", show.legend=TRUE) + 
  geom_label_repel(aes(x=lon.1, y=lat.1, label=site), data=plot_dat_transformed, size=4.5, point.padding = 0.2) + 
  labs(fill="Freeze-thaw cycles (#)") + 
  scale_fill_binned(values = soil_palette("redox", 4), guide = guide_colourbar(barwidth=10, barheight=1, direction = "horizontal", reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +  
  theme(legend.position=c(0.1,-0.3),
        plot.margin = unit(c(2,2,2,2), "cm"), 
        legend.title = element_text(size=12, color="black"), 
        legend.text = element_text(size=10, color="black"))
