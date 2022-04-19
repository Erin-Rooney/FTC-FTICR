#ECRooney
#8 2 2021
#Temp and Precip maps

source("code/0-method-packages.R")

library(ggforce)
library(ggrepel)
library(ggmap)
library(usmap)


#NEON climate dataset 
temp_data = read.csv("processed/temp.csv")

#SOMMOS site-level data (latitude and longitude)
site_dat = read.csv("processed/SOMMOS_Site_11-25-19.csv")

#Subset permafrost sites only 
permafrost_sites = subset(site_dat, site == "HEAL" | site == "BARR" | site == "BONA" | site == "TOOL")
dim(permafrost_sites) #should by 4 x 262


#Merge with FTC_dat
temp_site = temp_data %>% right_join(permafrost_sites, by=c("site")) 
head(temp_site)

 
  

#Plot US map needs data in specific format: first and second column must be named "lon" and "lat" to use the usmap_transform function. 
#For variables that differ within sites (e.g. FTC), needs just one value, so can calculate average over years etc, or use just one FTC measurement 

#Relevant data
plot_dat = subset(temp_site)
plot_dat=temp_site[,c("longitude.dec_deg","latitude.dec_deg","site","temp_C","season","elevation.m","fire_management")]

#Relevant data
plot_dat = subset(temp_site)
plot_dat=temp_site[,c("longitude.dec_deg","latitude.dec_deg","site","temp_C","season","elevation.m","fire_management")]


plot_dat =
  plot_dat %>%
  mutate(site = recode(site, "BARR" = "Utqiaġvik",
                         "TOOL" = "Toolik",
                         "BONA" = "Caribou Poker",
                         "HEAL" = "Healy"))
  
#Here can calculate what you want to average over (in this example, average over everything but site) 
temp_example = summarySE(plot_dat, measurevar = "temp_C", groupvars = c("longitude.dec_deg", "latitude.dec_deg","site"), na.rm=TRUE)

#To use usmap_transform, the first two columns need to be named "lon" and "lat"

colnames(temp_example)=c("lon","lat","site","mean","temp_C","sd","se","ci")
plot_dat_transformed <- usmap_transform(temp_example)


#This is giving a warning related to the projection, because checking the projection used in the function gives this warning: 
usmap_crs() 
# Warning message:
#   In showSRID(uprojargs, format = "PROJ", multiline = "NO") :
#   Discarded datum unknown in CRS definition

plot_usmap(include=c("AK")) +
  geom_point(aes(x=lon.1, y=lat.1, fill=temp_C), pch=21, data=plot_dat_transformed, size=4, color="black", show.legend=TRUE) + 
  geom_label_repel(aes(x=lon.1, y=lat.1, label=site), data=plot_dat_transformed, size=4.5, point.padding = 0.2) + 
  labs(fill="Mean Annual Temperature, Celsius") + 
  # scale_fill_viridis_c(option="viridis", direction=-1, guide = guide_colourbar
  #                      (barwidth=10, barheight=1, direction = "horizontal", 
  #                        reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +
  scale_fill_gradientn(colors = (pnw_palette("Shuksan2", 4)), 
                       guide = guide_colourbar(barwidth=10, barheight=1, direction = "horizontal", 
                        reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +  
  theme(legend.position=c(0.1,-0.3),
        plot.margin = unit(c(2,2,2,2), "cm"), 
        legend.title = element_text(size=12, color="black"), 
        legend.text = element_text(size=10, color="black"))

# plot_usmap(include=c("AK")) +
#   geom_point(aes(x=lon.1, y=lat.1, fill=N), pch=21, data=plot_dat_transformed, size=4, color="black", show.legend=TRUE) + 
#   geom_label_repel(aes(x=lon.1, y=lat.1, label=site), data=plot_dat_transformed, size=4.5, point.padding = 0.2) + 
#   labs(fill="Freeze-thaw cycles (#)") + 
#   scale_fill_gradientn(colors = rev(soil_palette("redox", 4)), 
#                        guide = guide_colourbar(barwidth=10, barheight=1, direction = "horizontal", 
#                                                reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +  
#   theme(legend.position=c(0.1,-0.3),
#         plot.margin = unit(c(2,2,2,2), "cm"), 
#         legend.title = element_text(size=12, color="black"), 
#         legend.text = element_text(size=10, color="black"))

#sum(FTC_example$Def1)




#NEON climate dataset 
precip_data = read.csv("processed/precip.csv")

#SOMMOS site-level data (latitude and longitude)
site_dat = read.csv("processed/SOMMOS_Site_11-25-19.csv")

#Subset permafrost sites only 
#permafrost_sites = subset(site_dat, site == "HEAL" | site == "BARR" | site == "BONA" | site == "TOOL")
#dim(permafrost_sites) #should by 4 x 262

permafrost_sites = subset(site_dat, site == "HEAL" | site == "TOOL")
dim(permafrost_sites) #should by 4 x 262

#Merge with FTC_dat
precip_site = precip_data %>% right_join(permafrost_sites, by=c("site")) 
head(precip_site)




#Plot US map needs data in specific format: first and second column must be named "lon" and "lat" to use the usmap_transform function. 
#For variables that differ within sites (e.g. FTC), needs just one value, so can calculate average over years etc, or use just one FTC measurement 

#Relevant data
plot_dat = subset(precip_site)
plot_dat=precip_site[,c("longitude.dec_deg","latitude.dec_deg","site","precip_cm","season","elevation.m","fire_management")]

#Relevant data
plot_dat = subset(precip_site)
plot_dat=precip_site[,c("longitude.dec_deg","latitude.dec_deg","site","precip_cm","season","elevation.m","fire_management")]

plot_dat =
  plot_dat %>% 
  mutate(site = recode(site, "BARR" = "Utqiaġvik",
                       "TOOL" = "Toolik",
                       "BONA" = "Caribou Poker",
                       "HEAL" = "Healy"))


plot_dat2 =
  plot_dat %>% 
  mutate(site = recode(site, "BARR" = "Utqiaġvik",
                       "TOOL" = "Toolik",
                       "BONA" = "Caribou Poker",
                       "HEAL" = "Healy")) %>% 
  filter(site == "Toolik" & "Healy")

#Here can calculate what you want to average over (in this example, average over everything but site) 
temp_example = summarySE(plot_dat, measurevar = "precip_cm", groupvars = c("longitude.dec_deg", "latitude.dec_deg","site"), na.rm=TRUE)

#To use usmap_transform, the first two columns need to be named "lon" and "lat"

colnames(temp_example)=c("lon","lat","site","mean","precip_cm","sd","se","ci")
plot_dat_transformed <- usmap_transform(temp_example)


#This is giving a warning related to the projection, because checking the projection used in the function gives this warning: 
usmap_crs() 
# Warning message:
#   In showSRID(uprojargs, format = "PROJ", multiline = "NO") :
#   Discarded datum unknown in CRS definition

plot_usmap(include=c("AK")) +
  geom_point(aes(x=lon.1, y=lat.1, fill=precip_cm), pch=21, data=plot_dat_transformed, size=4, color="black", show.legend=TRUE) + 
  geom_label_repel(aes(x=lon.1, y=lat.1, label=site), data=plot_dat_transformed, size=4.5, point.padding = 0.2) + 
  labs(fill="Precipitation, cm") + 
  # scale_fill_viridis_c(option="viridis", direction=-1, guide = guide_colourbar
  #                      (barwidth=10, barheight=1, direction = "horizontal", 
  #                        reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +
  scale_fill_gradientn(colors = rev(pnw_palette("Anemone", 4)), 
                       guide = guide_colourbar(barwidth=10, barheight=1, direction = "horizontal", 
                                               reverse = FALSE, title.position="top", ticks=FALSE, label=TRUE)) +  
  theme(legend.position=c(0.1,-0.3),
        plot.margin = unit(c(2,2,2,2), "cm"), 
        legend.title = element_text(size=12, color="black"), 
        legend.text = element_text(size=10, color="black"))

#manuscript figure, black points only

plot_usmap(include=c("AK")) +
  geom_point(aes(x=lon.1, y=lat.1), pch=19, data=plot_dat_transformed, size=4, fill="black", show.legend=TRUE) + 
  geom_label_repel(aes(x=lon.1, y=lat.1, label=site), data=plot_dat_transformed, size=4.5, point.padding = 0.2) + 
  labs(fill="Precipitation, cm") + 
    theme(legend.position=c(0.1,-0.3),
        plot.margin = unit(c(2,2,2,2), "cm"), 
        legend.title = element_text(size=12, color="black"), 
        legend.text = element_text(size=10, color="black"))

