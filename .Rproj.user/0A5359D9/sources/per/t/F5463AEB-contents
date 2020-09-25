# Erin C Rooney
# Sept 25 2020
# Adapted from Aleszu Bajak
# Map Plots

library(ggplot2)
library(tidyverse)
library(fiftystater)

mapdata <- read.csv("processed/state-medal-count.csv", header=TRUE, stringsAsFactors=FALSE) 
mapdata %>% glimpse()


probe_loc %>%
ggplot() + geom_polygon( data=probe_loc, aes(x=referenceLongitude, y=referenceLatitude, group = siteID),color="white", fill="grey10" )

ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) + 
  geom_point(data=mapdata, aes(x=lon, y=lat, size = medals), color="black") + 
  scale_size(name="", range = c(2, 20)) + 
  guides(size=guide_legend("GABF medals 1985-2015")) +
  theme_void()
