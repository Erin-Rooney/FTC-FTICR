# Erin C Rooney
# Sept 25 2020
# Adapted from Aleszu Bajak
# Map Plots


# Load data-----------------------------

probe_loc = read.csv("processed/site_locations.csv")

# Load libraries-----------------------

library(tidyverse)
library(urbnmapr)
library(usmap)
library(ggplot2)
library(tidyverse)

# Map attempt 1------------------------------------------
# install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")



states_sf <- get_urbn_map("states", sf = TRUE)

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

states_sf %>%
  filter(state_name == "Alaska") %>%
  ggplot(aes()) +
  geom_sf(fill = "pink", color = "#ffffff") +
  geom_point(data = sites, aes(x = lat, y = lon)) 
  

#sites <- data.frame(ID = c("BARR","TOOL", "BONA", "HEAL"),
 #                      x = c(71.28235, 68.66041, 65.15379, 63.87526),
  #                     y = c(-156.6189, -149.3701, -147.5042, -149.2143))

# KP map script------------------------------------------------------------

knitr::opts_chunk$set(dpi = 300,
                      echo=FALSE,message=FALSE,warning=FALSE,
                      collapse = TRUE,
                      comment = "#>"
)

library(ggplot2)
library(usmap)

sites <- data.frame(lon = c(-156.6189, -149.3701, -147.5042, -149.2143), lat = c(71.28235, 68.66041, 65.15379, 63.87526))
transformed_data <- usmap_transform(sites)
plot_usmap(color = "black") + 
  geom_point(data = transformed_data, 
             aes(x = lon.1, y = lat.1), 
             color = "pink",
             size = 2.5)
  #annotate("text", label = "Barrow", x = -1686856, y = 411954.3, size=2, hjust="left")+
  #annotate("text", label = "Bona", x = -1051020, y = -1842646.7, 
           #size=2, hjust="left")+
  #NULL



library(usmap)
usmap::plot_usmap("states", labels = TRUE)













# AT map script-------------------------------------

#title: "AK-Map"
#author: "Anna Talucci"
#date: "1/22/2020"
#output: html_document
---

library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(sp)
library(sf)
library(ggmap) # devtools::install_github("dkahle/ggmap")
library(ggrepel)
library(RStoolbox)
library(raster)
library(rgdal)
library(RColorBrewer)
library(cowplot)
library(ggspatial)
library(maps)


# Trying to create bound box but it depends on projection
UL -159.2085, 71.3877
UR -153.6549, 71.3877
LR -153.6549, 70.3104
LL -159.2085, 70.3104


(insetrect <- data.frame(xmin = -153.6549, xmax = -159.2085,
                         ymin = 70.3104, ymax = 71.3877))

proj_ak_shape = "+proj=longlat +datum=NAD83 +no_defs"

proj4string(insetrect) <-CRS("+proj=longlat +datum=NAD83 +no_defs")


Utqiavik, AK (AKA Barrow, AK)
labs <- tibble(
  long = c(-156.7886),
  lat = c(71.2906),
  names = ("Utqiagvik")) 
```

# AK Vector data

## As SF object (Plays nice with ggplot)
```{r}
ak_sf <- st_read(
  "../data/AK-state/GU_StateOrTerritory.shp")
```

```{r}
st_crs(ak_sf)
```

### Transform to SF object to equal area projection

```{r}
ak_ee = st_transform(ak_sf, CRS( "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
```


```{r}
ak_sf_eeak = st_transform(ak_sf, CRS( "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
```

## As SP polygon

```{r}
ak_sp = readOGR("../data/AK-state/GU_StateOrTerritory.shp", "GU_StateOrTerritory") 
```
```{r}
plot(ak_sp)
```

### Tasnforming rpojection for SP
```{r}
ak_sp_ee = spTransform(ak_sp, CRS( "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
```
+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs 


```{r}
ak_sp_3338 = spTransform(ak_sp, CRS( "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
```



+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs 
+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 

```{r}
ak_sp_eeak = spTransform(ak_sp, CRS( "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
```


```{r}
ak_sp_wgs84 = spTransform(ak_sp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(ak_sp_wgs84)
```

```{r}
plot(ak_sp_ee)
```
EPSG 3338 NAD83 / Alaska Albers
```{r}
plot(ak_sp_3338)
```
Alaska Albers Equal Area Conic
```{r}
plot(ak_sp_eeak)
```
```{r}
structure(ak_sf_eeak)
```

# Make Map with ggplot and SF Object
From ak_raster_3338 -214588.4, 23414.28, 2212535, 2399715  (xmin, xmax, ymin, ymax)
```{r}
map_ak1 = ggplot() + 
  geom_sf(data = ak_sf_eeak, fill = "#E8E8E8", color = "black") +
  geom_rect(aes(xmin = -214588.4, xmax = 23414.28, ymin = 2212535, ymax = 2399715), alpha = 0, colour = "#000080", size = .5, linetype = 1) +
  xlab("")+ylab("")+
  coord_sf() +
  theme_void() +
  # add a bounding box so that will border the inset
  theme(panel.background = element_rect(colour = "black", fill = "white", size = 0.5))
map_ak1
```
```{r eval=FALSE, include=FALSE}
map_ak2 = ggplot() + 
  geom_sf(data = ak_sf, fill = "#E8E8E8", color = "black") +
  geom_rect(data = insetrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "#000080", size = 1.5, linetype = 1) +
  xlab("")+ylab("")+
  coord_sf() +
  theme_void() +
  # add a bounding box so that will border the inset
  theme(panel.background = element_rect(colour = "black", fill = "white", size = 0.5))
map_ak2
```


# AK Raster Data

## Raster as WGS 84 from GEE
```{r}
ak_raster = raster("../data/raster/S2_AK-2.tif")
```

```{r}
projection(ak_raster)
```

```{r}
newproj = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
```


```{r}
ak_raster_3338 <- projectRaster(ak_raster, crs=newproj)
```


```{r}
structure(ak_raster_3338)
```


## Raster Stack for 3-band RGB Image

```{r}
ak_stack <- stack("../data/raster/S2_AK-2.tif")
```


### Raster stack with EPSG 3338
```{r}
ak_stack_3338 <- stack("../data/raster/S2_AK_3338.tif")
```


```{r}
ak_stack
```

```{r}
ak_stack_3338
```


```{r}
ak_stack@layers
```



From ak_raster_3338 -214588.4, 23414.28, 2212535, 2399715  (xmin, xmax, ymin, ymax)
{bands: ['B12', 'B11', 'B4'], min: 0, max: 1, gamma: 1.5}
```{r}
plotRGB(ak_stack,
        r = 1, g = 2, b = 3,
        stretch = "lin")
```

```{r}
plotRGB(ak_stack_3338,
        r = 1, g = 2, b = 3,
        stretch = "lin")
```

```{r}
ak_stack_3338_clip <- mask(ak_stack_3338, ak_sp_3338)
```

```{r}
plotRGB(ak_stack_3338_clip,
        r = 1, g = 2, b = 3,
        stretch = "lin")
```

```{r}
extent(ak_stack)
```
```{r}
extent(ak_stack_3338)
```
```{r}
extent(ak_stack_3338_clip)
```

```{r}
summary(ak_stack)
```

## Map with north arrow and scale bar
```{r}
plot_raster = ggplot() + ggRGB(ak_stack, r=1, g=2, b=3, stretch = "lin", ggLayer = TRUE, coord_equal = TRUE) +
  xlim(-159.4999, -153.4058) + ylim(69.89028, 71.52341) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()) +
  
  annotation_scale(location = "br", width_hint = 0.25, text_size = 12, text_face = NULL, text_family = "serif", text_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.4, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(line_width = 1, line_col = "white", fill = "white", text_size = 10, text_face = NULL, text_family = "serif", text_col = "black")) +
  coord_sf(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", xlim = c(-159.4999, -153.4058), ylim = c(69.89028, 71.52341), expand = TRUE)
plot_raster
```
xmin       : -214350 
xmax       : 93200 
ymin       : 2212800 
ymax       : 2399450 
```{r}
plot_raster0 = ggplot() + ggRGB(ak_stack_3338_clip, r=1, g=2, b=3, stretch = "lin", ggLayer = TRUE, coord_equal = TRUE) +
  xlim(-214350 , 93200) + ylim(2212800 , 2399450) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()) +
  annotation_scale(location = "bl", width_hint = 0.25, text_size = 12, text_face = NULL, text_family = "serif", text_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.4, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_nautical(line_width = 1, line_col = "black", fill = c("black", "white"), text_size = 10, text_face = NULL, text_family = "serif", text_col = "black", text_angle = 0)) +
  coord_sf(crs = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ", xlim = c(-214350, 93200), ylim = c(2212800, 2399450), expand = TRUE)
plot_raster0
```

```{r fig.height=2, fig.width=3}
plot_raster01 = ggplot() + ggRGB(ak_stack_3338_clip, r=1, g=2, b=3, stretch = "lin", ggLayer = TRUE, coord_equal = TRUE) +
  xlim(-214350 , 93200) + ylim(2212800 , 2399450) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()) +
  annotation_scale(location = "bl", line_width = .5, height = unit(0.1,  "cm"),pad_y = unit(0.1, "cm"),
                   text_pad = unit(0.15, "cm"), text_cex = 0.7, text_size = 8, text_face = NULL, text_family = "sans", text_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_nautical(line_width = .5, line_col = "black", fill = c("black", "white"), text_size = 10, text_face = NULL, text_family = "sans", text_col = "black", text_angle = 0)) +
  coord_sf(crs = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ", xlim = c(-214350, 93200), ylim = c(2212800, 2399450), expand = TRUE)
plot_raster01
```


## Map with image only
```{r}
plot_raster1 = ggplot() + ggRGB(ak_stack, r=1, g=2, b=3, stretch = "lin", ggLayer = TRUE, coord_equal = TRUE) +
  xlim(-159.4999, -153.4058) + ylim(69.89028, 71.52341) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()) 
plot_raster1
```

# Full Map for AK proposal

```{r fig.height=4, fig.width=6}
ak_map = ggdraw() +
  draw_plot(plot_raster1) +
  draw_plot(map_ak1, x = 0.05, y = 0.02, width = .35, height = .35) 
ak_map
```


```{r eval=FALSE, include=FALSE}
ggsave("../figures/ak-map.png", plot = ak_map, width = 6, height = 4, units = "in", dpi = 600)
```

## Equal area
```{r fig.height=4, fig.width=6}
ak_map1 = ggdraw() +
  draw_plot(plot_raster0) +
  draw_plot(map_ak1, x = 0.63, y = 0.62, width = .35, height = .35) 
ak_map1
```



```{r eval=FALSE, include=FALSE}
ggsave("../figures/alaska-map_epsg3338.png", plot = ak_map1, width = 6, height = 4, units = "in", dpi = 600)
```


## Equal area
```{r fig.height=2, fig.width=3}
ak_map2 = ggdraw() +
  draw_plot(plot_raster01) +
  draw_plot(map_ak1, x = 0.63, y = 0.62, width = .35, height = .35) 
ak_map2
```



```{r eval=FALSE, include=FALSE}
ggsave("../figures/alaska-map_epsg3338_sm2.png", plot = ak_map2, width = 3, height = 2, units = "in", dpi = 600)
```