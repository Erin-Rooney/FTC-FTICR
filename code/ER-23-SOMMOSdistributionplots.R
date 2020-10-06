# Erin C Rooney
# SOMMOS data

#load data-------------------------------------
sommos_csv = read.csv("processed/horizon_processed4.csv")

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

theme_jack <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      legend.position = "top",
      legend.key=element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.5, 'lines'),
      panel.border = element_rect(color="gray",size=2, fill = NA),
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black"), #size=rel(3)),
      axis.title.y = element_text(colour = "black", angle=90),
      panel.background = element_rect(fill="black"),
      panel.grid.minor = element_line(color= "dark gray"),
      panel.grid.major = element_line(colour = "dark gray"),
      plot.background = element_rect(fill="white"))
}
      
# select data-----------------------------------
# barrow = sommos_csv$site=="BARR"
# healy = sommos_csv$site=="HEAL"
# toolik = sommos_csv$site=="TOOL"
# bona = sommos_csv$site=="BONA"
# data = as.data.frame(barrow)
# data = as.data.frame(healy)
# data = as.data.frame(toolik)
# data = as.data.frame(bona)

#load library--------------------------------
library(tidyverse)
library(PNWColors)

#aov---------------------------------------

sommos_aov1 = aov(data = sommos_csv, LIG.ugg ~ site)
summary(sommos_aov1)





#ggplots------------------------------------------------------------

sommos_csv %>% 
  ggplot() +
  geom_point(data = sommos_csv, aes(y=HF.g100g, x=OCC.g100g, color=site)) +
  theme_er()
  #scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) 
  #facet_grid(. ~ site)

sommos_csv %>% 
  ggplot() +
  geom_point(data = sommos_csv, aes(y=OCC.g100g, x=LIG.ugg, color=site, size=10, shape=horizon_type)) +
  theme_jack() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))
  
  
#scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) 
#facet_grid(. ~ site)

sommos_csv %>% 
  ggplot() +
  geom_point(data = sommos_csv, aes(y=FLF.g100g, x=HF.g100g, color=site, size=OCC.g100g)) +
  #scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) +
  theme_er2() +
  strip.background = element_rect(colour="black", fill="black") #facet formatting

  #facet_grid(. ~ site)

###########



p <- ggplot() +
  
  geom_line(data = sommos_csv, aes(y=top_depth.cm, x=AO_Fe.g100g, color=site)) +
  
  #scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) +
  
  scale_y_continuous()
  
  scale_y_reverse +
  
  scale_x_continuous(position = "top")

p + facet_grid(. ~ site)



p <- ggplot() +
  geom_line(data = as.data.frame(barrow), aes(y = top_depth.cm, x= clay.gg, color = site))

p

#Active layer ftc plot----------------------------

plot(activelayer_dat)

p <- ggplot() +
  
  geom_line(data = activelayer_dat, aes(x = Def1, y = depth_m, color = site_pos)) +
  
  scale_y_reverse()
  
 # scale_y_continuous(trans = "reverse", breaks = (activelayer_dat$depth_m))

p

p + facet_grid(. ~ site_pos)



