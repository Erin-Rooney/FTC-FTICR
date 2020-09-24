# Erin C Rooney
# SOMMOS data

#load data
sommos_csv = read.csv("processed/horizon_processed4.csv")



#select data
barrow = sommos_csv$site=="BARR"
healy = sommos_csv$site=="HEAL"
toolik = sommos_csv$site=="TOOL"
bona = sommos_csv$site=="BONA"
data = as.data.frame(barrow)
data = as.data.frame(healy)
data = as.data.frame(toolik)
data = as.data.frame(bona)

#load library
library(ggplot2)

#plot Fe distributions


p <- ggplot() +
  
  geom_line(data = sommos_csv, aes(y=top_depth.cm, x=OC.g100g, color=site)) +
  
  scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm))

p + facet_grid(. ~ site)


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

###########

plot(activelayer_dat)

p <- ggplot() +
  
  geom_line(data = activelayer_dat, aes(x = Def1, y = depth_m, color = site_pos)) +
  
  scale_y_reverse()
  
 # scale_y_continuous(trans = "reverse", breaks = (activelayer_dat$depth_m))

p

p + facet_grid(. ~ site_pos)



