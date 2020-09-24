# Test AQP
# Adapted from D.E. Beaudette
# Erin C Rooney
# Sept 23 2020

# load required libraries

library(ggplot2)
library(soilpalettes)
library(soilDB)
library(aqp)
library(sharpshootR)
library(sp)

# load sample data
data("loafercreek")

# keep first 10 profiles
x <- loafercreek[1:10, ]

# default margins are too wide, make smaller
par(mar=c(1,1,2,1))

# profile sketches 
plot(x, label='pedon_id', id.style='side')
