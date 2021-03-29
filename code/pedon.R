library(aqp)
#library(plyr)
library(RColorBrewer)
library(latticeExtra)
library(reshape)
library(tidyverse)


#load csv
pedon <- read.csv("processed/ped_profiles_munsell_dry.csv", stringsAsFactors=FALSE)

levels(as.factor(pedon$site))

pedon = pedon %>% 
  mutate(site = factor(site, levels = c('HEAL', 'BONA', 'TOOL', 'BARR')))

#look at first several lines of imported file
head(pedon)

#double-check structure of the data
str(pedon)

#gives the class of the object
class(pedon)

# pbindlist into single SPC, not that attribute names may not be the same
#g <- pbindlist(list('HEAL', 'BONA', 'TOOL', 'BARR'))

#create new column for hex color and convert munsell color to hex format
pedon$soilcolor <- munsell2rgb(pedon$hue, pedon$value, pedon$chroma)


#double-check new "soil color" column
print(pedon)

#convert to apq object "soil profile collection"
depths(pedon) <- site ~ top + bottom


#check the new class
str(pedon)
class(pedon)
summary(pedon)

pedon$idcol = factor(pedon$idcol, levels = c('HEAL', 'BONA', 'TOOL', 'BARR'))

pedon$site

pedon$horizons

#plot
plot(pedon, name = 'horizon', color = 'soilcolor') 

