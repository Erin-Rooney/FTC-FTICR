library(aqp)
#library(plyr)
library(RColorBrewer)
library(latticeExtra)
library(reshape)
library(tidyverse)


#load csv
pedon <- read.csv("processed/ped_profiles_munsell_dry.csv", stringsAsFactors=FALSE)

tool <- read.csv("processed/ped_profiles_munsell_dry.csv", stringsAsFactors=FALSE) %>% filter(site == "TOOL")

levels(as.factor(pedon$site))

pedon = pedon %>% 
  mutate(site = recode(site, "BARR" = "Utqiaġvik",
                         "TOOL" = "Toolik",
                         "BONA" = "Caribou Poker",
                         "HEAL" = "Healy")) 

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

tool$soilcolor <- munsell2rgb(tool$hue, tool$value, tool$chroma)


#double-check new "soil color" column
print(pedon)

#convert to apq object "soil profile collection"
depths(pedon) <- site ~ top + bottom
depths(tool) <- site ~ top + bottom


#check the new class
str(pedon)
class(pedon)
summary(pedon)


#pedon$idcol = factor(pedon$idcol, levels = c('HEAL', 'BONA', 'TOOL', 'BARR'))

pedon$site

pedon$horizons

#plot
plot(pedon, name = 'horizon', color = 'soilcolor') 

plot(tool, name = 'horizon', color = 'soilcolor')

