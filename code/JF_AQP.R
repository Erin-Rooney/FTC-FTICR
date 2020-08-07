library(aqp)

library(plyr)

library(RColorBrewer)

library(latticeExtra)

library(plyr)

library(reshape)



#load csv

pedon <- read.csv("Pedon depths1.csv", stringsAsFactors=FALSE)





#look at first several lines of imported file

head(pedon)





#double-check structure of the data

str(pedon)





#gives the class of the object

class(pedon)





#create new column for hex color and convert munsell color to hex format

pedon$soilcolor <- munsell2rgb(pedon$hue, pedon$value, pedon$chroma)







#double-check new "soil color" column

print(pedon)





#convert to apq object "soil profile collection"

depths(pedon) <- id ~ top + bottom





#check the new class

str(pedon)

class(pedon)

summary(pedon)





#plot

plot(pedon, name = 'horizon', color = 'soilcolor')