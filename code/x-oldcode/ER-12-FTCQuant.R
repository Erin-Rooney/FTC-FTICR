# FTCQuant and NEON cores
# Erin Rooney
# July 23 2020

library(usethis)
library(devtools)

#install.packages("backports")
library(backports)

#####
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

Sys.which("make")
## "C:\\rtools40\\usr\\bin\\make.exe"

#####

#####


install_github("BajczA475/FTCQuant/FTCQuant")

library(FTCQuant)

show(Temp_07_08_2019_5cores)

soilTempMean=(Temp_07_08_2019_5cores$soilTempMean)

FTCQuant::freeze.thaw.analysis(data.frame(Temp_07_08_2019_5cores, soilTempMean), mag.vec=0, dur.vec=1, thres.vec=0)
