# Erin Rooney
# August 8 2020
# Temp data from NEON

setwd("~/R/R/R Datasets/introductoryR-master")

ftcquant_dat = read.csv("processed/activelayer_dat.csv") 
toolik = ftcquant_dat[ftcquant_dat$site=="TOOL",]
depth = ftcquant_dat[ftcquant_dat$depth_m,]
ftc = ftcquant_dat[ftcquant_dat$Def1,]


p = ggplot(toolik) +
  geom_line(aes(x = Def1, y=depth_m, color = core))

p
  
###########
  
p = ggplot(ftcquant_dat) +
  geom_line(aes(x = depth_m, y=Def1, color = site))

p
