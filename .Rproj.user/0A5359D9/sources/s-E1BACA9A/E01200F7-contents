# Erin Rooney
# Adapted from KP
# July 23 2020
# GHG FTC C12 and C13 data


ghg_csv = read.csv("processed/ghg_ftc.csv")

toolik = ghg_csv$Site=="TOOL"
healy = ghg_csv$Site=="HEALY"
control = ghg_csv$TRT=="CON"
ftc = ghg_csv$TRT=="FTC"
DAY
Day_1 = ghg_csv$DAY=="1"
Day_4 = ghg_csv$DAY=="4"
Day_7 = ghg_csv$DAY=="7"
Day_14 = ghg_csv$DAY=="14"

library(ggplot2)


ggplot(ghg_csv, aes(x=TRT, y=gain_ug_per_gOC, fill=DAY)) + geom_boxplot()


ggplot(ghg_csv, aes(x=Site, y=gain_ug_per_gOC, fill=DAY)) + geom_boxplot()

ggplot(ghg_csv, aes(x=Site, y=ug_13C_g_soil, fill=TRT)) + geom_boxplot()

################

ghg_aov1 = aov(data = ghg_csv, gain_ug_per_gOC ~ Site)
summary(ghg_aov1)


ghg_aov2 = aov(data = ghg_csv, gain_ug_per_gOC ~ DAY)
summary(ghg_aov2)


ghg_aov3 = aov(data = ghg_csv, gain_ug_per_gOC ~ Site*DAY)
summary(ghg_aov3)

###############


ggplot(ghg_csv, aes(x=TRT, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot()


ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot()

ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=TRT)) + geom_boxplot()
