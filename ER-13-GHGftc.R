# Erin Rooney
# Adapted from KP
# July 23 2020
# GHG FTC C12 and C13 data


ghg_csv = read.csv("processed/ghg_ftc.csv")

toolik = ghg_csv$Site=="TOOL"
healy = ghg_csv$Site=="HEALY"
control = ghg_csv$TRT=="CON"
ftc = ghg_csv$TRT=="FTC"
Day
Day_1 = ghg_csv$Day=="1"
Day_4 = ghg_csv$Day=="4"
Day_7 = ghg_csv$Day=="7"
Day_14 = ghg_csv$Day=="14"

library(ggplot2)


ggplot(ghg_csv, aes(x=TRT, y=ug_13C_g_soil, fill=DAY)) + geom_boxplot()


ggplot(ghg_csv, aes(x=Site, y=ug_13C_g_soil, fill=DAY)) + geom_boxplot()

ggplot(ghg_csv, aes(x=Site, y=ug_13C_g_soil, fill=TRT)) + geom_boxplot()


ggplot(ghg_csv, aes(x=TRT, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot()


ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=DAY)) + geom_boxplot()

ggplot(ghg_csv, aes(x=Site, y=ug_12C_g_soil, fill=TRT)) + geom_boxplot()
