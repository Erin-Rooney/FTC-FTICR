#xrd diffractogram patterns
# EC Rooney
# Feb 9 2022

source("code/0-method-packages.R")

library(rxylib)
library(magrittr)
library(utils)

# 
# H12433data = read_xyData("processed/H1-24-33.xy")
# H13350data = read_xyData("processed/H1-33-50.xy")
# H15060data = read_xyData("processed/H1-50-60.xy")
# H22834data = read_xyData("processed/H2-28-34.xy")
# H23447data = read_xyData("processed/H2-34-47.xy")
# H24768data = read_xyData("processed/H2-47-68.xy")
# H34050data = read_xyData("processed/H3-40-50.xy")
# H33038data = read_xyData("processed/H3-30-38.xy")
# T14060data = read_xyData("processed/T1-40-60.xy")
# T16067data = read_xyData("processed/T1-60-67.xy")
# T22838data = read_xyData("processed/T2-28-38.xy")
# T23844data = read_xyData("processed/T2-38-44.xy")
# T24458data = read_xyData("processed/T2-44-58.xy")
# T33541data = read_xyData("processed/T3-35-41.xy")
# T34150data = read_xyData("processed/T3-41-50.xy")
# T35058data = read_xyData("processed/T3-50-58.xy")
# 
# "H1_24_33dataframe" = data.frame(H12433data[["dataset"]][[1]][["data_block"]])
# "H1_33_50dataframe" = data.frame(H13350data[["dataset"]][[1]][["data_block"]])
# "H1_50_60dataframe" = data.frame(H15060data[["dataset"]][[1]][["data_block"]])
# "H2_28_34dataframe" = data.frame(H22834data[["dataset"]][[1]][["data_block"]])
# "H2_34_47dataframe" = data.frame(H23447data[["dataset"]][[1]][["data_block"]])
# "H2_47_68dataframe" = data.frame(H24768data[["dataset"]][[1]][["data_block"]])
# "H3_40_50dataframe" = data.frame(H34050data[["dataset"]][[1]][["data_block"]])
# "H3_30_38dataframe" = data.frame(H33038data[["dataset"]][[1]][["data_block"]])
# "T1_40_60dataframe" = data.frame(T14060data[["dataset"]][[1]][["data_block"]])
# "T1_60_67dataframe" = data.frame(T16067data[["dataset"]][[1]][["data_block"]])
# "T2_28_38dataframe" = data.frame(T22838data[["dataset"]][[1]][["data_block"]])
# "T2_38_44dataframe" = data.frame(T23844data[["dataset"]][[1]][["data_block"]])
# "T2_44_58dataframe" = data.frame(T24458data[["dataset"]][[1]][["data_block"]])
# "T3_35_41dataframe" = data.frame(T33541data[["dataset"]][[1]][["data_block"]])
# "T3_41_50dataframe" = data.frame(T34150data[["dataset"]][[1]][["data_block"]])
# "T3_50_58dataframe" = data.frame(T35058data[["dataset"]][[1]][["data_block"]])
# 
# 
# soils = list('H1_24_33dataframe', "H1_33_50dataframe")
# 
# soils <- list(name = subset('H1_24_33dataframe', V1 = V1, V2 = V2))

## OUTPUTS 1
# write.csv(H1_24_33dataframe,"output/H1_24_33dataframe.csv", row.names = FALSE)
# write.csv(H1_33_50dataframe,"output/H1_33_50dataframe.csv", row.names = FALSE)
# write.csv(H1_50_60dataframe,"output/H1_50_60dataframe.csv", row.names = FALSE)
# write.csv(H2_28_34dataframe,"output/H2_28_34dataframe.csv", row.names = FALSE)
# write.csv(H2_34_47dataframe,"output/H2_34_47dataframe.csv", row.names = FALSE)
# write.csv(H2_47_68dataframe,"output/H2_47_68dataframe.csv", row.names = FALSE)
# write.csv(H3_40_50dataframe,"output/H3_40_50dataframe.csv", row.names = FALSE)
# write.csv(H3_30_38dataframe,"output/H3_30_38dataframe.csv", row.names = FALSE)
# write.csv(T1_40_60dataframe,"output/T1_40_60dataframe.csv", row.names = FALSE)
# write.csv(T1_60_67dataframe,"output/T1_60_67dataframe.csv", row.names = FALSE)
# write.csv(T2_28_38dataframe,"output/T2_28_38dataframe.csv", row.names = FALSE)
# write.csv(T2_38_44dataframe,"output/T2_38_44dataframe.csv", row.names = FALSE)
# write.csv(T2_44_58dataframe,"output/T2_44_58dataframe.csv", row.names = FALSE)
# write.csv(T3_35_41dataframe,"output/T3_35_41dataframe.csv", row.names = FALSE)
# write.csv(T3_41_50dataframe,"output/T3_41_50dataframe.csv", row.names = FALSE)
# write.csv(T3_50_58dataframe,"output/T3_50_58dataframe.csv", row.names = FALSE)


##start here in re-runs of this code-------------------------


# H1_24_33dataframe2 = read.csv("output/H1_24_33dataframe.csv") %>%
#   mutate(site = 'Healy',
#        rep = "1",
#        depth = "24-33",
#        material = "upper mineral")
# 
# H1_33_50dataframe2 = read.csv("output/H1_33_50dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "1",
#          depth = "33-50",
#          material = "upper mineral")
# 
# H1_50_60dataframe2 = read.csv("output/H1_50_60dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "1",
#          depth = "50-60",
#          material = "lower mineral")
# 
# H2_28_34dataframe2 = read.csv("output/H2_28_34dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "2",
#          depth = "28-34",
#          material = "upper mineral")
# 
# H2_34_47dataframe2 = read.csv("output/H2_34_47dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "2",
#          depth = "34-47",
#          material = "upper mineral")
# 
# H2_47_68dataframe2 = read.csv("output/H2_47_68dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "2",
#          depth = "47-68",
#          material = "lower mineral")
# 
# H3_40_50dataframe2 = read.csv("output/H3_40_50dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "3",
#          depth = "40-54",
#          material = "lower mineral")
# 
# H3_30_38dataframe2 = read.csv("output/H3_30_38dataframe.csv") %>% 
#   mutate(site = 'Healy',
#          rep = "3",
#          depth = "30-38",
#          material = "upper mineral")
# 
# T1_40_60dataframe2 = read.csv("output/T1_40_60dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "1",
#          depth = "40-60",
#          material = "upper mineral")
# 
# T1_60_67dataframe2 = read.csv("output/T1_60_67dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "1",
#          depth = "60-67",
#          material = "lower mineral")
# 
# T2_28_38dataframe2 = read.csv("output/T2_28_38dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "2",
#          depth = "28-38",
#          material = "upper mineral")
# 
# T2_38_44dataframe2 = read.csv("output/T2_38_44dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "2",
#          depth = "38-44",
#          material = "upper mineral")
# 
# T2_44_58dataframe2 = read.csv("output/T2_44_58dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "2",
#          depth = "44-58",
#          material = "lower mineral")
# 
# T3_35_41dataframe2 = read.csv("output/T3_35_41dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "3",
#          depth = "35-41",
#          material = "upper mineral")
# 
# T3_41_50dataframe2 = read.csv("output/T3_41_50dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "3",
#          depth = "41-50",
#          material = "upper mineral")
# 
# T3_50_58dataframe2 = read.csv("output/T3_50_58dataframe.csv") %>% 
#   mutate(site = 'Toolik',
#          rep = "3",
#          depth = "50-58",
#          material = "lower mineral")




# allxrddata = 
#   rbind(H1_24_33dataframe2, H1_33_50dataframe2, H1_50_60dataframe2,
#         H2_28_34dataframe2, H2_34_47dataframe2, H2_47_68dataframe2,
#         H3_40_50dataframe2, H3_30_38dataframe2, T1_40_60dataframe2,
#         T1_60_67dataframe2, T2_28_38dataframe2, T2_38_44dataframe2, 
#         T2_44_58dataframe2, T3_35_41dataframe2, T3_41_50dataframe2,
#         T3_50_58dataframe2) %>% 
#   dplyr::mutate(intensity = as.numeric(V1),
#                 peaks = as.numeric(V2),
#                 site = as.factor(site),
#                 rep = as.factor(rep),
#                 depth = as.factor(depth),
#                 material = as.factor(material)) %>% 
#   select(-V1, -V2)
# 
# write.csv(allxrddata, "output/allxrddata.csv")

################################

allxrddata = read.csv("output/allxrddata.csv")

library(powdR)

# allxrddata_Healy = 
#   allxrddata %>% 
#   filter(site %in% "Healy")


# allxrddata_Healy %>% 
#   ggplot()+
#   #geom_point(aes(y = 'intensity', x = 'peaks')) +
#   geom_line(aes(y = intensity, x = peaks, color = depth), group = 'depth') +
#   #geom_histogram(aes(x = peaks, color = depth), alpha = 0.5, position = "identity", binwidth = 0.1)+
#   xlim(0, 1000)+
#   #ylim(0, 175)+
#   coord_trans(y = "log10")+
#   labs(x = "2-Theta",
#        y = "intensity, counts")+
#   facet_grid(material ~ rep)+
#   theme_er()

data(minerals)
data(soils)



H1_24_33dataframe3 = read.csv("output/H1_24_33dataframe.csv") 
H1_33_50dataframe3 = read.csv("output/H1_33_50dataframe.csv") 
H1_50_60dataframe3 = read.csv("output/H1_50_60dataframe.csv") 
H2_28_34dataframe3 = read.csv("output/H2_28_34dataframe.csv") 
H2_34_47dataframe3 = read.csv("output/H2_34_47dataframe.csv")
H2_47_68dataframe3 = read.csv("output/H2_47_68dataframe.csv") 
H3_40_50dataframe3 = read.csv("output/H3_40_50dataframe.csv") 
H3_30_38dataframe3 = read.csv("output/H3_30_38dataframe.csv") 
T1_40_60dataframe3 = read.csv("output/T1_40_60dataframe.csv") 
T1_60_67dataframe3 = read.csv("output/T1_60_67dataframe.csv") 
T2_28_38dataframe3 = read.csv("output/T2_28_38dataframe.csv") 
T2_38_44dataframe3 = read.csv("output/T2_38_44dataframe.csv") 
T2_44_58dataframe3 = read.csv("output/T2_44_58dataframe.csv") 
T3_35_41dataframe3 = read.csv("output/T3_35_41dataframe.csv") 
T3_41_50dataframe3 = read.csv("output/T3_41_50dataframe.csv") 
T3_50_58dataframe3 = read.csv("output/T3_50_58dataframe.csv")

healysoilslist = list(Rep1_24_33 = H1_24_33dataframe3, Rep1_33_50 = H1_33_50dataframe3, Rep1_50_60 = H1_50_60dataframe3,
                 Rep2_28_34 = H2_28_34dataframe3, Rep2_34_47 = H2_34_47dataframe3, Rep2_47_68 = H2_47_68dataframe3,
                 Rep3_40_50 = H3_40_50dataframe3, Rep3_30_38 = H3_30_38dataframe3)

healysoilslist2 = as_multi_xy(healysoilslist)


quartz <- data.frame(tth = minerals$tth,
                     counts = minerals$xrd$QUA.1)


plot(healysoilslist2, wavelength = "Cu",
     xlim = c(0,100),
     normalise = FALSE)+
  #text(x = 26, y = 0.9, "quartz")+
  labs(y = 'intensity (counts)',
       color="replicate and depth, cm")+
  theme_er()

healysoils = 
  plot(healysoilslist2, wavelength = "Cu",
       xlim = c(0,100),
       normalise = FALSE)+
  #text(x = 26, y = 0.9, "quartz")+
  labs(y = 'intensity (counts)',
       color="replicate and depth, cm")+
  theme_er()



T1_40_60dataframe3 = read.csv("output/T1_40_60dataframe.csv") 
T1_60_67dataframe3 = read.csv("output/T1_60_67dataframe.csv") 
T2_28_38dataframe3 = read.csv("output/T2_28_38dataframe.csv") 
T2_38_44dataframe3 = read.csv("output/T2_38_44dataframe.csv") 
T2_44_58dataframe3 = read.csv("output/T2_44_58dataframe.csv") 
T3_35_41dataframe3 = read.csv("output/T3_35_41dataframe.csv") 
T3_41_50dataframe3 = read.csv("output/T3_41_50dataframe.csv") 
T3_50_58dataframe3 = read.csv("output/T3_50_58dataframe.csv")

tooliksoilslist = list(Rep1_40_60 = T1_40_60dataframe3, Rep1_60_67 = T1_60_67dataframe3, Rep2_28_38 = T2_28_38dataframe3,
                      Rep2_38_44 = T2_38_44dataframe3, Rep2_44_58 = T2_44_58dataframe3, Rep3_35_41 = T3_35_41dataframe3,
                      Rep3_41_50 = T3_41_50dataframe3, Rep3_50_58 = T3_50_58dataframe3)

tooliksoilslist2 = as_multi_xy(tooliksoilslist)

plot(tooliksoilslist2, wavelength = "Cu",
     xlim = c(0,100),
     normalise = FALSE)+
  labs(y = 'intensity (counts)',
       color="replicate and depth, cm")+
  theme_er()

tooliksoils = 
  plot(tooliksoilslist2, wavelength = "Cu",
       xlim = c(0,100),
       normalise = FALSE)+
  labs(y = 'intensity (counts)',
       color="replicate and depth, cm")+
  theme_er()


ggsave("output/healyxrddiffracto.tiff", plot = healysoils, height = 5, width = 8.5)
ggsave("output/toolikxrddiffracto.tiff", plot = tooliksoils, height = 5, width = 8.5)








#need to make a list of each sample with V1 and V2. 
#V1 and V2 may need to be switched

aligned <- align_xy(H1_24_33dataframe2,
                    std = quartz,
                    xmin = 10,
                    xmax = 60,
                    xshift = 0.2)
#replot data
plot(aligned, wavelength = "Cu",
     xlim = c(26,27),
     normalise = TRUE)

