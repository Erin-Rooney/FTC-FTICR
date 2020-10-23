# Erin C Rooney
# SOMMOS data

#load libraries-------------------------------
library(tidyverse)
library(PNWColors)
library(agricolae)

#load data-------------------------------------
sommos_csv = read.csv("processed/horizon_processed4.csv")
neon_barr_csv = read.csv("processed/neon_barr_biogeochem.csv")
neon_heal_csv = read.csv("processed/neon_heal_biogeochem.csv")
neon_tool_csv = read.csv("processed/neon_tool_biogeochem.csv")
neon_bona_csv = read.csv("processed/neon_bona_biogeochem.csv")



#process data------------------------------------
sommos_proc = sommos_csv %>% 
dplyr::select(site, horizon_type, midpoint_depth.cm, DC_Al.g100g, 
              DC_Fe.g100g, DC_Mn.g100g, DC_Si.g100g, SP_Al.g100g, SP_Fe.g100g, 
              SP_Mn.g100g, SP_Si.g100g, AO_Al.g100g, AO_Fe.g100g, AO_Mn.mgkg, AO_Si.g100g) %>% 
  # create columns for indices
  dplyr::mutate(AO = (AO_Fe.g100g+AO_Al.g100g),
                SP = (SP_Fe.g100g+SP_Al.g100g),
                DC = (DC_Fe.g100g+DC_Al.g100g),
                AO_DC = (AO/DC),
                SP_DC = (SP/DC))
                
                
neon_barr_proc = neon_barr_csv %>% 
  dplyr::select(siteID, plotID, biogeoCenterDepth, alKcl, 
                feKcl, nitrogenTot, estimatedOC, carbonTot, ctonRatio, acidity, OlsenPExtractable, waterSatx, alOxalate, feOxalate, alCitDithionate, 
                feCitDithionate) %>% 
  # create columns for indices
  dplyr::mutate(AO = (alOxalate+feOxalate),
                KCL = (alKcl+feKcl),
                DC = (alCitDithionate+feCitDithionate),
                AO_DC = (AO/DC),
                KCL_DC = (KCL/DC))

neon_heal_proc = neon_heal_csv %>% 
  dplyr::select(siteID, plotID, biogeoCenterDepth, alKcl, 
                feKcl, nitrogenTot, estimatedOC, carbonTot, ctonRatio, acidity, OlsenPExtractable, waterSatx, alOxalate, feOxalate, alCitDithionate, 
                feCitDithionate) %>% 
  # create columns for indices
  dplyr::mutate(AO = (alOxalate+feOxalate),
                KCL = (alKcl+feKcl),
                DC = (alCitDithionate+feCitDithionate),
                AO_DC = (AO/DC),
                KCL_DC = (KCL/DC))

neon_bona_proc = neon_bona_csv %>% 
  dplyr::select(siteID, plotID, biogeoCenterDepth, alKcl, 
                feKcl, nitrogenTot, estimatedOC, carbonTot, ctonRatio, acidity, OlsenPExtractable, waterSatx, alOxalate, feOxalate, alCitDithionate, 
                feCitDithionate) %>% 
  # create columns for indices
  dplyr::mutate(AO = (alOxalate+feOxalate),
                KCL = (alKcl+feKcl),
                DC = (alCitDithionate+feCitDithionate),
                AO_DC = (AO/DC),
                KCL_DC = (KCL/DC))

neon_tool_proc = neon_tool_csv %>% 
  dplyr::select(siteID, plotID, biogeoCenterDepth, alKcl, 
                feKcl, nitrogenTot, estimatedOC, carbonTot, ctonRatio, acidity, OlsenPExtractable, waterSatx, alOxalate, feOxalate, alCitDithionate, 
                feCitDithionate) %>% 
  # create columns for indices
  dplyr::mutate(AO = (alOxalate+feOxalate),
                KCL = (alKcl+feKcl),
                DC = (alCitDithionate+feCitDithionate),
                AO_DC = (AO/DC),
                KCL_DC = (KCL/DC))

neon_proc = 
  neon_tool_proc %>% 
  bind_rows(neon_heal_proc, neon_barr_proc, neon_bona_proc)
  

# ggplot set up-----------------------------------
theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

#
theme_erclean <- function () {
   theme_clean() %+replace%
    theme(legend.position = "top",
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
          )
 }
      
# select data-----------------------------------
# barrow = sommos_csv$site=="BARR"
# healy = sommos_csv$site=="HEAL"
# toolik = sommos_csv$site=="TOOL"
# bona = sommos_csv$site=="BONA"
# data = as.data.frame(barrow)
# data = as.data.frame(healy)
# data = as.data.frame(toolik)
# data = as.data.frame(bona)


#aov_hsd---------------------------------------

sommos_aov1 = aov(data = sommos_proc, SP_DC ~ site)
summary(sommos_aov1)

SPDC_hsd = HSD.test(sommos_aov1,"site")
print(SPDC_hsd)
print(SPDC_hsd$groups)

neon_aov1 = aov(data = neon_proc, AO_DC * ctonRatio ~ siteID)
summary(neon_aov1)

neon_aov2 = aov(data = neon_proc, ctonRatio ~ siteID*depth)
summary(neon_aov2)

ctonneon_hsd = HSD.test(neon_aov2,"siteID")
print(ctonneon_hsd)
print(ctonneon_hsd$groups)

AODCneon_hsd = HSD.test(neon_aov2,"siteID")
print(AODCneon_hsd)
print(AODCneon_hsd$groups)

neon_aov3 = aov(data = neon_proc, nitrogenTot ~ siteID*depth)
summary(neon_aov3)

ntotneon_hsd = HSD.test(neon_aov3,"siteID")
print(ntotneon_hsd)
print(ntotneon_hsd$groups)

neon_aov4 = aov(data = neon_proc, acidity ~ siteID)
summary(neon_aov4)

acidity_hsd = HSD.test(neon_aov4,"siteID")
print(acidity_hsd)
print(acidity_hsd$groups)

neon_aov5 = aov(data = neon_proc, waterSatx ~ siteID)
summary(neon_aov5)

water_hsd = HSD.test(neon_aov5,"siteID")
print(water_hsd)
print(water_hsd$groups)

neon_aov6 = aov(data = neon_proc, estimatedOC ~ siteID*depth)
summary(neon_aov6)

oc_hsd = HSD.test(neon_aov6,"depth")
print(oc_hsd)
print(oc_hsd$groups)

neon_aov6 = aov(data = neon_proc, AO_DC * waterSatx ~ siteID*depth)
summary(neon_aov6)

oc_hsd = HSD.test(neon_aov6,"siteID")
print(oc_hsd)
print(oc_hsd$groups)

#ggplots------------------------------------------------------------
neon_proc = neon_proc %>% 
  mutate(siteID = factor (siteID, levels = c("HEAL", "BONA", "BARR", "TOOL"))) %>% 
  rename(depth = biogeoCenterDepth)

library(ggthemes)
library(gapminder)

#these plots were used to determine means within certain depth ranges.

neon_tool_proc %>% 
ggplot() +
  geom_boxplot(aes(x = ctonRatio, y = biogeoCenterDepth)) +
  ylim(28, 38)+
  labs(y = "OC", x = "Depth")
  
neon_tool_proc %>% 
  ggplot() +
  geom_boxplot(aes(x = biogeoCenterDepth, y = carbonTot)) +
 # ylim(28, 38)+
  labs(y = "OC", x = "Depth")

neon_tool_proc %>% 
  ggplot() +
  geom_boxplot(aes(x = biogeoCenterDepth, y = ctonRatio)) +
  #ylim()+
  labs(y = "C:N", x = "Depth")

#These plots are combo figures from NEON metadata

ggplot(neon_proc, aes(y=depth, x=nitrogenTot, size = carbonTot, color=siteID)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1, 10), name = "AO:DC")+
  theme_erclean() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "Total Nitrogen")+
  scale_y_reverse()+
  facet_grid(.~siteID)

ggplot(neon_proc, aes(y=depth, x=ctonRatio, size = AO_DC, color=siteID)) +
  geom_point(alpha = 0.4) +
  scale_size(range = c(1, 24), name = "AO:DC")+
  theme_erclean() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "C:N Ratio")+
  scale_y_reverse()+
  facet_grid(.~siteID)

p1 = neon_proc %>% 
  ggplot() +
  geom_point(data = neon_proc, aes(y=depth, x=nitrogenTot, color=siteID)) +
  theme_erclean() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "Total Nitrogen")+
  scale_y_reverse()+
  facet_grid(. ~ siteID)


p2 = neon_proc %>% 
  ggplot(aes(y=depth, x=estimatedOC, color=siteID)) +
  geom_point() +
  #geom_smooth(span = 0.3)+
  theme_erclean() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = NULL, x = "Organic Carbon")+
  scale_y_reverse()+
  facet_grid(. ~ siteID)

# neon_proc %>% 
#   ggplot(aes(y=depth, x=nitrogenTot, color=siteID)) +
#   geom_point() +
#   geom_smooth(span = 0.3)+
#   theme_er() +
#   scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
#   labs(y = "Depth, cm", x = "Total Nitrogen")+
#   scale_y_reverse()+
#   facet_grid(. ~ siteID)

p3 = neon_proc %>% 
  ggplot(aes(y=depth, x=AO_DC, color=siteID)) +
  geom_point() +
  xlim(0, 1.5)+
  #geom_smooth(span = 0.3)+
  theme_erclean() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "AO to DC ratio")+
  scale_y_reverse()+
  facet_grid(. ~ siteID)

p4 = neon_proc %>% 
  ggplot(aes(y=depth, x=ctonRatio, color=siteID)) +
  geom_point() +
  #xlim(0, 1.5)+
  #geom_smooth(span = 0.3)+
  theme_erclean() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = NULL, x = "C:N ratio")+
  scale_y_reverse()+
  facet_grid(. ~ siteID)

library(patchwork)
p1+p2+p3+p4+ #combines the two plots
  plot_layout(guides = "collect") & theme_erclean()

#


neon_proc %>% 
  ggplot(aes(y=depth, x=ctonRatio, color=siteID)) +
  geom_point() +
  #xlim(0, 1.5)+
  #geom_smooth(span = 0.3)+
  theme_er() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "C:N ratio")+
  scale_y_reverse()+
  facet_grid(. ~ siteID)





#

neon_proc %>% 
  ggplot() +
  geom_point(data = neon_proc, aes(y=depth, x=acidity, color=siteID)) +
  theme_er() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "Acidity")+
  scale_y_reverse()+
  facet_grid(. ~ siteID)

neon_proc %>% 
  ggplot(aes(y=depth, x=waterSatx, color=siteID)) +
  geom_point() +
  #geom_smooth(span = 0.3)+
  theme_er() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "Depth, cm", x = "Water Saturation") +
  scale_y_reverse()+
  facet_grid(. ~ siteID)
  


#Sommos ggplots

sommos_proc = sommos_proc %>% 
  mutate(site = factor (site, levels = c("HEAL", "BONA", "BARR", "TOOL")))

sommos_proc %>% 
  ggplot() +
  geom_boxplot(data = sommos_proc, aes(y=AO_DC, x=site, fill=site)) +
  theme_er() +
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "AO Extractable: DC Extractable Ratio")

sommos_proc %>% 
  ggplot() +
  geom_boxplot(data = sommos_proc, aes(y=SP_DC, x=site, fill=site)) +
  theme_er() +
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay")))+
  labs(y = "SP Extractable: DC Extractable Ratio")

  
  #scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) 
  #facet_grid(. ~ site)

sommos_csv %>% 
  ggplot() +
  geom_point(data = sommos_csv, aes(y=OCC.g100g, x=LIG.ugg, color=site, size=10, shape=horizon_type)) +
  theme_er() +
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay")))
  
  
#scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) 
#facet_grid(. ~ site)

# sommos_csv %>% 
#   ggplot() +
#   geom_point(data = sommos_csv, aes(y=FLF.g100g, x=HF.g100g, color=site, size=OCC.g100g)) +
#   #scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) +
#   theme_erclean() +
#   strip.background = element_rect(colour="black", fill="black") #facet formatting
# 
#   #facet_grid(. ~ site)

###########


# 
# p <- ggplot() +
#   
#   geom_line(data = sommos_csv, aes(y=top_depth.cm, x=AO_Fe.g100g, color=site)) +
#   
#   #scale_y_continuous(trans = "reverse", breaks = (sommos_csv$top_depth.cm)) +
#   
#   scale_y_continuous()+
#   
#   scale_y_reverse +
#   
#   scale_x_continuous(position = "top")
# 
# p + facet_grid(. ~ site)



# p <- ggplot() +
#   geom_line(data = as.data.frame(barrow), aes(y = top_depth.cm, x= clay.gg, color = site))
# 
# p

#Active layer ftc plot----------------------------

# plot(activelayer_dat)
# 
# p <- ggplot() +
#   
#   geom_line(data = activelayer_dat, aes(x = Def1, y = depth_m, color = site_pos)) +
#   
#   scale_y_reverse()
#   
#  # scale_y_continuous(trans = "reverse", breaks = (activelayer_dat$depth_m))
# 
# p
# 
# p + facet_grid(. ~ site_pos)



