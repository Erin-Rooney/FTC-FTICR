#ECRooney
#2021 12 6
#soil properties and texture
#fticr manuscript

#load libraries-------------------------------
source("code/0-method-packages.R")


#load data-------------------------------------
properties = read.csv("processed/2021_12_6_shl_soilproperties.csv")
texture = read.csv("processed/2021_12_6_shl_soiltexture.csv")

#dataprocessing table---------------------

texture_long = 
  texture %>% 
  pivot_longer(-c(Customer.ID, Site, Trtmt, Rep, depth_top_cm, depth_bottom_cm, 
                  Morph, Material, labID, grav_moist), names_to = "particle_size", 
                  values_to = "data") %>% 
  mutate (particle_size = factor(particle_size, levels = c("Clay_.2mm", "Silt_2to20mm",
                                                           "Silt_20to50mm", "Sand_50to100mm",
                                                           "Sand_100to250mm", "Sand_250to500mm",
                                                           "Sand_500to1000mm", "Sand_1000to2000mm"))) %>%
  na.omit() %>% 
  group_by(Site, Material, particle_size) %>% 
  dplyr::summarise(mean = round(mean(data), 2),
                   se = round(sd(data)/sqrt(n()),2)) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  dplyr::select(-mean, -se)


texture_simplified_long_a = 
  texture %>% 
  pivot_longer(-c(Customer.ID, Site, Trtmt, Rep, depth_top_cm, depth_bottom_cm, 
                  Morph, Material, labID, grav_moist), names_to = "particle_size", 
               values_to = "data") %>% 
  # mutate (particle_size = factor(particle_size, levels = c("Clay_.2mm", "Silt_2to20mm",
  #                                                          "Silt_20to50mm", "Sand_50to100mm",
  #                                                          "Sand_100to250mm", "Sand_250to500mm",
  #                                                          "Sand_500to1000mm", "Sand_1000to2000mm"))) %>%
  na.omit() %>% 
  group_by(Site, Material, particle_size) %>% 
  dplyr::summarise(mean = round(mean(data), 2),
                   se = round(sd(data)/sqrt(n()),2)) %>% 
 # mutate(summary = paste(mean, "\u00b1", se)) %>% 
  #dplyr::select(-mean, -se) %>% 
  ungroup() 

texture_simplified_long_b = 
  texture_simplified_long_a %>% 
  mutate(particle_size_simple = case_when(grepl("Clay", particle_size)~"clay",
                                          grepl("Silt", particle_size)~"silt",
                                          grepl("Sand", particle_size)~"sand")) %>%
  mutate (particle_size_simple = factor(particle_size_simple, levels = c("sand", "silt", "clay"))) %>% 
  group_by(Site, Material, particle_size_simple) %>% 
  dplyr::summarise(sum = round(sum(mean), 2),
                   se = round(sum(se),2)) %>% 
  mutate(summary = paste(sum, "\u00b1", se)) %>% 
  dplyr::select(-sum, -se)


#clean

texture_simplified_long_b %>% knitr::kable() # prints a somewhat clean table in the console

#export

write.csv(texture_simplified_long_b, "output/texture_simple_long.csv", row.names = FALSE)


properties_long = 
  properties %>%
  select(-c(Customer.ID, Lab.ID)) %>% 
  mutate (material = factor(material, levels = c("Organic", "Upper Mineral",
                                                           "Lower Mineral"))) %>%
    pivot_longer(-c(Site, Trtmt, Rep, depth_top, depth_bottom,
                  morph, material), names_to = "pH_EC",
               values_to = "data") %>%
  mutate (pH_EC = factor(pH_EC, levels = c("pH", "EC",
                                           "Gravimetric.Moisture"))) %>%
   na.omit() %>% 
  group_by(Site, material, pH_EC) %>% 
  dplyr::summarise(mean = round(mean(data), 2),
                   se = round(sd(data)/sqrt(n()),2)) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  dplyr::select(-mean, -se)

#clean

properties_long %>% knitr::kable() # prints a somewhat clean table in the console

#export

write.csv(properties_long, "output/properties_long.csv", row.names = FALSE)

  
#ggplot----------

texture_long %>% 
  mutate(Site = recode(Site, "TOOL" = "Toolik",
                       "HEAL" = "Healy")) %>% 
  ggplot(aes(x = Material, y = data, fill = Site))+
  labs(x = " ",
       y = "%")+
  #geom_bar(aes(fill = particle_size), stat = "identity")+
  geom_boxplot()+
  scale_fill_manual(values = (pnw_palette("Shuksan2",2)))+
  facet_wrap(. ~ particle_size)+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA), 
        axis.text.x.bottom = element_text (vjust = 0.5, hjust=1, angle = 90)
        
  )+
  NULL
