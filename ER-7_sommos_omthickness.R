# Erin C Rooney
# SOMMOS permafrost cores 
# organic mat thickness

setwd("~/R/R/R Datasets/introductoryR-master")
processed = read.csv("processed/sommos_omthick.csv")

alldat = processed[processed$project=="sommos",]


# 1. ANOVA: use function `aov`
## 1a. one variable
# how does site influence WSOC?
alldat_aov1 = aov(data = alldat, omdepth_cm ~ site)
# anova summary table, with F and P values
summary(alldat_aov1)
print(alldat_aov1)

#########


# Convert the variable dose from a numeric to a factor variable
alldat$omdepth_cm <- as.factor(alldat)
head(omdepth_cm)

library(ggplot2)
# Basic box plot
p <- ggplot(alldat, aes(x=site, y=omdepth_cm)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(alldat, aes(x=site, y=omdepth_cm)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)

# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

############

# Use custom color palettes
p+scale_color_manual(values=c("#56B4E9", "#E69F00", "#999999"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()


###############


# Use single color
ggplot(alldat, aes(x=site, y=omdepth_cm)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

################
# Change box plot colors by groups
p<-ggplot(alldat, aes(x=site, y=omdepth_cm, fill=site)) +
  geom_boxplot()
p

# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

##############

p + scale_fill_manual(values=c("cornflowerblue", "chartreuse3", "goldenrod1"))
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

p


