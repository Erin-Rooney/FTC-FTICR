# Erin Rooney
# Poisson Regression

# vector of 6 colors
colors <- c("Red", "Blue", "Gold", "Black", "Pink", "Green")

# declare a list to hold distribution values 
poisson.dist < - list()
# </code>

a < - c(1, 2, 3, 4, 5, 6) # A vector for values of u
for (i in 1:6) {
  poisson.dist[[i]] <- c(dpois(0:20, i)) # Store distribution vector for each corresponding value of u
}
# </code>

# plot each vector in the list using the colors vectors to represent each value for u
plot(unlist(poisson.dist[1]), type = "o", xlab="y", ylab = "P(y)",
     col = colors[i])
for (i in 1:6) {
  lines(unlist(poisson.dist[i]), type = "o", col = colors[i])
}


# Adds legend to the graph plotted
legend("topright", legend = a, inset = 0.08, cex = 1.0, fill = colors, title = "Values of u")

# create a sequence -3 to +3 with .05 increments
xseq < - seq(-3, 3, .05)

# generate a Probability Density Function
densities <- dnorm(xseq, 0, 1)

# plot the graph
plot(xseq, densities, col = "blue", xlab = "", ylab = "Density", type = "l", lwd = 2)
# col: changes the color of line
# 'xlab' and 'ylab' are labels for x and y axis respectively
# type: defines the type of plot. 'l' gives a line graph
# lwd: defines line width
# </code>

ppois(16, 12, lower.tail = FALSE)
# lower.tail = logical; if TRUE (default) then probabilities are P[X < = x], otherwise, P[X > x].
## [1] 0.101291

# Let's get modeling!

# get data

ftc_dat = read.csv("processed/final_dat.csv")
ftc_count = ftc_dat$Def1

# install.packages("datasets")
library(datasets) # include library datasets after installation

# generalized linear models general structure
poisson.model <- glm(Def1 ~ site_pos + depth_m, ftc_dat, family = poisson(link = "log"))
summary(poisson.model)

# generalized linear models general structure
poisson.model2 <- glm(Def1 ~ site_pos + depth_m + season, ftc_dat, family = poisson(link = "log"))
summary(poisson.model2)

# generalized linear models general structure
#poisson.model3 <- glm(Def1 ~ site_pos + depth_pos, ftc_dat, family = poisson(link = "log"))
#summary(poisson.model3)

# install.packages("datasets")
library(datasets) # include library datasets after installation

data < - warpbreaks
# </code>

columns < - names(data) # Extract column names from dataframe
columns # show columns
# </code>

ls.str(warpbreaks)

hist(data$breaks)

var(data$breaks) # calculate variance

# model poisson regression using glm()
#poisson.model < - glm(breaks ~ wool + tension, data, family = poisson(link = "log"))
#summary(poisson.model)
# </code>

# Plotting
# Install the package jtools if not already installed
install.packages("jtools")

# you may be asked to install 'broom' and 'ggstance' packages as well
install.packages("broom")
install.packages("ggstance")
installed.packages("interactions")

# Include jtools library
library(jtools)
library(ggplot2)
library(ggstance)
library(broom)
library(interactions)

# plot regression coefficients for poisson.model2
plot_summs(poisson.model2, scale = TRUE, exp = TRUE)

cat_plot(poisson.model2, pred = site_pos, modx = season) +
  labs (title = "FTC Poisson Regression") +
        xlab("Site") +
        ylab("FTC factor")
  

# argument 1: regression model
# pred: The categorical variable that will appear on x-axis
# modx: Moderator variable that has an effect in combination to pred on outcome