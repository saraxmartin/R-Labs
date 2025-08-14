#LAB 4 Theory

#Load libraries
library(rio)
library(psych)
#Load data
world_happiness <- rio::import("C:/Users/sara0/Desktop/uni/probability and stadistics/RStudio/Lab 4/world_happiness_2015.csv")
#Visualize data set
str(world_happiness)
head(world_happiness)

#VISUALIZING DISTRIBUTIONS
hist(x = world_happiness$Happiness, main = "Histogram of Happiness Scores", xlab = "Happiness")
boxplot(x = world_happiness$Happiness, main = "Boxplot of Happiness Scores")
boxplot.stats #to identify outliers

#BASIC DESCRIPTIVES
#Measures of central tendency
simple_vector <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 5, 5, 6, 7)
simple_vector_missing <- c(1, 1, NA, 1, 2, 2, 2, 2, 2, 3, 5, NA, 5, 6, 7)
mean(simple_vector) #mean
mean(simple_vector_missing, na.rm = TRUE) #mean with Na
median(simple_vector) #median
#Measures of variability
range(simple_vector) #range
var(simple_vector) #variance
sd(simple_vector) #standard deviation
var(simple_vector_missing, na.rm = TRUE) #var with Na
sd(simple_vector_missing, na.rm = TRUE) #sd with Na
#Outliers
simple_vector_outlier <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 5, 5, 6, 7, 25)
boxplot(simple_vector_outlier) #we can simply visualize it
boxplot.stats(simple_vector_outlier)$out #identify specific outliers
#   Z-score: standardize scores to identify outliers
as.numeric(scale(simple_vector_outlier))

#SUMMARIZING DATA
psych::describe(world_happiness) #summarized statistics (all of the above)
  #variables with * are non-numerical (categorical) -> not good statistics

#BIVARIATE DESCRIPTIVES (relation between 2 variables)
#correlation
cor(world_happiness$Happiness, world_happiness$Support, use = "pairwise.complete.obs")
#scatterplots
plot(world_happiness$Support, world_happiness$Happiness, xlab = "Social Support", ylab = "Happiness", main = "Relationship between social support and happiness \nat the country level")
#matrix with scatterplots (for not so many variables)
pairs.panels(world_happiness, lm=T, ellipses = F)
#covariance
cov(world_happiness$Happiness, world_happiness$Support, use = "pairwise.complete.obs")

#IN-LINE R CODE -> RMarkdown
