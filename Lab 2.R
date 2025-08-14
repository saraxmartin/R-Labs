#LAB 2

library(mosaic)
#Load data
load(url("http://www.openintro.org/stat/data/bdims.RData"))
#Create data set for men and women
mdims = subset(bdims,sex == 1)
fdims = subset(bdims,sex == 0)

#MINIHACK 1------------------------
#EXERCISE 1: Percentage of females taller than 170.2 and actual percentage
fhgtmean = mean(fdims$hgt)
fhgtsd = sd(fdims$hgt)
(1- pnorm(q = 170.2, mean = fhgtmean, sd = fhgtsd))*100 #Z score
#Proportion of women taller than 170.2
with(fdims, sum(hgt > 170.2) / length(hgt))*100
#Percentage of woman taller than 170.2
(pnorm(q = 170.2, mean = fhgtmean, sd = fhgtsd))*100
with(fdims, sum(hgt > 170.2) / length(hgt))*100

#EXERCISE 2: 25 percentile of female height
qnorm(0.25, mean = fhgtmean, sd = fhgtsd)
#Percentiles of data without assuming normality
quantile(fdims$hgt, prob=0.25, data=fdims)

#EXERCISE 3: Histogram of age of females + overlay curve
fagemean = mean(fdims$age)
fagesd = sd(fdims$age)
# X-axis grid
x <- seq(min(fdims$age), max(fdims$age), length = 40)
# Normal curve
fun <- dnorm(x, mean = fagemean, sd = fagesd)
# Histogram
hist(fdims$age, prob = TRUE, col = "white", ylim = c(0, max(fun)),main = "Women's age")
lines(x, fun, col = 2, lwd = 2) 

#EXERCISE 4: Percentage of females <=25 age
#Proportion of women younger than 25
with(fdims, sum(age <= 25) / length(age))*100
#Percentage of woman younger than 25
(pnorm(q = 25, mean = fagemean, sd = fagesd))*100


#MINIHACK 2----------------------------------
data(iris)
head(iris)

#EXERCISE 1: Normal distribution
sw_mean = mean(iris$Sepal.Width)
sw_mean
sw_sd = sd(iris$Sepal.Width)
sw_sd
# X-axis grid
x <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length = 40)
# Normal curve
fun <- dnorm(x, mean = sw_mean, sd = sw_sd)
# Histogram
hist(iris$Sepal.Width, prob = TRUE, col = "white", ylim = c(0, max(fun)),main = "Width Sepal")
lines(x, fun, col = 2, lwd = 2) 
#Maybe it is well distributed but we can see that the mean does not 
#correspond to the center of the histogram-
#The center of the historigram is the mean = 3.057333 
#and the spred is the std = 0.4358663

#EXERCISE 2: Normal model
1-pnorm(max(iris$Sepal.Width), mean = sw_mean, sd = sw_sd)

#EXERCISE 3: Extreme probabilities
pnorm(min(iris$Sepal.Width), mean = sw_mean, sd = sw_sd)

#EXERCISE 4: Normal draws
draws<-rnorm(n = 100, mean = 67, sd = 3)
hist(draws)

#EXERCISE 5: P's and Q's
qnorm(p = 0.25, mean = 67, sd = 3)

#MINIHACK 3------------------------------
require(titanic)
#Where women in titanic younger than men?
data<-titanic_train
head(data)
f = subset(data, Sex == "female")
m = subset(data, Sex == "male")
#histogram
hist(m$Age, col = "pink")
hist(f$Age, col = "black", add = TRUE)

