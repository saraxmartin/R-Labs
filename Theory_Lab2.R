# THEORY OF LAB 2
library(mosaic)
#Load data
load(url("http://www.openintro.org/stat/data/bdims.RData"))
head(bdims)
#Create data set for men and women
mdims = subset(bdims,sex == 1)
fdims = subset(bdims,sex == 0)
head(bdims)
#Histogram
hist(mdims$hgt, col = "white", main = "Men's Height in cm")
hist(fdims$hgt, col = "white", main = "Women's Height in cm")
#Normal distribution------
mhgtmean = mean(mdims$hgt)
mhgtsd = sd(mdims$hgt)
mhgtmean
mhgtsd
# X-axis grid
x <- seq(min(mdims$hgt), max(mdims$hgt), length = 40)
# Normal curve
fun <- dnorm(x, mean = mhgtmean, sd = mhgtsd)
# Histogram
hist(mdims$hgt, prob = TRUE, col = "white", ylim = c(0, max(fun)),main = "Men's Height in cm")
lines(x, fun, col = 2, lwd = 2) 
#Normal probabilities------
#Z score
1- pnorm(q = 182, mean = mhgtmean, sd = mhgtsd)
#Probability + graph
xpnorm(q = 182, mean = mhgtmean, sd = mhgtsd)
#Proportion of men taller than 182
with(mdims, sum(hgt > 182) / length(hgt))
#Percentage of men shorter than 165.1
pnorm(q = 165.1, mean = mhgtmean, sd = mhgtsd)
with(mdims, sum(hgt < 165.1) / length(hgt))
#Normal percentiles---------
#Find 90th percentile of male height
qnorm(0.9, mean = mhgtmean, sd = mhgtsd)
#Graphic
xqnorm(0.9, mean = mhgtmean, sd = mhgtsd)
#Percentiles of data without assuming normality
quantile(mdims$hgt, prob=0.9, data=mdims)
