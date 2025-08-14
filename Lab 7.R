#LAB 7

#THEORY
load("bdims.RData")
bdims
mdims = subset(bdims, sex == 1)
fdims = subset(bdims, sex == 0)

#MINIHACK 1
load("malaria.RData")
malaria

#a)Find a 95% confidence interval for average age
mean_x<- mean(malaria$Edat)
sigma <- sd(malaria$Edat)
n<-120
z<-qnorm(0.975)
sprintf("The confidence interval is: (%s %s)",round(mean_x-z*sigma/sqrt(n),4), round(mean_x+z*sigma/sqrt(n),4))

#b)Calculate range interval of 92% for proportion of women and for men
m = subset(malaria, Sexe == "H")
f = subset(malaria, Sexe == "D")
p <- sum(nrow(m))/120
n <- 120
z <- qnorm(0.96)
sprintf("Supposing proportion is 92 per cent: Confidence interval for men is (%s %s)",round(p - z* sqrt(p*(1-p)/n),4), round(p + z*sqrt(p*(1-p)/n),4))
p <- sum(nrow(f))/120
sprintf("Supposing proportion is 92 per cent: Confidence interval for women is (%s %s)",round(p - z* sqrt(p*(1-p)/n),4), round(p + z*sqrt(p*(1-p)/n),4))

#c)Determine range interval of 93% for difference in average between ages of mean and women
nh <- nrow(m) #nH - number of males
nd <- nrow(f) #nD - number of females
mean_m <- mean(m$Edat)
mean_f <- mean(f$Edat)
sd_m <- sd(m$Edat) #sH
sd_f <- sd(f$Edat) #sD
std_error <- sqrt((sd_m^2/nh)+(sd_f^2/nd))
margin_error <- qnorm(0.965)*std_error
range_interval <- mean_m - mean_f
sprintf("Range interval is (%s %s)",round(range_interval-margin_error,4), round(range_interval+margin_error,4))

#d)Find a 90% confidence interval for variance of Age
library(DescTools)
age_data <- c(malaria$Edat)
VarTest(age_data, conf.level=0.90)

#MINIHACK 2

