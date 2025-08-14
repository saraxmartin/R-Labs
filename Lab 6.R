#LAB 6 - Confidence intervals
# How to find confidence intervals for the best known statisticians.

#1. Confidence intervals for the MEAN of a population X with Normal Distribution
#   N(mean,std)
#   1.1 - Std known
std <- 21
mean_x <- 110
n <- 144 #size of sample
z <- qnorm(0.975) #confidence 95%
c(mean_x-z*std/sqrt(n), mean_x+z*std/sqrt(n))
#   1.2 - Std unknown (confidence 90%)
x <- c(3,7,11,0,7,0,4,5,6,2) #data
n <- length(x) #size of sample
c(mean(x)-qt(0.95,n-1)*sd(x)/sqrt(n),mean(x)+qt(0.95,n-1)*sd(x)/sqrt(n))
#   1.3 - Using t.test() function
t.test(x, conf.level = 0.90)$conf.int # x is vector of data

#2. Confidence intervals for the VARIANCE of a population with Normal Distribution
#   Option 1 (confidence 99%)
S <- sqrt(138098.5)
n <- 17
chisqval_inf <- qchisq(0.005,n-1)
chisqval_sup <- qchisq(0.995,n-1)
c((n-1)*S^2/chisqval_sup , (n-1)*S^2/chisqval_inf)
#   Option 2 - VarTest() function
library(DescTools)
duration <- c(1470, 1510, 1690, 1740, 1900, 2000, 2030,
              2010, 2190, 2200, 2290, 2380, 2390, 2480,
              2500, 2580, 2700)
VarTest(duration, conf.level=0.99)

#3. Confidence intervals for the PROPORTION of a population with Binomial Distribution 
#   Option 1 (confidence 99%)
p <- 319/1100
n <- 1100
z <- qnorm(0.995)
c(p-z*sqrt(p*(1-p)/n), p+z*sqrt(p*(1-p)/n))
#   Option 2 - prop.test() function
prop.test(319, 1100, conf.level = 0.99, correct = FALSE)$conf.int

# MINIHACKS..........................................................













