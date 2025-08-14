#Lab 3: Central limit theorem

#EXERCISE 1: Prove the central limit theorem using some variables with Bernoulli distribution.
# Generate N x B matrix of Bernoulli samples with parameter p = 0.5
A <- matrix(rbinom(200*400, size=1, prob=0.5), nrow = 200, ncol = 400)
# Calculate list of N averages of the N rows of A
a <- apply(A,1,mean)
# Calculate theoretical mean and variance of the Bernoulli distribution given p = 0.5
mean <- 0.5
sd <- 0.5 *(1-0.5)
# Center and normalize the mean vector
a_norm <- (a-mean)/sqrt(sd/400)
# Draw histogram of normalized mean vector with normal density curve
hist(a_norm, freq = FALSE, main = "Histogram of Normalized Mean Vector")
curve(dnorm(x, mean=0, sd=1), add = TRUE, col = "blue")
#Need to increase the N for a better approximation) (ANSWER!)

#EXERCISE 2: X-Exp(lambda = 0.3)
#a) Draw the density function and the distribution function of X.
lambda <- 0.3
x <- seq(0,20,by=0.1)
y <- dexp(x,rate = lambda)
F <- pexp (x, rate = lambda)
plot(x,y,type="l", main="Density Distribution of X")
lines(x,F, col="red")
#b) Calculate the specified probabilities
p1 <- dexp(5, rate = lambda)
p2 <- 1 - pexp(1.7, rate = lambda)
p3 <- pexp(7, rate = lambda) - pexp(2, rate = lambda)
p4 <- (1 - pexp(3, rate = lambda)) / (1 - pexp(10, rate = lambda))
p1
p2
p3
p4
#c) Generate 500 random numbers and calculate mean and variance of each column
X <- rexp(500, rate = lambda)
X_matrix <- matrix(X, nrow = 50, ncol = 10)
colMeans_X <- colMeans(X_matrix) #mean of each column
colMeans_X
var_X <- apply(X_matrix, 2, var) #variance of each column
var_X
mean <- 1/lambda #general mean
mean
variance <- 1/lambda^2 #general variance
variance

#EXERCISE 3: X is continuous random variable with density function f(x)=(5/32)x^4, O<x<2
#a) Represent density function. Calculate and represent distribution function.
#density function
x <- seq(0,2,by=0.05)
f <- function(x){
  fx <- (5/32)*x^4
  fx
}
plot(x,f(x), type="l")
#distribution function
x <- seq (0,2,by =0.05)
F <- function(x){
  Fx <- (1/32)*x^5 #integrated function
  Fx
}
plot(x,F(x), type="l")
#b) Simulate n=500 values of X and draw the histogram. Overlay the drawing of the density function.
x <- (32/5 *runif(500))^(1/5)
hist(x, freq=FALSE)
curve((5/32)*x^4, add = TRUE, col = "red")
#c) Calculate (theoretically) the mean and variance of X . 
mean <- integrate(function(x) x * (5/32)*x^4, 0, 2)$value
mean
mean_2 <- integrate(function(x) x^2 * (5/32)*x^4, 0, 2)$value
variance <- mean_2 - mean
variance
#Calculate the empirical mean and variance. 
empirical_mean <- mean(x)
empirical_variance <- var(x)
empirical_mean
empirical_variance
#Did you get a good approximation? For the mean yes but for the variance no.
#d) P(X < t) = 0.85. Calculate t.
t <- qexp(0.85, rate = 32/5)
t
sum(x < t) #number of simulations
#The number of simulations that have a value less than t is 425, 
#which is approximately equal to 0.85 times the total number of simulations. 
#This is what we expected, since t is the 85th percentile of the exponential distribution 
#with the given parameters, and P(X < t) = 0.85.

#EXERCISE 4: μ = 298, σ = 3
#a) What is the probability that a bottle contains less than 295 ml?
p <- pnorm(295, mean = 298, sd = 3)
p
#b) What is the amount of m of soda such that the probability of more than m ml of soda is 90%?
m <- qnorm(0.9, mean = 298, sd = 3, lower.tail = FALSE)
m
#c)
p <- 1 - pbinom(1, size = 6, prob = pnorm(295, mean = 298, sd = 3))
p
