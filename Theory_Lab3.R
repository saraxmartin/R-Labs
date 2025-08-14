#THEORY ------------------------------------------------
#1. SIMULATION CONTINUOUS RANDOM VARIABLES

#1.1. Simulate sample of size n = 1000 of X-N(40,9)
x<-rnorm (1000,40,3)

#1.2 Density function representation - draw probability mass function
#Define 2 vectors: vector with values of variable and vector with probabilities
x<- seq (20,60, by = 0.05)
y<- dnorm(x,40,3)
plot (x, y, type = "l", lwd = 2, bty = "n", las = 1, xlim = c(20,60), ylim = c(0,0.15), xaxp = c(0,80,8), col = "orange", xlab = "x", ylab = "f(x)")
grid ()

#1.3 Representation of distribution function
x<- seq(20,60, by= 0.05)
y<- pnorm(x,40,3)
plot (x, y, type = "l", bty = "n", las = 1, xlim = c(20,60), ylim = c(0,1), xaxp = c(20,60,4), col = "orange", main = "", xlab = "x", ylab = "F(x)")
grid ()
#Calculate probabilities
pnorm(45,40,3) #P(X<=45)
1-pnorm(35,40,3) #P(X>=35)=1-P(X<35)
qnorm(0.8,40,3) #P(X<=y)=0.8


#2. SIMULATION OF UNKNOWN VARIABLES

#2.1. Random number simulation -> inverse function method
u <- runif(10) #Get random n numbers
y <- (7*u+1)^{1/3}-1 #calculate distribution function F^-1(u) = sqrt3(7u+1)-1
#draw histogram with drawing of density function
hist(y, freq = FALSE) 
z <- seq(0,1,by=0.05)
t <- 3/7*(z+1)^2
lines(z,t)

#2.2. Density function representation
#Since f(x) = 0 if x < 0 or if x > 1, the sequence we need is between 0 and 1
x <- seq(0,1,by=0.05)
y <- (3/7)*(x+1)^2
plot(x,y,type="l")
#or
x <- seq(0,1,by=0.05)
f <- function(x){
  fx <- (3/7)*(x+1)^2
  fx
}
plot(x,f(x), type="l")

#2.3. Representation of distribution function
x <- seq (0,1,by =0.05)
y <- (1/7)*(x+1)^3-1/7
plot (x,y,type ="l")
#or
x <- seq (0,1,by =0.05)
F <- function(x){
  Fx <- (1/7)*(x+1)^3-1/7
  Fx
}
plot(x,F(x), type="l")

#2.4. Probability calculation
(1/7)*(0.3+1)^3-1/7 #P(X ≤ 0.3)
F(0.3) #P(X ≤ 0.3)
1-(1/7)*(0.8+1)^3+1/7 #P(X ≥ 0.8) = 1−P(X < 0.8)
1-F(0.8) #P(X ≥ 0.8) = 1−P(X < 0.8)
