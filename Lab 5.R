#LAB 5

#EXERCISE 11
data(iris)
head <- head(iris, n=150)
#a) It contains sepal and petal length and width.
#b) Draw histogram of lenght petals
hist(head$Petal.Length, xlab = "Lenght", ylab = "Frequency", main ="Lenght of petals", col = "green")
#Can you change the number of classes to 5 or 10?
hist(head$Petal.Length, nclass = 5, xlab = "Lenght", ylab = "Frequency", main ="Lenght of petals", col = "blue")
hist(head$Petal.Length, nclass = 10, xlab = "Lenght", ylab = "Frequency", main ="Lenght of petals", col ="red")
#With nclass = 5 the histogram looks well, however, with nclass = 10 there are some empty bars.
#c)Calculate the quartiles and interquarthyl range
# and construct box diagram of lenght of petals
head <- head(iris, n=300)
x <- sort(head$Petal.Length, decreasing = FALSE)
quartiles <- quantile(x)
quartiles
interquartile_range <- quartiles[4]-quartiles[2]
interquartile_range
boxplot(head$Petal.Length, data = iris)
#d) Boxplot of amplitude of petals
boxplot(Petal.Width~Species, data = iris)
#The vurginica species tends to have a bigger amplitude,
#in contrast of the setosa which tends to have a smaller one.

#EXERCISE 12.
#Data from table
value = c(0,1,2,4,5) 
frequencies = c(40,52,24,83,12)
table <- data.frame(Values = value, Frequency = frequencies)
table
#a)
relative_freq <- frequencies / length(frequencies)
relative_freq
cumulative_freq <- cumsum(frequencies)
cumulative_freq
table2 <- data.frame(table,relative_freq,cumulative_freq)
table2
#b)
values<- c(rep(0,40),rep(1,52),rep(2,24), rep(4,83), rep(5,12))
mean(values) # Mean
median(values) # Median
#c)
varp<-(1/(length(values)))*(sum((values - mean(values))^2))
varp #variance
var(values)# corrected variance
std<- (varp)^(1/2)
std #standard deviation
(var(values))^(1/2)

#EXERCISE 13. File motos.
data <- load("C:/Users/sara0/Downloads/motos.RData")
price <- motos$PREU
capacity <- motos$CAPACITAT
#a) Covariance and linear correlation
cov(price, capacity)
cor(price, capacity)
#b) Scatter plot
plot(price,capacity,xlab="Price",ylab="Capacity",col="blue")
#c) Regression line
dis <- motos$CC
straight <- lm (price ~ dis)
reg <- summary (straight)
plot(dis,price, xlab="Displacement",ylab="Price",col="red")
abline(straight)
summary(straight)$r.square
#d) Adjusted values and residuals
data <- data.frame(dis)
pr <- predict(straight, newdata = data, interval = "prediction")[0:5,]
round(pr,digits=2)
pr <- predict(straight, newdata = data, interval = "prediction")
residual <- price-pr
residual[0:5,]
round(residual,digits=2)[0:5]
data <- data.frame(x =c(400))
colnames(data) <- "displacement"
predicted_price <- predict(straight, newdata= data)
round(predicted_price,digits=2)


