#Lab 4 exercises

#MINIHACK 1: Find the mode
library("lsr")
?modeOf
simple_vector <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 5, 5, 6, 7)
mode <- modeOf(simple_vector) #mode of vector
mode
modal_freq <- maxFreq(simple_vector) #frequency of modal value
modal_freq
print("The mode is 2 and frequency is 5")

#MINIHACK 2: Outliers
corruption_std <- rio::import("C:/Users/sara0/Desktop/uni/probability and stadistics/RStudio/Lab 4/corruption_standardized.csv")
hist(x=corruption_std$Corruption, main= "Histogram of corruption") #histogram of corruption
#The shape of the histogram increases as the x grows higher, therefore, as the x-axis increases, the corruption value is higher. 
#The curve of the graphic gets really high at one point and then decreases almost exponentially.
corruption_std
#Outliers: Rwanda and Singapure.
corruption_std[c(0:123),] #eliminates rows 124 and 125

#MINIHACK 3: Descriptive plots
world_happiness_sim <- rio:: import("C:/Users/sara0/Desktop/uni/probability and stadistics/RStudio/Lab 4/world_happiness_2015_sim.csv")
hist(x=world_happiness_sim$Service, breaks=30)
#It seems that in this graphic there exists two maximums that are like two normal distributions combined.ç
psych::describeBy(x=world_happiness_sim$Service,group=world_happiness_sim$War)
#Both of the means correspond to the max vars (0.26 and 0.7).

#MINIHACK 4: Correlations
#EX1
world_happiness<- rio::import("C:/Users/sara0/Desktop/uni/probability and stadistics/RStudio/Lab 4/world_happiness_2015.csv")
world_happiness
world_happiness_num<-world_happiness[,c(2:8)]
#EX2
cor(x=world_happiness_num,use="complete.obs")
#EX3
cor(world_happiness_num$Happiness, world_happiness_num$Freedom,use="complete.obs")
cor(world_happiness_num$Corruption, world_happiness_num$Freedom,use="complete.obs")
#We can see those countries with low freedom have high corruption (as none fights for their rights). However, those with freedom are happier.
#EX4
cor(world_happiness_num, use="pairwise.complete.obs")
#EX5
?cor
#The complete.obs and pairwise are different.
#When using 'complete.obs' the missing values are handled by case wise deletion (if no complete cases-> error). 
#On the other hand, the 'pairwise.complete.obs' variable the correlation between each pair of variables is computed using all 
#complete pairs of observations on those variables.
#EX6
cor(world_happiness_num$GDP, world_happiness_num$Happiness,use="complete.obs")
cor(world_happiness_num$GDP, world_happiness_num$Happiness,use="pairwise.complete.obs")
#The correlation in both of the previous cases are the same 0.8193273 = 0.82
#WE DID NOT LEARN HOW TO WORK WITH IN-LINE R CODE




