#Load libraries
library(prob)
library(combinat)

#SAMPLE SPACES
#Represent 3 different outcomes of how a cup will fall
s<- data.frame(land = c("down","up","side"))
s
#Toss coin function: possibilities when tossed 3 times
tosscoin(3)
#Roll die function: possibilities when rolled once
rolldie(1, nsides = 4)
#Draw 1 card from deck of cards
head(cards(jokers = FALSE))

#SAMPLING FROM URNS / BOXES
#urnsamples(x,size,replace,ordered)
urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE)
urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE)
urnsamples(1:3, size = 2, replace = FALSE, ordered = FALSE)
urnsamples(c("red", "red", "blue"), size = 2, replace = TRUE, ordered = FALSE)

#EVENTS; Event A = subset of sample space
#makespace = include probabilities
s<-tosscoin(2, makespace= TRUE)
s[c(2,4),] #Extract rows
s<-cards()
subset(s,suit == "Heart") #Extract rows with "Heart" cards
subset(s,rank %in% 7:9) #Extract rows with rank between 7 and 9
subset(rolldie(3), X1+X2+X3 > 16) 

#DEFINE PROBABILITY SPACE
#probspace(x,probs)
#Equally likely model:every outcome has same probability
outcomes<-rolldie(1)
p<- rep(1/6, times = 6)


