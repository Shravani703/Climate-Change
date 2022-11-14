parole = read.csv("parole.csv")

nrow(parole)

table(parole$Violator)



set.seed(1234) #random number generator, which is useful for creating simulations or random objects that can be reproduced.
library(caTools) #Contains several basic utility functions including: LogitBoost classifier
split = sample.split(parole$Violator, SplitRatio = 0.7) #Split data from vector Y into two sets in predefined ratio.
train = subset(parole, split == TRUE) #Return subsets of vectors, or data frames which meet conditions.
test = subset(parole, split == FALSE)

model = glm(Violator~., data=train, family="binomial") #generalized linear model (family is a description of the error distribution and link function to be used in the model.)
summary(model)

# Calculate log odds
male=1 
race=1
age=50
state2=0
state3=0
state4=0
time.served=3
max.sentence=12
multiple.offenses=0
crime2=1
crime3=0
crime4=0
logodds =-2.671322 + 0.579944*male -1.039219*race 0.005085*age + 0.423908*state2 + 0.070880*state3 -3.892343*state4  -0.048695*time.served + 0.041600*max.sentence +  1.612907*multiple.offenses -0.111354*crime2 + 1.106512*crime3 + 0.159530*crime4
logodds
# Calculate Probability
1/(1 + exp(-logodds))


predictions = predict(model, newdata=test, type="response")  #type: type of prediction (response or model term).
z = table(test$Violator, predictions > 0.5) # Creates confusion matrix for t=0.5


# Calculate accuracy
sum(diag(z))/sum(z)

#baseline
z = table(test$Violator)
#179/202 = 0.89.


