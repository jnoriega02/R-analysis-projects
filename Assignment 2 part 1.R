# Load the Houses data
load("Houses.RData")

# View the structure of the data
str(houses)

# Part (1): Initial Simple Linear Model

model1 <- lm(price ~ age + nbh + cbd + inst + rooms + area + land + baths, data = houses)
summary(model1)

.

# Part (2): Trying a model with quadratic terms for some continuous variables
model2 <- lm(price ~ age + nbh + cbd + inst + rooms + poly(area, 2) + poly(land, 2) + baths, data = houses)
summary(model2)


# Using a log transformation on price and other variables that could have exponential relationships.
model3 <- lm(log(price) ~ log(age) + log(nbh) + log(cbd) + log(inst) + log(rooms) + log(area) + log(land) + log(baths), data = houses)
summary(model3)

#the best model to use would be model3
