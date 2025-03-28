# Load the LawSchools data
load("LawSchools.RData")

# View the structure of the data
str(schools)

# Part (a): Simple Linear Regression between Rank and Median Salary
model1 <- lm(salary ~ rank, data = schools)
summary(model1)

# Interpretation of slope coefficient: A 1-point increase in rank increases median salary by X percent.
#
# Part (b): Test the theory that rank affects median salary (at 1% significance level)
# Null Hypothesis: Rank does not affect median salary
# Alternative Hypothesis: Rank affects median salary
summary(model1) # Check p-value for the slope coefficient

# Part (c): Choose 5 additional variables to help predict Median salary
# Motivation: We may include variables such as tuition, bar_pass_rate, faculty_ratio, etc.
model2 <- lm(salary ~ rank + cost + LSAT + GPA+ faculty + studfac , data = schools)
summary(model2)

# Part (d): Re-estimate the model with the new variables
# Optionally, we can use logs for some variables if needed
model3 <- lm(log(salary) ~ rank + log(cost) + LSAT + GPA+ faculty + studfac, data = schools)
summary(model3)

# Part (e): Test again if rank affects median salary at the 1% significance level
# Check p-value for rank in the updated model
summary(model3)


