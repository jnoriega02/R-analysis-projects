# (i) Load the dataset
load("AirFares.RData")  # Loads the dataset named AirFares from an RData file

# Define a function to compute summary statistics for a given variable
summary_stats <- function(var) {
  return(c(mean = mean(var, na.rm = TRUE),      # Calculate mean, ignoring NA values
           median = median(var, na.rm = TRUE),  # Calculate median, ignoring NA values
           sd = sd(var, na.rm = TRUE),          # Calculate standard deviation, ignoring NA values
           min = min(var, na.rm = TRUE),        # Find minimum value, ignoring NA values
           max = max(var, na.rm = TRUE),        # Find maximum value, ignoring NA values
           n = sum(!is.na(var))))               # Count number of non-NA values
}

# Call the function to get summary statistics for the 'fare' variable
summary_stats(AirFares$fare)  

# Call the function to get summary statistics for the 'dist' variable
summary_stats(AirFares$dist)  

# (iii) Estimate a simple linear regression model (Fare as dependent variable, Distance as independent variable)
model <- lm(fare ~ dist, data = AirFares)  # Fit the linear model

# Print the summary of the fitted model
summary(model)

# (a) Scatterplot of Fare vs. Distance
plot(AirFares$dist, AirFares$fare,  # Create a scatterplot
     xlab = "Distance",              # Label for x-axis
     ylab = "Fare",                  # Label for y-axis
     main = "Scatterplot of Fare vs. Distance")  # Title of the plot

# (b) Plot residuals vs. predicted values
residuals <- model$residuals        # Extract residuals from the model
predicted <- model$fitted.values    # Extract fitted values (predicted values) from the model

plot(predicted, residuals,          # Create a scatterplot of predicted values vs. residuals
     xlab = "Predicted Values",     # Label for x-axis
     ylab = "Residuals",            # Label for y-axis
     main = "Residuals vs. Predicted Values")  # Title of the plot
abline(h = 0, col = "red")          # Add a horizontal line at y=0 for reference

# (c) Breusch-Pagan test for heteroskedasticity
library(lmtest)                    # Load the lmtest library for statistical tests
bptest(model)                      # Perform the Breusch-Pagan test on the model

# (d) Heteroskedasticity-robust standard errors
library(sandwich)                  # Load the sandwich library for robust standard errors

# Calculate robust standard errors
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))  # Use the 'HC1' type for heteroskedasticity-consistent covariance matrix

# Re-calculate t-statistics and p-values using robust standard errors
robust_results <- coeftest(model, vcov. = vcovHC(model, type = "HC1"))  # Adjust coefficients for robust standard errors

# Display robust results
robust_results  # Print the results with robust standard errors



#PART 2



# Load the necessary library for heteroskedasticity-robust standard errors
library(sandwich)  # Provides functions for robust covariance matrix estimation
library(lmtest)    # Provides functions for hypothesis testing in linear models

# (a) Calculate heteroskedasticity-robust standard errors and re-calculate t-statistics and p-values for both the intercept and the slope coefficient.

# Load the dataset (assuming Houses.RData contains the necessary data)
load("Houses.RData")

# View the structure of the data
str(houses)

# Fit the multiple linear regression model (based on your previous assignment)
model <- lm(price ~ age + nbh + cbd + inst + rooms + area + land + baths, data = houses)

# Display the summary of the model
summary(model)

# Calculate robust standard errors using the vcovHC function with type "HC1"
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))

# Re-calculate t-statistics and p-values using the coeftest function with robust standard errors
robust_results <- coeftest(model, vcov. = vcovHC(model, type = "HC1"))

# Display the robust results
print(robust_results)

# (b) Conduct the test of joint significance of the variables that are not statistically significant
# at the 0.5% level using heteroskedasticity-robust standard errors.

# Identify the non-significant variables (assuming significance level of 0.005)
# Extract p-values from robust results
p_values <- robust_results[, "Pr(>|t|)"]
non_significant_vars <- names(p_values)[p_values > 0.005]

# Conduct the test of joint significance using the waldtest function
joint_test <- waldtest(model, vcov = vcovHC(model, type = "HC1"), terms = non_significant_vars)

# Display the results of the joint significance test
print(joint_test)



