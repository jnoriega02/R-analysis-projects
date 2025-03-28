# PART (a): Load the dataset from the .RData file
# Load the RData file 
load("Houses.RData")

# Display the names of loaded objects to verify data is loaded
ls()

# PART (a): Model log(price) = β0 + β1x1 + ... + β5x5
# Select explanatory variables for the model
X <- houses[, c("age", "nbh", "cbd", "rooms", "area")]

# Fit the model for log(price) using the selected explanatory variables
log_price_model <- lm(log(price) ~ age + nbh + cbd + rooms + area, data = houses)

# Display the summary of the log(price) model
summary(log_price_model)

# PART (b): Predict prices using the log(price) model
# Predict log(price) using the fitted model
log_price_predictions <- predict(log_price_model)

# Convert predicted log(price) back to actual price using exponential function
predicted_prices <- exp(log_price_predictions)

# PART (c): Calculate R-squared for predictions
# Calculate residuals (difference between actual and predicted prices)
residuals <- houses$price - predicted_prices

# Calculate Residual Sum of Squares (SS_res)
SS_res <- sum(residuals^2)

# Calculate Total Sum of Squares (SS_tot)

SS_tot <- sum((houses$price - mean(houses$price))^2)

# Calculate R-squared for the log(price) model

r_squared <- 1 - (SS_res / SS_tot)
cat("R-squared for log(price) model:", r_squared, "\n")

# PART (d): Compare to the original price model

# Fit the original model (price = β0 + β1x1 + ... + β5x5) using the same explanatory variables
price_model <- lm(price ~ age + nbh + cbd + rooms + area, data = houses)

# Display the summary of the original price model

summary(price_model)

# Predict prices using the original price model

original_price_predictions <- predict(price_model)

# Calculate residuals for the original model

residuals_original <- Houses$price - original_price_predictions

# Calculate Residual Sum of Squares (SS_res_original)

SS_res_original <- sum(residuals_original^2)

# Calculate Total Sum of Squares (SS_tot_original)

SS_tot_original <- sum((houses$price - mean(houses$price))^2)

# Calculate R-squared for the original price model

r_squared_original <- 1 - (SS_res_original / SS_tot_original)
cat("R-squared for price model:", r_squared_original, "\n")

# PART (d): Comparison of the two models


if (r_squared > r_squared_original) {
  cat("The log(price) model performed better with a higher R-squared value.\n")
  cat("Log(price) R-squared:", r_squared, "vs. Original price R-squared:", r_squared_original, "\n")
  cat("The log(price) model captures variations in the data more effectively and may reduce the impact of heteroscedasticity (non-constant variance).\n")
} else {
  cat("The original price model performed better with a higher R-squared value.\n")
  cat("Log(price) R-squared:", r_squared, "vs. Original price R-squared:", r_squared_original, "\n")
  cat("The original model captures the relationship between variables and price directly, which might be more interpretable in certain contexts.\n")
}
