# Load the dplyr package for data manipulation functions like summarize and piping (%>%).
library(dplyr)

# Load the TrafficFatalities.RData file which contains data on traffic fatalities.
load("TrafficFatalities.RData")

# Display the column names of the 'Fatalities' data frame to verify variable names.
colnames(Fatalities)

# List all loaded objects in the environment to confirm successful loading of data.
ls()

# Calculate summary statistics for selected variables in the Fatalities data frame.
summary_stats <- Fatalities %>%
  summarize(
    fatality_rate_mean = mean(fatality_rate, na.rm = TRUE),  # Mean of traffic fatality rates
    fatality_rate_sd = sd(fatality_rate, na.rm = TRUE),      # Standard deviation of traffic fatality rates
    fatality_rate_min = min(fatality_rate, na.rm = TRUE),    # Minimum value of traffic fatality rates
    fatality_rate_max = max(fatality_rate, na.rm = TRUE),    # Maximum value of traffic fatality rates
    fatality_rate_count = sum(!is.na(fatality_rate)),        # Count of non-missing traffic fatality rates
    
    sb_useage_mean = mean(sb_useage, na.rm = TRUE),          # Mean seat belt usage rate
    sb_useage_sd = sd(sb_useage, na.rm = TRUE),              # Standard deviation of seat belt usage rate
    sb_useage_min = min(sb_useage, na.rm = TRUE),            # Minimum seat belt usage rate
    sb_useage_max = max(sb_useage, na.rm = TRUE),            # Maximum seat belt usage rate
    sb_useage_count = sum(!is.na(sb_useage)),                # Count of non-missing seat belt usage rates
    
    speed65_mean = mean(speed65, na.rm = TRUE),              # Mean indicator for 65 mph speed limit
    speed65_sd = sd(speed65, na.rm = TRUE),                  # Standard deviation for 65 mph speed limit
    speed65_min = min(speed65, na.rm = TRUE),                # Minimum for 65 mph speed limit indicator
    speed65_max = max(speed65, na.rm = TRUE),                # Maximum for 65 mph speed limit indicator
    speed65_count = sum(!is.na(speed65)),                    # Count of non-missing values for 65 mph indicator
    
    speed70_mean = mean(speed70plus, na.rm = TRUE),          # Mean indicator for 70+ mph speed limit
    speed70_sd = sd(speed70plus, na.rm = TRUE),              # Standard deviation for 70+ mph speed limit
    speed70_min = min(speed70plus, na.rm = TRUE),            # Minimum for 70+ mph speed limit indicator
    speed70_max = max(speed70plus, na.rm = TRUE),            # Maximum for 70+ mph speed limit indicator
    speed70_count = sum(!is.na(speed70plus)),                # Count of non-missing values for 70+ mph indicator
    
    drinkage21_mean = mean(drinkage21, na.rm = TRUE),        # Mean indicator for drinking age of 21
    drinkage21_sd = sd(drinkage21, na.rm = TRUE),            # Standard deviation for drinking age of 21
    drinkage21_min = min(drinkage21, na.rm = TRUE),          # Minimum value for drinking age of 21 indicator
    drinkage21_max = max(drinkage21, na.rm = TRUE),          # Maximum value for drinking age of 21 indicator
    drinkage21_count = sum(!is.na(drinkage21)),              # Count of non-missing values for drinking age of 21 indicator
    
    bac_08_mean = mean(bac_08, na.rm = TRUE),                # Mean for BAC limit indicator of 0.08%
    bac_08_sd = sd(bac_08, na.rm = TRUE),                    # Standard deviation for BAC limit indicator
    bac_08_min = min(bac_08, na.rm = TRUE),                  # Minimum for BAC limit indicator
    bac_08_max = max(bac_08, na.rm = TRUE),                  # Maximum for BAC limit indicator
    bac_08_count = sum(!is.na(bac_08))                       # Count of non-missing values for BAC limit indicator
  )

# Display the summary statistics calculated above.
print(summary_stats)

# Step (b)(ii) - Estimate a linear regression model to explain traffic fatality rates.
model <- lm(fatality_rate ~ sb_useage + speed65 + speed70plus + drinkage21 + bac_08, data = Fatalities)
# Display the summary of the regression model to view estimated coefficients and other statistics.
summary(model)

# Step (b)(iii) - Test for joint significance of variables that are not significant at 1% level.
# Perform an ANOVA test on the model to check for joint significance of all predictors.
anova_result <- anova(model)

# Display the ANOVA result which includes F-statistics and p-values.
print(anova_result)
