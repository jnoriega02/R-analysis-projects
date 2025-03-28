# Load the SmokingWork.RData dataset
load("SmokingWork.RData")

# Filter for relevant variables
relevant_variables <- smoking_data %>%
  select(smoker, smoke_ban, age, hs_grad, female, race_black, race_hispan)

# Summary statistics of the dataset
summary_stats <- relevant_variables %>%
  summarise_all(~ c(
    Mean = mean(., na.rm = TRUE),
    SD = sd(., na.rm = TRUE),
    Min = min(., na.rm = TRUE),
    Max = max(., na.rm = TRUE),
    Count = sum(!is.na(.))
  ))
print(summary_stats)

# Fit the linear probability model (LPM) using OLS
model <- lm(smoker ~ smoke_ban + age + hs_grad + female + race_black + race_hispan, data = smoking)

# Display summary of the model
summary(model)

# Load necessary package
library(broom)

# Extract and process the tidy summary of the model
model_summary <- tidy(model) %>%
  mutate(significance = case_when(
    p.value < 0.01 ~ "***", p.value < 0.05 ~ "**", p.value < 0.1 ~ "*", TRUE ~ "")) %>%
  select(term, estimate, std.error, p.value, significance) %>%
  rename(Variable = term, Coefficient = estimate, `Standard Error` = std.error, `P-value` = p.value, Significance = significance)

# Print the result table
print(model_summary)

# Extract and print R-squared and Adjusted R-squared
cat("R-squared:", summary(model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")
