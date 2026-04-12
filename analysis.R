# =========================================
# YOUTH UNEMPLOYMENT ANALYSIS (SOUTH AFRICA)
# =========================================

# 1. Load required libraries
library(tidyverse)

# 2. Load dataset
# Make sure your file path is correct
data <- read.csv("C:/Users/msnbu/Downloads/data.csv")

# View data
head(data)
summary(data)

# =========================================
# Expected columns:
# Year, Unemployment, GDP_Growth, Inflation, Education
# =========================================

# 3. Data Cleaning
data_clean <- na.omit(data)

# Convert Year to numeric
data_clean$Year <- as.numeric(data_clean$Year)

# Check cleaned data
summary(data_clean)

# =========================================
# 4. Exploratory Data Analysis (EDA)
# =========================================

# Unemployment over time
ggplot(data_clean, aes(x = Year, y = Unemployment)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time",
       x = "Year",
       y = "Unemployment Rate")

# GDP Growth vs Unemployment
ggplot(data_clean, aes(x = GDP_Growth, y = Unemployment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "GDP Growth vs Unemployment",
       x = "GDP Growth",
       y = "Unemployment")

# Inflation vs Unemployment
ggplot(data_clean, aes(x = Inflation, y = Unemployment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Inflation vs Unemployment",
       x = "Inflation",
       y = "Unemployment")

# =========================================
# 5. Regression Model
# =========================================

model <- lm(Unemployment ~ GDP_Growth + Inflation + Education, data = data_clean)

# Display results
summary(model)

# =========================================
# 6. Model Diagnostics
# =========================================

par(mfrow = c(2, 2))
plot(model)

# =========================================
# 7. Save Outputs
# =========================================

# Save cleaned dataset
write.csv(data_clean, "cleaned_data.csv", row.names = FALSE)

# Save regression results
capture.output(summary(model), file = "model_summary.txt")

# =========================================
# END OF SCRIPT
# =========================================