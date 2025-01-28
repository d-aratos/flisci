# Load necessary libraries
library(ggplot2)

url <- "https://raw.githubusercontent.com/d-aratos/flisci/refs/heads/main/NHANES%20source%20data/NHANES%20data%20-%20NHANES.csv"
data <- read.csv(url)

str(data)       
head(data)       

# 3. Plotting

# Histogram: Age Distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

# Scatter Plot: Age vs Height
ggplot(data, aes(x = Age, y = Height)) +
  geom_point(color = "purple") +
  labs(title = "Height vs Age", x = "Age", y = "Height") +
  theme_minimal()

# Bar Plot: Average BMI by Gender
ggplot(data, aes(x = Gender, y = BMI, fill = Gender)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average BMI by Gender", x = "Gender", y = "BMI") +
  theme_minimal()

# Box Plot: BMI Distribution by Gender
ggplot(data, aes(x = Gender, y = BMI, fill = Gender)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Gender", x = "Gender", y = "BMI") +
  theme_minimal()

