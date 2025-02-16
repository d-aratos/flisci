library(readr)
NHANES_data_NHANES <- read_csv("C:/Users/Brian/Downloads/NHANES.csv")
View(NHANES_data_NHANES)
ggplot(NHANES_data_NHANES, aes(x = Depressed, y = Weight)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Boxplot: Relationship Between Weight and Happiness",
    x = "Happiness Scale",
    y = "Weight"
  ) +
  theme_minimal()