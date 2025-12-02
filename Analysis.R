#libraries
library(tidyverse)
#loading the dataset
data <- read.csv("student_lifestyle_dataset 2.csv")

#quick look at the data
glimpse(data)
summary(data)

#We have 3 questions to answer, so we will need new variables
#Ratio of social hours to study hours
data <- data %>%
  mutate(Social_Study_Ratio = Social_Hours_Per_Day / Study_Hours_Per_Day)
summary(data$Social_Study_Ratio)

#In the dataset, stress levels(low, moderate, high), so we turn it to numeric values
data <- data %>%
  mutate(
    Stress_Level_Num = case_when(
      Stress_Level == "Low" ~ 1,
      Stress_Level == "Moderate" ~ 2,
      Stress_Level == "High" ~ 3,
      TRUE ~ NA_real_  
    )
  )

table(data$Stress_Level, data$Stress_Level_Num)

#Question 1: How does the ratio of social hours to study hours impact GPA?
#Model: GPA = β0 + β1 * Social_Study_Ratio + error
model_question1 <- lm(GPA ~ Social_Study_Ratio, data = data)

# View detailed results
summary(model_question1)

#Plot of Question 1
ggplot(data, aes(x = Social_Study_Ratio, y = GPA)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "GPA vs Social/Study Hours Ratio",
    x = "Social Hours / Study Hours",
    y = "GPA"
  )

#Question 2: How does the amount of time a student spends sleeping affect their GPA?
# Model: GPA = β0 + β1 * Sleep_Hours_Per_Day + error
model_question2 <- lm(GPA ~ Sleep_Hours_Per_Day, data = data)

# View detailed results
summary(model_question2)

#Plot of Question 2
ggplot(data, aes(x = Sleep_Hours_Per_Day, y = GPA)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "GPA vs Sleep Hours Per Day",
    x = "Sleep Hours Per Day",
    y = "GPA"
  )

#Question 3: How does the amount of time a student spends studying impact their mental well-being?
# Model: Stress_Level_Num = β0 + β1 * Study_Hours_Per_Day + error
model_question3 <- lm(Stress_Level_Num ~ Study_Hours_Per_Day, data = data)

# View detailed results
summary(model_question3)

#Plot of Question 3
ggplot(data, aes(x = Study_Hours_Per_Day, y = Stress_Level_Num)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Stress Level vs Study Hours Per Day",
    x = "Study Hours Per Day",
    y = "Stress Level (1 = Low, 3 = High)"
  )



