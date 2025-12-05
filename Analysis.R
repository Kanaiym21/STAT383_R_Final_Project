#libraries
library(tidyverse)
#loading the dataset
data <- read.csv("student_lifestyle_dataset 2.csv")

#quick look at the data
glimpse(data)
summary(data)

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

#Question: How does the amount of time a student spends socializing, studying, sleeping, and student's stress level affect their GPA?
#Model: GPA=β0+β1(Social_Hours)+β2(Study_Hours)+β3(Sleep_Hours)+β4(Stress_Level)+ε
# Multiple regression
model_question_multi <- lm(
  GPA ~ Social_Hours_Per_Day + Study_Hours_Per_Day + Sleep_Hours_Per_Day + Stress_Level_Num ,data = data
)

summary(model_question_multi)
