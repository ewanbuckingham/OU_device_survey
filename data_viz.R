# Pivot wider the whole dataset after filtering out the freetext questions and some 
# rogue values. 

library(tidyverse)
library(readr)
library(ggplot2)

questions <- tribble(
  ~q_number, ~question,
  "Q1", "Which of the following device types do you have access to?",
  "Q2", "Of the devices you have identified in Q1, which do you use personally?",
  "Q3", "Which do you use for study?",
  "Q4", "Are there any reasons that currently prevent you from using the devices for study purposes?",
  "Q5", "Which devices might you be interested in using for study in the future?"
)




# Load the complete dataset
df <- read_rds("device_survey.rds")

# Drop everything that can't be used in the vizualisation and filter out the free-text questions
df <- df %>%
  filter(!question %in% c("Q1a", "Q2a", "Q3a", "Q4a", "Q5a", "Q6"))

# Add a cohort so we can compare trends for students with large/small numbers of devices
# First calculate the number of devices each student has:
devices_owned <- df %>% 
  group_by(response, question) %>%
  summarise(devices_owned = sum(as.integer(answer)))

df <- left_join(df, devices_owned, by = c("response", "question"))

df <- df %>%
  mutate(cohort = as.integer(cut_interval(devices_owned, 4)))

# Density chart of number of devices owned
df %>%
  ggplot(aes(x = devices_owned)) + 
  geom_histogram()

# Create a summary datatable that 
df %>%
  group_by(survey, question, device) %>%
  summarise(answer = sum(answer)) %>%
  filter(question == "Q1") %>%
  group_by(survey) %>%
  ggplot(aes(fill = device, x = survey, y = answer)) + 
  geom_bar(position="fill", stat="identity")

# As line chart
df %>%
  filter(question == "Q1") %>%
  group_by(survey, question, device, cohort) %>%
  summarise(answer = sum(as.integer(answer))) %>%
  ggplot(aes(x = as.Date(survey), y = answer, colour = device)) + 
    geom_smooth(position="fill", stat="identity") + 
  facet_wrap(~cohort)

# As line chart
df %>%
  filter(question == "Q1") %>%
  group_by(survey, question, device, cohort) %>%
  summarise(answer = sum(as.integer(answer))) %>%
  ggplot(aes(x = as.Date(survey), y = answer, colour = device)) + 
  geom_smooth(position="fill", stat="identity") + 
  facet_wrap(~cohort)

# Chart removing the specified combinations devices owned.
device_choice = c("Streaming device", "Laptop PC")
device_number = 4
question_choice = "Q1"

question_selected <- questions %>% filter(q_number == question_choice)

device_selected <- df %>%
  filter(device %in% device_choice,
         answer, 
         question %in% question_choice,
         devices_owned == device_number)


device_selected %>%
  group_by(survey, question, device) %>%
  summarise(answer = sum(as.integer(answer))) %>%
  ggplot(aes(x = as.Date(survey), y = answer, colour = device)) + 
  geom_smooth(position="fill", stat="identity") + 
  labs(title = question_selected$question)
