# Validating the logic in app.R

# Logic needs reworking
#   Find all the devices that match the condition of device types. Then go back and 
# filter every combination of devices (device number). 
# Add a control for less than/equal to/more than? the device number?



library(tidyverse)
library(lubridate)

device_list <- c("Desktop PC","Laptop PC")

device_list2 <- c("Desktop PC", 
                 "Laptop PC", 
                 "Hybrid PC", 
                 "Tablet", 
                 "Smartphone", 
                 "Games console", 
                 "Streaming device", 
                 "Smart TV", 
                 "Smart speaker", 
                 "Smart screen", 
                 "Virtual reality headset", 
                 "Other")

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
  mutate(cohort = as.integer(cut_interval(devices_owned, 6)))

# Filter the complete list of device types. 
devices <- df %>%
  distinct(device) %>%
  head(12)
devices <- as.character(devices$device)

# Filter for the number of devices each user has access to 
device_number <- 1:11

devices_chosen <-  function(data_frame, survey_date, device_choice,  question_choice, device_number){
  # Filter the correct combination of input variables.
  device_selected <- data_frame %>%
    filter(
           survey == ymd(survey_date),
           devices_owned == device_number,
           device %in% device_choice,
           question == question_choice) %>%
    select(response, survey, question, device, answer) %>%
    group_by(response, survey, question, device) %>%
    summarise(answer = sum(answer))

  device_selected <- device_selected %>%
    group_by(response) %>%
    filter(all(answer == TRUE)) %>%
    select(response) %>%
    distinct()

 final_result <- df %>%
    filter(response %in% device_selected$response,
           question == question_choice) %>%
    select(response, survey, question, device, answer) %>%
    pivot_wider(names_from = device, values_from = answer)

 # Work out the number of responses in the result and the specific survey/question group it came from
 responses <- count(final_result) 

  total_sample <- df %>% filter(
    survey == ymd(survey_date),
    question == question_choice) %>%
    select(response, device, answer) %>%
    pivot_wider(names_from = device, values_from = answer) %>%
    summarise(sample_length = n())
  total_sample <- as.integer(total_sample$sample_length)
 
  out <- (paste0(responses,"/", total_sample, " in this selection"))
}

# Return the response IDs of the devices that match the input criteria.
out <- devices_chosen(df, "2019-04-01", device_list, "Q1", 5)


# Return the dataset that needs to be plotted (nasty code duplication here - need to break up into sub-functions)
plot <-  function(data_frame, survey_date, device_choice,  question_choice, device_number){
  # Filter the correct combination of input variables.
  device_selected <- data_frame %>%
    filter(
      # survey == ymd(survey_date),    Need to add a range input function for date range slider
      devices_owned == device_number,
      device %in% device_choice,
      question == question_choice) %>%
    select(response, survey, question, device, answer) %>%
    group_by(response, survey, question, device) %>%
    summarise(answer = sum(answer))
  
  device_selected <- device_selected %>%
    group_by(response) %>%
    filter(all(answer == TRUE)) %>%
    select(response) %>%
    distinct()

  interim_result <- df %>%
     filter(response %in% device_selected$response,
            question == question_choice) %>%
     select(response, survey, question, device, answer) %>%
    group_by(survey, question, device) %>%
    summarise(answer = sum(answer))
  
  # Convert to percentage based on cohort size
  perc_result <- interim_result %>%
    group_by(survey, question) %>%
    mutate(cohort_size = sum(answer)) %>%
    mutate(perc_result = answer/cohort_size * 100) %>%
  ggplot(aes(x = survey, y = perc_result, colour = device)) + 
    geom_line()
  
}

# Return the response IDs of the devices that match the input criteria.
out <- devices_chosen(df, "2019-04-01", device_list, "Q1", 5)



