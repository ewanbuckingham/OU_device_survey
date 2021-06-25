library(tidyverse)
library(readr)

### Watch for Excel screwing with the dates formats. Simplest to create a CSV and ingest from that. 

df <- read_csv("Data/Database_format.csv")

# Split off the metadata section to focus on tidying the data itself
df_meta <- df %>% select(1:10)
df_data <- df %>% select(2,11:74)

#Replace the 1 values with the name from the column header (this begins to move the data
# into rows, rather than headsers and allows us to split them up into question, device 
# and answer relatively easily).
df_data <- imap_dfr(df_data, ~replace(.x, .x==1, .y))

#Pivot to long format creating a 'compound' column containing the device and question number
# and an 'answer' column for the student response.
df_data <- df_data %>%
  pivot_longer(!Response, names_to = "compound", values_to = "answer")

#Now we've extracted the information from the header, convert the 'answer' to logical.
df_data$answer <- as.logical(ifelse(df_data$answer != 0, TRUE, FALSE))

#Clean up the question numbers and split them out of the device list
# Step 1: Remove the Q00_ junk that precedes each question number
# Step 2: Split the question number from the device and place in its own column
df_data$compound <- gsub("^.{0,4}", "", df_data$compound)

str_vector <- str_split_fixed(df_data$compound, "->", 2)
df_data <- as.tibble(cbind(str_vector, df_data))

# Tidy up the dataframe, removing the compound column that's now redundant and reordering
# for readability
df_data <- df_data %>% 
  select(!compound) %>%
  rename("question" = `1`, "device" = `2`) %>%
  mutate(Response = as.double(Response)) %>%
  relocate(Response, everything())

#Join the metadata back to the reworked dataset. 
df_result <- left_join(df_data, df_meta, by = "Response") %>%
  rename_with(tolower)

#Store the result
# write_rds(df_result, path = "device_survey.rds")
