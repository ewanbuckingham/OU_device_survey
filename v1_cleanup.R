library(tidyverse)
library(readr)

### Watch for Excel screwing with the dates formats. Simplest to create a CSV and ingest from that. 
# Need to filter out the free-text questions to prevent them becoming NA

df <- read_csv("Data/Database_format.csv")

# Split off the metadata section to focus on tidying the data itself
df_meta <- df %>% select(1:10)
df_data <- df %>% select(2,11:74)

# Move free-text entries to seperate table since they'll be NA by the next step.
# df_free_text <- df_data %>% 
# select(c("Q02_Q1a", "Q04_Q2a", "Q06_Q3a", "Q08_Q4a", "Q10_Q5a"))

df_data <- df_data %>%
  select(!c("Q02_Q1a", "Q04_Q2a", "Q06_Q3a", "Q08_Q4a", "Q10_Q5a"))

#Replace the 1 values with the name from the column header (this begins to move the data
# into rows, rather than headsers and allows us to split them up into question, device 
# and answer relatively easily).
df_data <- imap_dfr(df_data, ~replace(.x, .x==1, .y))

# Future dev: Reinsert the free-text answers in a useful way. Not used for current
# purpose, so omitted here.

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
df_data <- as_tibble(cbind(str_vector, df_data))

# Tidy up the dataframe, removing the compound column that's now redundant and reordering
# for readability
df_data<- df_data %>% 
  select(!compound) %>%
  rename("question" = `1`, "device" = `2`) %>%
  mutate(Response = as.double(Response)) %>%
  relocate(Response, everything())

#Join the metadata back to the reworked dataset. 
df <- left_join(df_data, df_meta)

# Recode the device names to match earlier reports.
df$device[df$device == "Desktop PC"] <- "Desktop PC"
df$device[df$device == "Laptop PC" ] <- "Laptop PC" 
df$device[df$device == "Hybrid PC (e.g. Microsoft Surface)"] <- "Hybrid PC"
df$device[df$device == "Tablet (e.g. iPad)"] <- "Tablet"
df$device[df$device == "Smartphone"] <- "Smartphone"
df$device[df$device == "Games console (e.g. Xbox One, PS4)"] <- "Games console"
df$device[df$device == "Streaming device (e.g. Google Chromecast, Apple TV, Amazon Fire Stick)"] <- "Streaming device"
df$device[df$device == "Smart TV (i.e. a TV with built-in apps)"] <- "Smart TV"
df$device[df$device == "Smart speaker (e.g. Amazon Echo, Google Home)"] <- "Smart speaker"
df$device[df$device == "Smart screen (e.g. Amazon Echo Show, Google Home Hub)"] <- "Smart screen"
df$device[df$device == "Virtual Reality headset (e.g. Oculus Rift, PlayStation VR, Gear VR)"] <- "Virtual reality headset"
df$device[df$device == "Other (please specify)"] <- "Other"

# Convert the device, survey date  column to a factors
df$device <- as.factor(df$device)
df$question <- as.factor(df$question)

# Repeat values spotted in the entry data. This strip out perfect duplicates. 
df <- unique(df)

# Set all the column headings to lower case
names(df)<-tolower(names(df))
  
#Store the result
write_rds(df, path = "device_survey.rds")

# Remove temporary processing dataframes
rm(df_data, df_meta, str_vector)



