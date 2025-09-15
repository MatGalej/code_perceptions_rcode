# NOTE: This script must be run FIRST (DataClean.R)

library(ggplot2)
library(dplyr)
library(ordinal)
library(lubridate)
library(scales)
library(purrr)

csv <- list.files(pattern = "RawData.csv")

rawData <- read.csv(csv, header = T, sep=",")
rawData <- rawData[3:nrow(rawData), 5:ncol(rawData)] 
rawData <- rawData %>%
  mutate(across(everything(), ~ ifelse(. == -99, NA, .)))

rawData <- subset(rawData, rawData$AgreeToParticipate == 'I agree' & 
                         rawData$Finished == 'True' &
                         ((rawData$condition == 'control' & 
                             rawData$AttentionCheck_ctrl == 
                             'This code snippet is written in C.')
                          | (rawData$condition == 'experimental' 
                             & rawData$AttentionCheck_exp == 
                               'This code snippet was generated using AI.')))

# Add a row to manually track if security bug was detected
rawData$noticed_security_bug <- NA

# Add a flag for potential bot responses (If captcha score is below 4)
rawData <- rawData %>% mutate(POTENTIAL_BOT = Q_RecaptchaScore < 0.4)

# Due to high bot count after 2025-8-12, filter out all responses after that 
# date
filteredData <- rawData %>%
  mutate(RecordedDate = ymd_hms(RecordedDate)) %>%
  filter(as_date(RecordedDate) <= ymd("2025-08-12"))

# Overwrite filtered Data with new CSV after manual security screening
filteredData <- read.csv("filteredResponses.csv", header = T, sep=",")
