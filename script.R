# Lib setup  
library(ggplot2)
library(dplyr)

# Load in fake results for testing, filter out unneeded info, replace -99 w NA
rawData <- read.csv("FAKE_DATA.csv", header = T, sep=",")
rawData <- rawData[3:nrow(rawData), 5:ncol(rawData)] 
rawData <- rawData %>%
  mutate(across(everything(), ~ ifelse(. == -99, NA, .)))

# Filter out non complete results
filteredData <- subset(rawData, rawData$AgreeToParticipate == 'I agree' & 
                         rawData$Finished == 'True' &
                         ((rawData$condition == 'control' & 
                             rawData$AttentionCheck_ctrl == 
                             'This code snippet is written in C.')
                          | (rawData$condition == 'experimental' 
                          & rawData$AttentionCheck_exp == 
                          'This code snippet was generated using AI.')))

# Add a row to manually track if security bug was detected
filteredData$noticed_security_bug <- NA

# How many completed the survey
total_n <- as.numeric(nrow(rawData))
filtered_n <- as.numeric(nrow(filteredData))

count_df <- data.frame(Group = c("Total", "Filtered"), Count
                         = c(total_n, filtered_n))

ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Observations: Total vs Filtered",
    x = "",
    y = "Count"
  ) + theme_bw()

# Survey Completion Time (minutes)
summary(as.numeric(as.character(filteredData$Duration..in.seconds)))

# Overall Quality 
quality_levels <- c("Very low quality", "Low quality", "Somewhat low quality", 
                   "Neither high nor low quality", "Somewhat high quality", 
                   "High quality", "Very high quality")
filteredData$Rate_Overall_Quality_Numeric <- as.numeric(factor(
  filteredData$RateOverallQuality, levels=quality_levels, ordered = TRUE))

ggplot(filteredData, aes(x = factor(Rate_Overall_Quality_Numeric))) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ condition) +
  labs(
    title = "Overall Quality Rating by Condition",
    x = "Rating (1 = Extremely Bad, 7 = Extremely Good)",
    y = "Count"
  ) +
  theme_bw()

# For each likert scale, convert to numeric quantities
likert_levels <- c("Strongly disagree", "Disagree", "Somewhat disagree", 
                   "Neither agree nor disagree", "Somewhat agree", 
                   "Agree", "Strongly agree")

for(i in 1:10) {
  current_col <- paste0("Likert_", i)
  new_col <- paste0("Likert_numeric_", i)
  
  filteredData[[new_col]] <- as.numeric(factor(
    filteredData[[current_col]], levels=likert_levels, ordered = TRUE))
}

for (i in 1:6) {
  current_ai_col <- paste0("AI_Likert_", i)
  new_ai_col <- paste0("AI_Likert_numeric_", i)
  
  filteredData[[new_ai_col]] <- as.numeric(factor(
    filteredData[[current_ai_col]], levels=likert_levels, ordered = TRUE))
}

# Spearmans RHO for credit hours and CS courses
filteredData$num_classes <- as.numeric(lengths(strsplit(
  filteredData$Courses, ",")))

catagoryTypes <- c("fewer than 30", "between 30 and 59", "between 60 and 89", 
                   "90 or more")

filteredData$CreditHours_catagory <- as.numeric(factor(
  filteredData$CreditHours), levels=catagoryTypes, ordered=TRUE)

cor.test(filteredData$CreditHours_catagory, filteredData$num_classes,
         method="spearman", exact=FALSE)
