# ======== Init ggplot2 for visuals ========
library(ggplot2)
library(dplyr)

# ======== Load in fake results for testing ========
rawData <- read.csv("FAKE_DATA.csv", header = T, sep=",")

# ======== Filter out non complete results ========
filteredData <- subset(rawData, rawData$AgreeToParticipate == 'I agree' & 
                         rawData$Finished == 'True' &
                         ((rawData$condition == 'control' & 
                             rawData$AttentionCheck_ctrl == 
                             'This code snippet is written in C.')
                          | (rawData$condition == 'experimental' 
                          & rawData$AttentionCheck_exp == 
                          'This code snippet was generated using AI.')))

# ======== How many completed the survey ========
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
  )

# ======== Survey Completion Time (minutes) ========


# Overall Quality
likert_levels <- c("Very low quality", "Low quality", "Somewhat low quality", 
                   "Neither high nor low quality", "Somewhat high quality", 
                   "High quality", "Very high quality")
filteredData$RateOverallQualityNumeric <- as.numeric(factor(
  filteredData$RateOverallQuality, levels=likert_levels, ordered = TRUE))

ggplot(filteredData, aes(x = factor(RateOverallQualityNumeric))) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ condition) +
  labs(
    title = "Overall Quality Rating by Condition",
    x = "Rating (1 = Extremely Bad, 7 = Extremely Good)",
    y = "Count"
  ) +
  theme_minimal()
