# Init ggplot2 for visuals and load data
library(ggplot2)

# Load in fake results for testing
rawData <- read.csv("FAKE_DATA.csv", header = T, sep=",")

# Filter out those who did not agree, who did not answer the control question
# correctly, or who did not finish.
filteredData <- subset(rawData, rawData$AgreeToParticipate == 'I agree' & 
                         rawData$Finished == 'True' &
                         ((rawData$condition == 'control' & 
                             rawData$AttentionCheck_ctrl == 
                             'This code snippet is written in C.')
                          | (rawData$condition == 'experimental' 
                          & rawData$AttentionCheck_exp == 
                          'This code snippet was generated using AI.')))

# Stat information setup for visuals
n <- as.numeric(nrow(filteredData))
