# Histogram for overall response time (before bot filtering)
rawData$Duration..in.seconds. <- 
  as.numeric(as.character(rawData$Duration..in.seconds.))

ggplot(rawData, aes(x = Duration..in.seconds.)) +
  geom_histogram(binwidth = 60, 
                 fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Survey Completion Time (Pre Filter)"),
    x = "Completion Time (seconds)",
    y = "Count"
  ) +
  theme_bw()

botData <- rawData %>%
  filter(POTENTIAL_BOT == TRUE)

# Plot histogram for potential bots only (before bot filtering)
ggplot(botData, aes(x = Duration..in.seconds.)) +
  geom_histogram(binwidth = 60,
                 fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Suspected Bot Completion Times"),
    x = "Completion Time (seconds)",
    y = "Count"
  ) +
  theme_bw()

# Print count of who noticed security bug
table(filteredData$noticed_security_bug)

# Redo survey time histogram AFTER filtering
ggplot(filteredData, aes(x = Duration..in.seconds.)) +
  geom_histogram(binwidth = 120, 
                 fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Survey Completion Time (Post Filter)"),
    x = "Completion Time (seconds)",
    y = "Count"
  ) +
  theme_bw()

# How many completed the survey
total_n <- as.numeric(nrow(rawData))
filtered_n <- as.numeric(nrow(filteredData))

# Completion time after log transformation
ggplot(filteredData, aes(x = log(Duration..in.seconds.))) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Log-Transformed Survey Completion Times (Post filter)",
    x = "Log Completion Time (seconds)",
    y = "Count"
  )

count_df <- data.frame(Group = c("Total", "Filtered"), Count
                       = c(total_n, filtered_n))

ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5)  
labs(
  title = "Number of Observations: Total vs Filtered",
  x = "",
  y = "Count"
) + theme_bw()

# Survey Completion Time (minutes)
summary(as.numeric(as.character(filteredData$Duration..in.seconds)))
print(quantile(as.numeric(as.character(filteredData$Duration..in.seconds)), 0.05))

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

# Spearman correlation testing for credit hours and CS courses
filteredData$num_classes <- as.numeric(lengths(strsplit(
  filteredData$Courses, ",")))

catagoryTypes <- c("fewer than 30", "between 30 and 59", "between 60 and 89", 
                   "90 or more")

filteredData$CreditHours_catagory <- as.numeric(factor(
  filteredData$CreditHours), levels=catagoryTypes, ordered=TRUE)

cor.test(filteredData$CreditHours_catagory, filteredData$num_classes,
         method="spearman", exact=FALSE)

# Add in variables for core_class completion and security
cs_core <- c("CS 250","CS 251","CS 252")
cs_security <- c("CS 354","CS 355","CS 426")

# Check if a participant has completed ALL core classes
filteredData$completed_core <- sapply(filteredData$Courses, function(x) {
  if (is.na(x)) {
    return(FALSE)
  }
  courses <- trimws(unlist(strsplit(x, ",")))
  all(cs_core %in% courses)
})

# Checks if a participant has completed AT LEAST ONE security course
filteredData$taken_security <- sapply(filteredData$Courses, function(x) {
  if (is.na(x)) {
    return(FALSE)
  }
  courses <- trimws(unlist(strsplit(x, ",")))
  any(cs_security %in% courses)
})

# trust_in_AI likert calculations
ai_questions <- c("AI_Likert_numeric_1", "AI_Likert_numeric_2",
                  "AI_Likert_numeric_3", "AI_Likert_numeric_4",
                  "AI_Likert_numeric_5", "AI_Likert_numeric_6")
filteredData$avg_ai_trust <- rowMeans(filteredData[,ai_questions])

# GLM model for the relationship:
# noticed_security_bug ~ condition + condition*trust_in_AI + credithours + 
# completed_CS_core + security_class
bug_detected <- glm(noticed_security_bug ~ condition +
                      (condition*avg_ai_trust) + CreditHours_catagory
                    + completed_core + taken_security, data=filteredData,
                    family = gaussian())
summary(bug_detected)

# CLM Models for various quality perceptions, including from overall, likert, 
# etc.

# TODO: Use different model? This error was obtained:
# (1) Hessian is numerically singular: parameters are not uniquely determined 
# In addition: Absolute convergence criterion was met, but relative criterion\
# was not met

qual_perception_overall <- clm(factor(Rate_Overall_Quality_Numeric) ~ condition +
                                 (condition*avg_ai_trust) + CreditHours_catagory
                               + completed_core + taken_security,
                               data=filteredData)
summary(qual_perception_overall)


likert_questions <- c("Likert_numeric_1", "Likert_numeric_2", "Likert_numeric_3",
                      "Likert_numeric_4", "Likert_numeric_5", "Likert_numeric_6",
                      "Likert_numeric_7", "Likert_numeric_8", "Likert_numeric_9",
                      "Likert_numeric_10")
filteredData$likert_avg <- rowMeans(filteredData[,likert_questions])

qual_perception_likert <- clm(factor(likert_avg) ~ condition +
                                (condition*avg_ai_trust) + CreditHours_catagory
                              + completed_core + taken_security,
                              data=filteredData)
summary(qual_perception_likert)
