# ==== Histogram for overall response time (before bot filtering) ====
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

# ==== Histogram for potential bots only (before bot filtering) ====
botData <- rawData %>%
  filter(POTENTIAL_BOT == TRUE)

ggplot(botData, aes(x = Duration..in.seconds.)) +
  geom_histogram(binwidth = 60,
                 fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Suspected Bot Completion Times"),
    x = "Completion Time (seconds)",
    y = "Count"
  ) +
  theme_bw()

# ==== Histogram for overall response time (after bot filtering) ====
ggplot(filteredData, aes(x = Duration..in.seconds.)) +
  geom_histogram(binwidth = 120, 
                 fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = paste("Distribution of Survey Completion Time (Post Filter)"),
    x = "Completion Time (seconds)",
    y = "Count"
  ) +
  theme_bw()

# ==== Histogram for overall response time (after bot filtering + log transform) ====
ggplot(filteredData, aes(x = log(Duration..in.seconds.))) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Log-Transformed Survey Completion Times (Post filter)",
    x = "Completion Times (seconds)",
    y = "Count"
  )

# ==== Bar Graph of Total Observations vs Filtered ====
count_df <- data.frame(Group = c("Total", "Filtered"), Count
                       = c(as.numeric(nrow(rawData)), 
                           as.numeric(nrow(filteredData))))

ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) + 
labs(
  title = "Number of Observations: Total vs Filtered",
  x = "",
  y = "Counts"
) + theme_bw()

# ==== Bar Graph of who noticed security bug or not ==== 
count_df <- data.frame(Group = c("Total", "Noticed"), Count
                       = c(as.numeric(nrow(filteredData)), 
                           sum(filteredData$noticed_security_bug == TRUE, 
                               na.rm = TRUE)))

ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) + 
  labs(
    title = "Noticed Bug Count: Total vs Noticed",
    x = "",
    y = "Counts"
  ) + theme_bw()

# ==== Stacked Likert Scale of Overall Code Quality ====
quality_levels <- c(
  "Very low quality","Low quality","Somewhat low quality",
  "Neither high nor low quality",
  "Somewhat high quality","High quality","Very high quality"
)

filteredData <- filteredData %>%
  mutate(
    condition = factor(condition),
    quality_resp = factor(RateOverallQuality,
                          levels = quality_levels, ordered = TRUE)
  )

# Compute within-condition percentages (so bars sum to 100%)
plot_df <- filteredData %>%
  count(condition, quality_resp, .drop = FALSE) %>%
  group_by(condition) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

cond_order <- plot_df %>%
  mutate(pos = ifelse(as.integer(quality_resp) >= 5, pct, 0)) %>%
  group_by(condition) %>%
  summarise(pos_share = sum(pos), .groups = "drop") %>%
  arrange(desc(pos_share)) %>%
  pull(condition)

plot_df <- plot_df %>%
  mutate(condition = factor(condition, levels = cond_order))
cols <- c("#D73027","#FC8D59","#FEE090","#E0E0E0","#91BFDB","#4575B4","#313695")


ggplot(plot_df, aes(x = condition, y = pct, fill = quality_resp)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(
    aes(label = ifelse(pct >= 0.06, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  scale_y_continuous(labels = percent_format(), expand = expansion(c(0, 0.01))) +
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE), name = NULL) +
  labs(
    title = "Overall Quality",
    x = NULL, y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

# ==== For each likert scale, convert to numeric quantities ==== 
likert_levels <- c("Strongly disagree", "Disagree", "Somewhat disagree", 
                   "Neither agree nor disagree", "Somewhat agree", 
                   "Agree", "Strongly agree")

likert_cols    <- paste0("Likert_", 1:10)
ai_likert_cols <- paste0("AI_Likert_", 1:6)

filteredData <- filteredData %>%
  mutate(
    condition = factor(condition)
  ) %>%
  # Regular Likerts -> numeric
  mutate(across(all_of(likert_cols),
                ~ as.integer(factor(.x, levels = likert_levels, ordered = TRUE)),
                .names = "Likert_numeric_{.col}")) %>%
  # AI Likerts -> numeric
  mutate(across(all_of(ai_likert_cols),
                ~ as.integer(factor(.x, levels = likert_levels, ordered = TRUE)),
                .names = "AI_Likert_numeric_{.col}"))

# ==== Stacked Plot of Each Likert Scale + helper function ====
likert_100_plot <- function(df, item_col, title_text) {
  # Use the original (categorical) column to preserve labels/order
  resp_fac <- factor(df[[item_col]], levels = likert_levels, ordered = TRUE)
  
  plot_df <- df %>%
    transmute(condition = factor(condition), response = resp_fac) %>%
    # count with all response levels present
    count(condition, response, .drop = FALSE) %>%
    group_by(condition) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
  
  # Optional: order conditions by positive share (5–7)
  cond_order <- plot_df %>%
    mutate(pos = ifelse(as.integer(response) >= 5, pct, 0)) %>%
    group_by(condition) %>%
    summarise(pos_share = sum(pos), .groups = "drop") %>%
    arrange(desc(pos_share)) %>%
    pull(condition)
  
  plot_df <- plot_df %>%
    mutate(condition = factor(condition, levels = cond_order))
  
  # Colors (neg → neutral → pos)
  cols <- c("#D73027","#FC8D59","#FEE090","#E0E0E0","#91BFDB","#4575B4","#313695")
  
  ggplot(plot_df, aes(x = condition, y = pct, fill = response)) +
    geom_col(width = 0.8) +
    coord_flip() +
    geom_text(
      aes(label = ifelse(pct >= 0.06, percent(pct, accuracy = 1), "")),
      position = position_stack(vjust = 0.5),
      size = 3, color = "black"
    ) +
    scale_y_continuous(labels = percent_format(), expand = expansion(c(0, 0.01))) +
    scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE), name = NULL) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "top"
    )
}

likert_titles <- c(
  "Item 1 title", "Item 2 title", "Item 3 title", "Item 4 title", "Item 5 title",
  "Item 6 title", "Item 7 title", "Item 8 title", "Item 9 title", "Item 10 title"
)

ai_likert_titles <- c(
  "AI Item 1 title", "AI Item 2 title", "AI Item 3 title",
  "AI Item 4 title", "AI Item 5 title", "AI Item 6 title"
)

likert_plots <- map2(likert_cols, likert_titles, ~
                       likert_100_plot(filteredData, .x, .y)
)

ai_likert_plots <- map2(ai_likert_cols, ai_likert_titles, ~
                          likert_100_plot(filteredData, .x, .y)
)

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
