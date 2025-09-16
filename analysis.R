# NOTE: Previous script must be run FIRST (DataClean.R)
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

# ==== Bar Graph of various demographics ====
# Includes gender, first gen, core, sec, credit hours
count_df <- data.frame(Group = c("Male", "Female"), Count
                       = c(sum(filteredData$Gender == "Man", na.rm = TRUE), 
                           sum(filteredData$Gender == "Woman", na.rm = TRUE)))
demo_1 <- ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) + 
  labs(
    title = "Demographics: Gender",
    x = "",
    y = "Counts"
  ) + theme_bw()

count_df <- data.frame(Group = c("Total", "First Gen"), Count
                       = c(as.numeric(nrow(filteredData)), 
                           sum(filteredData$FirstGen == "Yes", na.rm = TRUE)))
demo_2 <- ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) + 
  labs(
    title = "Demographics: First Gen",
    x = "",
    y = "Counts"
  ) + theme_bw()

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

count_df <- data.frame(Group = c("Completed Core", "Taken Security"), Count
                       = c(sum(filteredData$completed_core == TRUE),
                           sum(filteredData$taken_security == TRUE)))
demo_3 <- ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) + 
  labs(
    title = "Demographics: Core & Security Classes",
    x = "",
    y = "Counts"
  ) + theme_bw()

count_df <- data.frame(Group = c("<30", "30 to 59", "60 to 89", ">=90"), 
                       Count
                       = c(sum(filteredData$CreditHours == "fewer than 30",
                               na.rm = TRUE), 
                           sum(filteredData$CreditHours == "between 30 and 59",
                                                  na.rm = TRUE),
                           sum(filteredData$CreditHours == "between 60 and 89",
                                                  na.rm = TRUE),
                           sum(filteredData$CreditHours == "90 or more",
                               na.rm = TRUE)))
demo_4 <- ggplot(count_df, aes(x = Group, y = Count, fill = Group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) + 
  labs(
    title = "Demographics: Credit Hours",
    x = "",
    y = "Counts"
  ) + theme_bw()

demographics_plots <- list(demo_1, demo_2, demo_3, demo_4)

# ==== Overall Code Quality Stacked Likert Scale  ====
quality_levels <- c(
  "Very low quality","Low quality",
  "Neither high nor low quality",
  "High quality","Very high quality"
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
  mutate(pos = ifelse(as.integer(quality_resp) >= 3, pct, 0)) %>%
  group_by(condition) %>%
  summarise(pos_share = sum(pos), .groups = "drop") %>%
  arrange(desc(pos_share)) %>%
  pull(condition)

plot_df <- plot_df %>%
  mutate(condition = factor(condition, levels = cond_order))
cols <- c("#D73027","#FC8D59","#E0E0E0","#4575B4","#313695")


ggplot(plot_df, aes(x = condition, y = pct, fill = quality_resp)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(
    aes(label = ifelse(pct >= 0.06, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  scale_y_continuous(labels = percent_format(), 
                     expand = expansion(c(0, 0.01))) +
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE), name = NULL) +
  labs(
    title = "Overall Code Quality Ratings",
    x = NULL, y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

# ==== For each likert scale, convert to numeric quantities ====
likert_levels <- c("Strongly disagree", "Disagree",
                   "Neither agree nor disagree", 
                   "Agree", "Strongly agree")

likert_cols <- paste0("Likert_", 1:10)
ai_likert_cols <- paste0("AI_Likert_", 1:6)

safe_to_int <- function(x) {
  if (is.numeric(x)) {
    as.integer(x)
  } else {
    as.integer(factor(x, levels = likert_levels, ordered = TRUE))
  }
}

filteredData <- filteredData %>%
  mutate(condition = factor(condition)) %>%
  mutate(across(all_of(likert_cols), ~ safe_to_int(.x), .names = "{.col}")) %>%
  mutate(across(all_of(ai_likert_cols), ~ safe_to_int(.x), .names = "{.col}"))

# ==== Stacked Plot of Each Likert Scale + helper function ====
likert_100_plot_num <- function(df, item_col, title_text,
                                drop_na = TRUE) {
  # Map numeric 1 to 5 into ordered factor with labels
  resp_fac <- factor(df[[item_col]], levels = 1:5,
                     labels = likert_levels, ordered = TRUE)
  
  plot_df <- tibble(
    condition = factor(df$condition),
    response  = resp_fac
  ) %>%
    { if (drop_na) dplyr::filter(., !is.na(response)) else . } %>%
    count(condition, response, .drop = FALSE) %>%
    group_by(condition) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
  
  cond_order <- plot_df %>%
    mutate(pos = ifelse(as.integer(response) >= 5, pct, 0)) %>%
    group_by(condition) %>%
    summarise(pos_share = sum(pos), .groups = "drop") %>%
    arrange(desc(pos_share)) %>%
    pull(condition)
  
  plot_df <- plot_df %>% mutate(condition = factor(condition, 
                                                   levels = cond_order))
  cols <- c("#D73027","#FC8D59","#E0E0E0","#4575B4","#313695")
  
  ggplot(plot_df, aes(x = condition, y = pct, fill = response)) +
    geom_col(width = 0.8) +
    coord_flip() +
    geom_text(aes(label = ifelse(pct >= 0.06, percent(pct, accuracy = 1), "")),
              position = position_stack(vjust = 0.5), size = 3, 
              color = "black") +
    scale_y_continuous(labels = percent_format(), 
                       expand = expansion(c(0, 0.01))) +
    scale_fill_manual(values = cols, 
                      guide = guide_legend(reverse = TRUE), name = NULL) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "top")
}

likert_cols    <- paste0("Likert_", 1:10)
ai_likert_cols <- paste0("AI_Likert_", 1:6)

likert_titles <- c(
  "Q1: The code is readable", "Q2: The code is well-structured",
  "Q3: The code is testable", "Q4: The code is robust", 
  "Q5: The code is secure", "Q6: The code is well-documented", 
  "Q7: The code is easy to understand", "Q8: The code is correct", 
  "Q9: The code is free of bugs", "Q10: The code is adaptable"
)

ai_likert_titles <- c(
  "AI Q1: I trust the systems' output",
  "AI Q2: The output the system produces 
  is as good as that which a highly competent person could produce.",
  "AI Q3: I know what will happen the next time I use the system because 
  I understand how it behaves",
  "AI Q4: I believe the output of the system even when I don't know for 
  certain that it is correct",
  "AI Q5: I have a personal preference for using such AI systems for my tasks",
  "AI Q6: Overall, I trust the AI system I use"
)

# Build the plot lists & print the likerts
likert_plots <- map2(likert_cols, likert_titles,
                     ~ likert_100_plot_num(filteredData, .x, .y, 
                                           drop_na = TRUE))
ai_likert_plots <- map2(ai_likert_cols, ai_likert_titles,
                        ~ likert_100_plot_num(filteredData, .x, .y, 
                                              drop_na = TRUE))

for (i in 1:10) {
  print(likert_plots[[i]])
}

for (i in 1:6) {
  print(ai_likert_plots[[i]])
}

# ==== Spearman correlation testing for credit hours and CS courses ==== 
filteredData$num_classes <- as.numeric(lengths(strsplit(
  filteredData$Courses, ",")))

catagoryTypes <- c("fewer than 30", "between 30 and 59", "between 60 and 89", 
                   "90 or more")

filteredData$CreditHours_catagory <- as.numeric(factor(
  filteredData$CreditHours), levels=catagoryTypes, ordered=TRUE)

sink(file = "./results/spearmenTest.txt")
cor.test(filteredData$CreditHours_catagory, filteredData$num_classes,
         method="spearman", exact=FALSE)
sink()

# ==== Setup for CLM and GLM testing ====  
# trust_in_AI likert calculations & Rate_Overall_Quality_Numeric (Average)  
filteredData$avg_ai_trust <- rowMeans(filteredData[,ai_likert_cols], 
                                      na.rm = TRUE)

filteredData$Rate_Overall_Quality_Numeric <- as.numeric(factor(
  filteredData$RateOverallQuality, levels=quality_levels, ordered = TRUE))

# ==== GLM model for the relationship: ====
# noticed_security_bug ~ condition + condition*trust_in_AI + credithours + 
# completed_CS_core + security_class
bug_detected <- glm(noticed_security_bug ~ condition +
                      (condition*avg_ai_trust) + CreditHours_catagory
                    + completed_core + taken_security, data=filteredData,
                    family = gaussian())
sink(file = "./results/AI_Likert_Average_GLM.txt")
summary(bug_detected)
sink()

# CLM Model for overall quality likert
qual_perception_overall <- clm(factor(Rate_Overall_Quality_Numeric) ~ condition +
                                 (condition*avg_ai_trust) + CreditHours_catagory
                               + completed_core + taken_security,
                               data=filteredData)
sink(file = "./results/Overall_Qual_Likert_Average_CLM.txt")
summary(qual_perception_overall)
sink()

# CLM model for quality based on average of all quality likerts
filteredData$likert_avg <- rowMeans(filteredData[,likert_cols])

qual_perception_likert <- clm(factor(likert_avg) ~ condition +
                                (condition*avg_ai_trust) + CreditHours_catagory
                              + completed_core + taken_security,
                              data=filteredData)
sink(file = "./results/Specific_Likert_Average_CLM.txt")
summary(qual_perception_likert)
sink()

# CLM model PER LIKERT
out_path <- "./results/Per_likert_CLM.txt"
con <- file(out_path, open = "wt")
fac_cols <- paste0("fac_", likert_cols)

for (i in 1:10) {
  colname <- paste0("Likert_", i)
  fml <- as.formula(
    paste0("factor(", colname, ") ~ condition + condition*avg_ai_trust + ",
           "CreditHours_catagory + completed_core + taken_security")
  )
  
  fit <- try(clm(fml, data = filteredData), silent = TRUE)
  
  writeLines(paste0("\n================ Likert_", i, " ================\n"), 
             con)
  
  if (inherits(fit, "try-error")) {
    writeLines("Model failed for this item.\n", con)
  } else {
    writeLines(capture.output(summary(fit)), con)
  }
}

close(con)

# ==== Save all plots to a PDF for easy viewing ====


# Likert stacked plots
pdf("./plots/all_likert_plots.pdf", width = 8, height = 5)
for (p in likert_plots) print(p)
for (p in ai_likert_plots) print(p)
dev.off()

# Demographics plots
pdf("./plots/demographics_plots.pdf", width = 8, height = 5)
for (p in demographics_plots) print(p)
dev.off()
