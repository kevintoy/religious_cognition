library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

raw_data<-read.csv("~/explaining atheism/ariel_project/prolific_survey/merged_prolific_data_muslim_first_person.csv",fileEncoding="utf8")

replace_values <- function(x) {
  case_when(
    x == "definitely decrease" ~ -3,
    x == "very likely to decrease" ~ -2,
    x == "may decrease" ~ -1,
    x == "remain the same" ~ 0,
    x == "stay the same" ~ 0,
    x == "may increase" ~ 1,
    x == "very likely to increase" ~ 2,
    x == "definitely increase" ~ 3,
    x == "will definitely increase" ~ 3,
    TRUE ~ as.numeric(x)
  )
}

raw_data <- raw_data %>%
  mutate(across(c(X10.1, X10.2, X9.1, X9.2, X8.1, X8.2, X7.1, X7.2, X6.1, X6.2, X5.1, X5.2, X4.1, X4.2, X3.1, X3.2), replace_values)) %>%
  rename(
    `exam failure (belief)` = X4.1,
    `exam failure (behavior)` = X4.2,
    `exam success (belief)` = X3.1,
    `exam success (behavior)` = X3.2,
    `illness low survival failure (belief)` = X8.1,
    `illness low survival failure (behavior)` = X8.2,
    `illness high survival failure (belief)` = X5.1,
    `illness high survival failure (behavior)` = X5.2,
    `illness low survival success (belief)` = X6.1,
    `illness low survival success (behavior)` = X6.2,
    `illness high survival success (belief)` = X7.1,
    `illness high survival success (behavior)` = X7.2,
    `safe travel failure (belief)` = X9.1,
    `safe travel failure (behavior)` = X9.2,
    `safe travel success (belief)` = X10.1,
    `safe travel success (behavior)` = X10.2
  )
belief_data <- raw_data %>% select(contains("(belief)"))
behavior_data <- raw_data %>% select(contains("(behavior)"))

# Calculate mean and standard error for each column in belief_data
belief_summary <- belief_data %>%
  summarise(across(everything(), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))))

# Calculate mean and standard error for each column in behavior_data
behavior_summary <- behavior_data %>%
  summarise(across(everything(), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))))
# Pivot the summary data into long format for plotting
belief_long <- pivot_longer(belief_summary, cols = everything(),
                            names_to = c("Scenario", ".value"),
                            names_pattern = "(.*)_(mean|se)")

behavior_long <- pivot_longer(behavior_summary, cols = everything(),
                              names_to = c("Scenario", ".value"),
                              names_pattern = "(.*)_(mean|se)")
# Create the combined plot
belief_plot <- ggplot(belief_long, aes(x = Scenario, y = mean, fill = "Belief")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(0.9), width = 0.25)

behavior_plot <- ggplot(behavior_long, aes(x = Scenario, y = mean, fill = "Behavior")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(0.9), width = 0.25)
# Combine the two datasets
combined_long <- bind_rows(belief_long, behavior_long)
# Modify the combined_long dataframe to create a 'Prayer Outcome' variable
combined_long <- combined_long %>%
  mutate(PrayerOutcome = ifelse(str_detect(Scenario, "success"), "Success", "Failure"),
         Type = ifelse(str_detect(Scenario, "\\(belief\\)"), "Belief", "Behavior"),
         Scenario = str_replace_all(Scenario, "\\s\\((belief|behavior)\\)", ""))

combined_long$Type <- recode(combined_long$Type, 
                             'Belief' = 'Belief Change (prolific Muslim, second-person)',
                             'Behavior' = 'Behavior Change (prolific Muslim, second-person)')
combined_long$Type <- factor(combined_long$Type, levels = c("Belief Change (prolific Muslim, second-person)", "Behavior Change (prolific Muslim, second-person)"))

# Recreate the plot with the updated order
plot <- ggplot(combined_long, aes(x = Scenario, y = mean, fill = PrayerOutcome)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = position_dodge()) +
  facet_wrap(~ Type, scales = "fixed", nrow = 2, dir = "v") + # Facets with fixed scales
  scale_y_continuous(limits = c(-3, 3)) + # Set the y-axis limits with the max value of 2
  scale_fill_manual(values = c("Success" = "green", "Failure" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, face = "bold"),
        legend.position = "right") + # Move the legend to the right
  labs(x = "", y = "Mean Score", fill = "Prayer Outcome")

# Print the updated plot
print(plot)

t.test(belief_data$`illness low survival success (belief)`, belief_data$`illness high survival success (belief)`)

#check if belief increase in case of prayer failures significantly deviate from 0
t.test(raw_data$`exam failure (belief)`, mu = 0)
t.test(raw_data$`illness low survival failure (belief)`, mu = 0)
t.test(raw_data$`illness high survival failure (belief)`, mu = 0)
t.test(raw_data$`safe travel failure (belief)`, mu = 0)

#create a "directional bayesian rational" variable
# Create a new column for each scenario indicating whether a participant follows Bayesian rationality
raw_data <- raw_data %>%
  mutate(
    bayesian_exam = (`exam success (belief)` > 0 & `exam failure (belief)` < 0),
    bayesian_illness_high = (`illness high survival success (belief)` > 0 & `illness high survival failure (belief)` < 0),
    bayesian_illness_low = (`illness low survival success (belief)` > 0 & `illness low survival failure (belief)` < 0),
    bayesian_travel = (`safe travel success (belief)` > 0 & `safe travel failure (belief)` < 0)
  )

# Summing across all four scenarios to get the Directional Bayesian Rationality score (0 to 4)
raw_data <- raw_data %>%
  mutate(directional_bayesian_rationality = rowSums(select(., bayesian_exam, bayesian_illness_high, bayesian_illness_low, bayesian_travel), na.rm = TRUE))

# Recode the values in column "X1.1"
raw_data <- raw_data %>%
  mutate(religiosity = recode(X1.1, 
                              "strongly disbelieve" = -2,
                              "somewhat disbelieve" = -1,
                              "unsure" = 0,
                              "somewhat believe" = 1,
                              "strongly believe" = 2))

raw_data <- raw_data %>%
  mutate(education = recode(X1.2, 
                            "never attended school" = 0,
                            "primary school" = 1,
                            "middle school" = 2,
                            "high school" = 3,
                            "vocational/technical school" = 4,
                            "undergraduate (college/University)" = 5,
                            "graduate school (postgraduate)" = 6
  ))

raw_data <- raw_data %>%
  mutate(religious_activity_freq = recode(X1.3, 
                                          "Almost never" = 0,
                                          "2-3 times a year" = 1,
                                          "Once a few months" = 2,
                                          "Once a month" = 3,
                                          "Once a week" = 4,
                                          "Daily or more" = 5
  ))
raw_data$Age <- suppressWarnings(as.numeric(raw_data$Age))
# Run a linear regression model
model <- lm(directional_bayesian_rationality ~ Age + education + religiosity + religious_activity_freq, data = raw_data)

# View the model summary
summary(model)

library(stargazer)
stargazer(model, type = "text",
          title = "Regression Results",
          digits = 3, star.cutoffs = c(0.1, 0.05, 0.01))

raw_data$Ethnicity.simplified
raw_data$Country.of.residence
raw_data$X1.2
