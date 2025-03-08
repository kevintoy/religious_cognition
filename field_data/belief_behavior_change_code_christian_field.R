
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
raw_data<-read.csv("~/explaining atheism/ariel_project/cleaned_data_christian.csv",fileEncoding="utf8")

raw_data <- raw_data %>%
  mutate(faith_in_god = recode(faith_in_god,
                               '根本不相信' = -2,
                               '不太相信' = -1,
                               '半信半疑' = 0,
                               '比较相信' = 1,
                               '非常相信' = 2))

raw_data <- raw_data %>%
  mutate(religious_activity_freq = recode(religious_activity_freq,
                               '几乎从未有过' = 0,
                               '一年2-3次' = 1,
                               '几个月1次' = 2,
                               '每月一次' = 3,
                               '每周一次' = 4,
                               '每天都有，甚至不止一次' = 5))
raw_data <- raw_data %>%
  mutate(education = recode(education,
                            '未上过学' = 0,
                            '小学' = 1,
                            '初中' = 2,
                            '高中毕业' = 3,
                            '高中'= 3,
                            '中专毕业' = 4,
                            '大专毕业' = 5,
                            '本科毕业' = 5,
                            '大学毕业' = 5,
                            '研究生毕业' = 6))
faithful_subset <- raw_data %>%
  filter(faith_in_god %in% c(1, 2))

raw_data<-faithful_subset


replace_values <- function(x) {
  case_when(
    x == "不确定" ~ 0,
    x == "不变" ~ 0,
    x == "也许会增加" ~ 1,
    x == "很有可能会增加" ~ 2,
    x == "一定会增加" ~ 3,
    x == "也许会降低" ~ -1,
    x == "很有可能会降低" ~ -2,
    x == "一定会降低" ~ -3,
    x == "更大" ~ 1,
    x == "更小" ~ -1,
    x == "也许更少" ~ -1,
    x == "也许更多" ~ 1,
    x == "很有可能更多" ~ 2,
    x == "很有可能更少" ~ -2,
    TRUE ~ as.numeric(x)
  )
}

# Apply the function to the specified columns
raw_data <- raw_data %>%
  mutate(across(c(Q10.1, Q10.2, Q9.1, Q9.2, Q8.1, Q8.2, Q7.1, Q7.2, Q6.1, Q6.2, Q5.1, Q5.2, Q4.1, Q4.2, Q3.1, Q3.2), replace_values)) %>%
  rename(
    `exam failure (belief)` = Q3.1,
    `exam failure (behavior)` = Q3.2,
    `exam success (belief)` = Q4.1,
    `exam success (behavior)` = Q4.2,
    `illness low survival failure (belief)` = Q5.1,
    `illness low survival failure (behavior)` = Q5.2,
    `illness high survival failure (belief)` = Q6.1,
    `illness high survival failure (behavior)` = Q6.2,
    `illness low survival success (belief)` = Q7.1,
    `illness low survival success (behavior)` = Q7.2,
    `illness high survival success (belief)` = Q8.1,
    `illness high survival success (behavior)` = Q8.2,
    `safe travel failure (belief)` = Q9.1,
    `safe travel failure (behavior)` = Q9.2,
    `safe travel success (belief)` = Q10.1,
    `safe travel success (behavior)` = Q10.2
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
                             'Belief' = 'Belief change (field Christian)',
                             'Behavior' = 'Behavior change (field Christian)')

# Recode the 'Type' factor to have the new labels 'Belief Change' and 'Behavior Change'
combined_long$Type <- factor(combined_long$Type, levels = c("Belief change (field Christian)", "Behavior change (field Christian)"))

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
  labs(x = "", y = "Mean Score", fill = "Prayer outcome")

# Print the plot
print(plot)

# compute one sample p value and effect size using t-test
t.test(belief_data$`exam failure (belief)`, mu = 0, alternative = "less")
t.test(belief_data$`illness low survival failure (belief)`, mu = 0, alternative = "less")
t.test(belief_data$`illness high survival failure (belief)`, mu = 0, alternative = "less")
t.test(belief_data$`safe travel failure (belief)`, mu = 0, alternative = "less")


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


model <- lm(directional_bayesian_rationality ~ age + education + faith_in_god + religious_activity_freq, data = raw_data)

# View the model summary
summary(model)

library(stargazer)
stargazer(model, type = "text",
          title = "Regression Results",
          digits = 3, star.cutoffs = c(0.1, 0.05, 0.01))
