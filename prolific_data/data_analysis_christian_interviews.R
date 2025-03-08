
# Read CSV file with proper encoding
raw_data <- read.csv("~/explaining atheism/ariel_project/christian interview coding.csv", fileEncoding="UTF-8-BOM")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Select relevant columns
factors <- raw_data %>%
  select(life_setbacks, unanswered_prayers, hypocrisy, intellectual_inconsistence, external_influence)

# Convert wide data to long format
factors_long <- factors %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Presence") %>%
  filter(!is.na(Presence))  # Remove NA values

# Count occurrences of each factor
factor_counts <- factors_long %>%
  count(Factor, name = "Count") %>%
  mutate(Factor = str_replace_all(Factor, "_", " "))  # Remove underscores in factor names

# Create the bar chart
ggplot(factor_counts, aes(x = reorder(Factor, -Count), y = Count, fill = Factor)) +
  geom_bar(stat = "identity") +
  labs(title = "",
       x = "Factors",
       y = "Count",
       fill = "Factors") +  # Add figure legend
  theme_minimal() +
  scale_fill_manual(values = c("life setbacks" = "red",
                               "unanswered prayers" = "blue",
                               "hypocrisy" = "green",
                               "intellectual inconsistence" = "purple",
                               "external influence" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11, face = "bold"),
        legend.position = "") + # Move the legend to the right
  labs(x = "reasons for lowered belief in God", y = "count")

# Count participants who mentioned either life_setbacks or unanswered_prayers at least once
mentioned_at_least_once <- raw_data %>%
  filter(!is.na(life_setbacks) | !is.na(unanswered_prayers)) %>%
  nrow()

# Total number of participants
total_participants <- nrow(raw_data)

# Calculate the proportion
proportion <- mentioned_at_least_once / total_participants
proportion
