# Install and load necessary packages

library(haven)
library(dplyr)
library(knitr)

# Read your data from SPSS
data <- read_sav("data/forskerlinje_all_data_8feb.sav")

# Calculate mean and filter for valid observations
summary_table <- data %>%
  group_by(studie_sortert) %>%
  summarize(
    mean_BRIEF_GEF_T = mean(BRIEF_GEF_T, na.rm = TRUE),
    valid_obs = sum(!is.na(BRIEF_GEF_T))
  ) %>%
  filter(valid_obs >= 5) %>%
  arrange(desc(mean_BRIEF_GEF_T))

# Print the table
kable(summary_table, caption = "Summary Table Sorted by Mean (highest first)")
