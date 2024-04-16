library(haven)
library(tidyverse)
library(gt)
library(moments)

data <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")

names(data)[names(data) == "KJONN"] <- "Sex"
names(data)[names(data) == "SumbisNoFour"] <- "BIS"
names(data)[names(data) == "consci"] <- "Conscientiousness"
names(data)[names(data) == "agree"] <- "Agreeableness"
names(data)[names(data) == "open"] <- "Openness"
names(data)[names(data) == "extra"] <- "Extraversion"
names(data)[names(data) == "neuro"] <- "Neuroticism"
names(data)[names(data) == "BAIsum"] <- "BAI"
names(data)[names(data) == "BDIsum"] <- "BDI"
names(data)[names(data) == "BRIEF_AI_T"] <- "BRI"
names(data)[names(data) == "BRIEF_MI_T"] <- "MI"

# Select variables of interest
selected_vars <- data %>%
  select(BRI, MI, BDI, BAI, Neuroticism, Extraversion, Openness, 
         Agreeableness, Conscientiousness, BIS)

# Calculate correlation matrix
cor_result <- cor(selected_vars, method = "pearson", use = "complete.obs") %>%
  round(2)

# Omit only the upper triangle (not the diagonal)
cor_result[upper.tri(cor_result)] <- NA

# Convert the correlation matrix to a long-format data frame
cor_result_long <- as.data.frame(cor_result) %>%
  rownames_to_column(var = "Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  filter(!is.na(Correlation)) # Keep only the lower triangle

# Print the correlation matrix to check the result
print(cor_result)

# Convert the matrix to a data frame and move the row names to a column
cor_result_df <- cor_result %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable")

# Create a gt table with the variable names in the stub and without variable names as column headers
cor_table <- gt(cor_result_df, rownames_to_stub = F, auto_align = T) %>%
  cols_label(.list = setNames(rep("", ncol(cor_result_df) - 1), colnames(cor_result_df)[-1])) %>%
  fmt_missing(missing_text = "") %>%
  tab_header(title = "Correlation Matrix")

# Print the gt table
print(cor_table)
###########################################
library(haven)
library(tidyverse)
library(gt)
library(moments) # for skewness and kurtosis

# Load your data
data <- read_sav("data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_01.mars2024_runar.sav")

# Rename variables
data <- data %>%
  rename(
    Sex = KJONN,
    BIS = SumbisNoFour,
    Conscientiousness = consci,
    Agreeableness = agree,
    Openness = open,
    Extraversion = extra,
    Neuroticism = neuro,
    BAI = BAIsum,
    BDI = BDIsum,
    BRI = BRIEF_AI_T,
    MI = BRIEF_MI_T
  )

# Select variables of interest
selected_vars <- data %>%
  select(BRI, MI, BDI, BAI, Neuroticism, Extraversion, Openness, 
         Agreeableness, Conscientiousness, BIS) %>%
  na.omit()

# Calculate summary statistics
summary_stats <- selected_vars %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    skew = ~skewness(., na.rm = TRUE),
    kurt = ~kurtosis(., na.rm = TRUE)
  ))) %>%
  pivot_longer(cols = everything(), names_to = c("statistic", "Variable"), names_sep = "_") %>%
  pivot_wider(names_from = Variable, values_from = value)

# Calculate correlation matrix
cor_result <- selected_vars %>%
  cor(method = "pearson") %>%
  round(2)

# Omit only the upper triangle
cor_result[upper.tri(cor_result)] <- NA

# Convert the correlation matrix to a long-format data frame
cor_result_long <- as.data.frame(cor_result) %>%
  rownames_to_column(var = "Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlation") %>%
  filter(!is.na(Correlation)) # Keep only the lower triangle

# Combine summary statistics and correlation matrix
combined_df <- bind_cols(summary_stats, cor_result_long)

# Create a gt table with the summary statistics and correlation matrix
cor_table <- gt(combined_df) %>%
  tab_spanner(
    label = "Summary Statistics",
    columns = vars(mean, sd, min, max, skew, kurt)
  ) %>%
  tab_spanner(
    label = "Correlations",
    columns = vars(Variable2, Correlation)
  ) %>%
  fmt_missing(missing_text = "") %>%
  tab_header(title = "Summary Statistics and Correlation Matrix")

# Print the gt table
print(cor_table)

scores_between_18_29 <- data$ALDER >= 18 & data$ALDER <= 29
count <- sum(scores_between_18_29)

# Print the count
print(count)


# Use logical indexing to identify scores in the age range 30-39
scores_between_30_39 <- data$ALDER >= 30 & data$ALDER <= 39

# Use logical indexing to identify scores in the age range 40-49
scores_between_40_49 <- data$ALDER >= 40 & data$ALDER <= 49

# Use sum() function to count the number of scores in each age range
count_30_39 <- sum(scores_between_30_39)
count_40_49 <- sum(scores_between_40_49)

# Calculate the total number of scores
total_scores <- length(data$ALDER)

# Calculate the percentage for each age range
percentage_30_39 <- (count_30_39 / total_scores) * 100
percentage_40_49 <- (count_40_49 / total_scores) * 100

# Print the percentages
print(percentage_30_39)
print(percentage_40_49)
########################
# med tibble
selected_vars2 <- tibble(data %>%
  select(BRI, MI, BDI, BAI, Neuroticism, Extraversion, Openness, 
         Agreeableness, Conscientiousness, BIS))
selected_vars2

# Calculate correlation matrix
cor_result <- selected_vars2 %>%
  cor(method = "pearson", use = "complete.obs") %>%
  round(2) %>%
  as_tibble()

# Save the current row names
current_row_names <- rownames(cor_result)

# Change row names to column names
rownames(cor_result) <- colnames(cor_result)
columnnames(cor_result) <- rownames(cor_result)

# Convert row names to a new column
cor_result <- cor_result %>%
  rownames_to_column(var = "Variable")
cor_result[upper.tri(cor_result, diag = TRUE)] <- NA
# Set row names to numbers
rownames(cor_result) <- 1:nrow(cor_result)
cor_result


