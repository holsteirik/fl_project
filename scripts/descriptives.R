library(igraph)
library(qgraph)
library(haven)
library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)
library(apaTables)
library(corrplot)

data <- read_sav("data/forskerlinje_all_data_8feb.sav")

options(max.print = 2500)
describe(data)

theme_set(theme_bw())

# Set data
g <- ggplot(data, aes(x = BRIEF_MI_T))

# Calculate bin width using Freedman-Diaconis Rule for each variable
bin_brief_ai <- 2 * IQR(data$BRIEF_AI_T, na.rm = TRUE) / (length(data$BRIEF_AI_T)^(1/3))
bin_brief_mi <- 2 * IQR(data$BRIEF_MI_T, na.rm = TRUE) / (length(data$BRIEF_MI_T)^(1/3))
bin_bdi_sum <- 2 * IQR(data$BDIsum, na.rm = TRUE) / (length(data$BDIsum)^(1/3))
bin_bai_sum <- 2 * IQR(data$BAIsum, na.rm = TRUE) / (length(data$BAIsum)^(1/3))
bin_neuro <- 2 * IQR(data$neuro, na.rm = TRUE) / (length(data$neuro)^(1/3))
bin_extra <- 2 * IQR(data$extra, na.rm = TRUE) / (length(data$extra)^(1/3))
bin_open <- 2 * IQR(data$open, na.rm = TRUE) / (length(data$open)^(1/3))
bin_agree <- 2 * IQR(data$agree, na.rm = TRUE) / (length(data$agree)^(1/3))
bin_consci <- 2 * IQR(data$consci, na.rm = TRUE) / (length(data$consci)^(1/3))
bin_sumbis_no_four <- 2 * IQR(data$SumbisNoFour, na.rm = TRUE) / (length(data$SumbisNoFour)^(1/3))
bin_agree = bin_open
# Create histograms for each variable
g_brief_ai <- ggplot(data, aes(x = BRIEF_AI_T)) +
  geom_histogram(binwidth = bin_brief_ai, fill = "steelblue", color = "white") +
  labs(title = "Histogram of BRIEF_AI_T", x = "BRIEF_AI_T", y = "Frequency")

g_brief_mi <- ggplot(data, aes(x = BRIEF_MI_T)) +
  geom_histogram(binwidth = bin_brief_mi, fill = "steelblue", color = "white") +
  labs(title = "Histogram of BRIEF_MI_T", x = "BRIEF_MI_T", y = "Frequency")

g_bdi_sum <- ggplot(data, aes(x = BDIsum)) +
  geom_histogram(binwidth = bin_bdi_sum, fill = "steelblue", color = "white") +
  labs(title = "Histogram of BDIsum", x = "BDIsum", y = "Frequency")

g_bai_sum <- ggplot(data, aes(x = BAIsum)) +
  geom_histogram(binwidth = bin_bai_sum, fill = "steelblue", color = "white") +
  labs(title = "Histogram of BAIsum", x = "BAIsum", y = "Frequency")

g_neuro <- ggplot(data, aes(x = neuro)) +
  geom_histogram(binwidth = bin_neuro, fill = "steelblue", color = "white") +
  labs(title = "Histogram of neuro", x = "neuro", y = "Frequency")

g_extra <- ggplot(data, aes(x = extra)) +
  geom_histogram(binwidth = bin_extra, fill = "steelblue", color = "white") +
  labs(title = "Histogram of extra", x = "extra", y = "Frequency")

g_open <- ggplot(data, aes(x = open)) +
  geom_histogram(binwidth = bin_open, fill = "steelblue", color = "white") +
  labs(title = "Histogram of open", x = "open", y = "Frequency")

g_agree <- ggplot(data, aes(x = agree)) +
  geom_histogram(binwidth = bin_agree, fill = "steelblue", color = "white") +
  labs(title = "Histogram of agree", x = "agree", y = "Frequency")

g_consci <- ggplot(data, aes(x = consci)) +
  geom_histogram(binwidth = bin_consci, fill = "steelblue", color = "white") +
  labs(title = "Histogram of consci", x = "consci", y = "Frequency")

g_sumbis_no_four <- ggplot(data, aes(x = SumbisNoFour)) +
  geom_histogram(binwidth = bin_sumbis_no_four, fill = "steelblue", color = "white") +
  labs(title = "Histogram of SumbisNoFour", x = "SumbisNoFour", y = "Frequency")

g_brief_ai
g_brief_mi
g_bdi_sum
g_bai_sum
g_neuro
g_extra
g_open
g_agree
g_consci
g_sumbis_no_four

# Assuming 'data' is your dataset

variables <- c("BRIEF_AI_T", "BRIEF_MI_T", "BDIsum", "BAIsum", "neuro", "extra", "open", "agree", "consci", "SumbisNoFour")

# Create a data frame to store summary statistics
summary_stats <- data.frame(Variable = character(), Mean = numeric(), SD = numeric(), Median = numeric(), Min = numeric(), Max = numeric(), stringsAsFactors = FALSE)

# Loop through each variable
for (variable in variables) {
  # Extract summary statistics using dplyr
  stats <- data %>%
    summarise(
      Variable = variable,
      Mean = mean(!!sym(variable), na.rm = TRUE),
      SD = sd(!!sym(variable), na.rm = TRUE),
      Median = median(!!sym(variable), na.rm = TRUE),
      Min = min(!!sym(variable), na.rm = TRUE),
      Max = max(!!sym(variable), na.rm = TRUE)
    )
  
  # Append to the summary dataframe
  summary_stats <- bind_rows(summary_stats, stats)
}

# Print summary statistics
print(summary_stats)

###########################################################
# Corrolatations

cor_matrix <- round(cor(na.omit(data[c("BRIEF_AI_T", "BRIEF_MI_T", "BDIsum", 
                                       "BAIsum", "neuro", "extra", "open", 
                                       "agree", "consci", "SumbisNoFour", "KJONN")], 
                                method = "spearman"), 
                        use = "pairwise.complete.obs"), 2)

cor_matrix

