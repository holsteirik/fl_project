library(haven)
library(tidyverse)
library(gt)


data <- read_sav("data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_01.mars2024_runar.sav")

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

cor_result <- data |>
  na.omit() |>
  select("BRI", "MI", "BDI", "BAI", "Neuroticism", "Extraversion", "Openness", 
         "Agreeableness", "Conscientiousness", "BIS") |>
  cor(method = "pearson") |>
  round(2)
        
# Omit the upper triangle and diagonal
cor_result[upper.tri(cor_result, diag = TRUE)] <- NA

# Convert the matrix to a data frame for gt
cor_result_df <- as.data.frame(cor_result)
cor_result_df
head(cor_result_df)

# Add row names as a column
cor_result_df <- cor_result_df %>%
  rownames_to_column(var = "Variable")

# Convert wide data to long data
cor_result_long <- cor_result_df %>%
  pivot_longer(cols = -Variable, names_to = "Comparison", values_to = "Correlation")

# Create a gt table
cor_table <- gt(cor_result_df, auto_align = F, row_group_as_column = T, 
                rownames_to_stub = F) %>%
  fmt_missing(missing_text = "") %>%
  tab_header(title = "Correlation Matrix")

print(cor_table)
help(gt)