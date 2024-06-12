library(igraph)
library(qgraph)
library(haven)
library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)
library(apaTables)
library(corrplot)

data <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")

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
bin_KJONN <- 2 * IQR(data$KJONN, na.rm = TRUE) / (length(data$SumbisNoFour)^(1/3))
bin_agree = bin_open
bin_KJONN = 0.7
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

g_KJONN <- ggplot(data, aes(x = KJONN)) +
  geom_histogram(binwidth = bin_KJONN, fill = "steelblue", color = "white") +
  labs(title = "Histogram of KJONN", x = "KJONN", y = "Frequency")

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
g_KJONN
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

cor_matrix <- round(cor(na.omit(data[c("BRIEF_AI_T", "BRIEF_MI_T", "BDIsum", 
                                       "BAIsum", "neuro", "extra", "open", 
                                       "agree", "consci", "SumbisNoFour", "KJONN")], 
                                method = "spearman"), 
                        use = "pairwise.complete.obs"), 2)

# Create an APA-style table
apa_table <- apa.table(cor_matrix, 
                       table.number = 1, 
                       title = "Correlation Matrix", 
                       show.row.names = FALSE)

# Print the APA-style table
print(apa_table, style = "apa")

#########################
# how many score 65 or above?
over_bri <- sum(data$BRIEF_AI_T > 64, na.rm = TRUE)
over_mi <- sum(data$BRIEF_MI_T > 64, na.rm = TRUE)
over_gec <- sum(data$BRIEF_GEF_T > 64, na.rm = TRUE)
over_imp <- sum(data$BRIEF_IMPULSHEMMING_T > 64, na.rm = TRUE)
over_fle <- sum(data$BRIEF_FLEKSIBILITET_T > 64, na.rm = TRUE)
over_emo <- sum(data$BRIEF_EMOSJONELLKONTROLL_T > 64, na.rm = TRUE)
over_sel <- sum(data$BRIEF_SELVMONITORERING_T > 64, na.rm = TRUE)
over_ini <- sum(data$BRIEF_INITIERING_T > 64, na.rm = TRUE)
over_arb <- sum(data$BRIEF_ARBEIDSHUKOMMELSE_T > 64, na.rm = TRUE)
over_pla <- sum(data$BRIEF_PLANLEGGING_T  > 64, na.rm = TRUE)
over_opp <- sum(data$BRIEF_OPPGAVEMONITORERING_T > 64, na.rm = TRUE)
over_org <- sum(data$BRIEF_ORGANISERING_T  > 64, na.rm = TRUE)

print(over_bri / sum(!is.na(data$BRIEF_AI_T)) * 100)
print(over_mi / sum(!is.na(data$BRIEF_MI_T)) * 100)
print(over_gec / sum(!is.na(data$BRIEF_GEF_T)) * 100)
print(over_imp / sum(!is.na(data$BRIEF_IMPULSHEMMING_T)) * 100)
print(over_fle / sum(!is.na(data$BRIEF_FLEKSIBILITET_T)) * 100)
print(over_emo / sum(!is.na(data$BRIEF_EMOSJONELLKONTROLL_T)) * 100)
print(over_sel / sum(!is.na(data$BRIEF_SELVMONITORERING_T)) * 100)
print(over_ini / sum(!is.na(data$BRIEF_INITIERING_T)) * 100)
print(over_arb / sum(!is.na(data$BRIEF_ARBEIDSHUKOMMELSE_T)) * 100)
print(over_pla / sum(!is.na(data$BRIEF_PLANLEGGING_T)) * 100)
print(over_opp / sum(!is.na(data$BRIEF_OPPGAVEMONITORERING_T)) * 100)
print(over_org / sum(!is.na(data$BRIEF_ORGANISERING_T)) * 100)






#########################################
# Compute the 'insomnia' variable based on the provided logic
data$insomnia <- ifelse(((data$BIS_1 >= 3 | data$BIS_2 >= 3 | data$BIS_3 >= 3) & 
                           (data$BIS_5 >= 3 | data$BIS_6 >= 3)), 1, 0)

# Remove missing values from the 'insomnia' variable
insomnia_cleaned <- na.omit(data$insomnia)

without_insomnia <- mean(insomnia_cleaned == 0) * 100
with_insomnai <- mean(insomnia_cleaned == 1) * 100

without_insomnia
with_insomnai
#################
# means
describe(data$BRIEF_AI_T)   
describe(data$BRIEF_MI_T) 
describe(data$BRIEF_GEF_T) 
describe(data$BDIsum) 
describe(data$BAIsum) 
describe(data$neuro)
describe(data$agree) 
describe(data$consci)
describe(data$SumbisNoFour)
##################################
# Enhanced QQ plot for BRIEF_AI_T
qqplot_brief_ai_t <- qqplot(data, aes(sample = BRIEF_AI_T)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Enhanced QQ Plot for BRIEF_AI_T") +
  theme_minimal()

# Enhanced QQ plot for BRIEF_MI_T
qqplot_brief_mi_t <- ggplot(data, aes(sample = BRIEF_MI_T)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Enhanced QQ Plot for BRIEF_MI_T") +
  theme_minimal()

# Print the plots
print(qqplot_brief_ai_t)
print(qqplot_brief_mi_t)

# Boxplot for BRIEF_AI_T
boxplot_brief_ai_t <- ggplot(data, aes(y = BRIEF_AI_T)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Boxplot for BRIEF_AI_T") +
  theme_minimal()

# Boxplot for BRIEF_MI_T
boxplot_brief_mi_t <- ggplot(data, aes(y = BRIEF_MI_T)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot for BRIEF_MI_T") +
  theme_minimal()

# Print the plots
print(boxplot_brief_ai_t)
print(boxplot_brief_mi_t)

# Enhanced Boxplot for BRIEF_AI_T
boxplot_brief_ai_t <- ggplot(data, aes(y = BRIEF_AI_T, x = 1)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 1) +
  geom_jitter(width = 0.1, color = "blue", size = 1.5, alpha = 0.5) +
  labs(title = "Enhanced Boxplot for BRIEF_AI_T", x = NULL, y = "BRIEF_AI_T") +
  theme_minimal()

# Enhanced Boxplot for BRIEF_MI_T
boxplot_brief_mi_t <- ggplot(data, aes(y = BRIEF_MI_T, x = 1)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.shape = 1) +
  geom_jitter(width = 0.1, color = "green", size = 1.5, alpha = 0.5) +
  labs(title = "Enhanced Boxplot for BRIEF_MI_T", x = NULL, y = "BRIEF_MI_T") +
  theme_minimal()

# Print the plots
print(boxplot_brief_ai_t)
print(boxplot_brief_mi_t)
