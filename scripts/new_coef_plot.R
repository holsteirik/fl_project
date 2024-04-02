library(ggplot2)
library(psych)
library(haven)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)

data_rawtidyrdata_raw <- read_sav("data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_01.mars2024_runar.sav")

options(max.print = 2500)
describe(data)
describe(data_raw)

# Rename variables
names(data_raw)[names(data_raw) == "KJONN"] <- "Sex"
names(data_raw)[names(data_raw) == "SumbisNoFour"] <- "Insomnia"
names(data_raw)[names(data_raw) == "consci"] <- "Conscientiousness"
names(data_raw)[names(data_raw) == "agree"] <- "Agreeableness"
names(data_raw)[names(data_raw) == "open"] <- "Openness"
names(data_raw)[names(data_raw) == "extra"] <- "Extraversion"
names(data_raw)[names(data_raw) == "neuro"] <- "Neuroticism"
names(data_raw)[names(data_raw) == "BAIsum"] <- "Anxiety"
names(data_raw)[names(data_raw) == "BDIsum"] <- "Depression"

data_raw |>
  mutate(
    across(c(BRIEF_AI_T, BRIEF_MI_T, Conscientiousness, Agreeableness, Openness,
             Extraversion, Neuroticism,
             Depression, Anxiety, Insomnia),
           scale), .before=1) -> 
  data

##########################
# make model
# Fit the multivariate linear model
fit <- lm(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(Sex) + 
            Agreeableness + Conscientiousness + Neuroticism + 
            Depression + Anxiety + Insomnia, data = data)

# Extract coefficients for each dependent variable
coefs_BRIEF_AI_T <- summary(fit)$coefficients[[1]]
coefs_BRIEF_MI_T <- summary(fit)$coefficients[[2]]

# Create a data frame for each set of coefficients
df_BRIEF_AI_T <- as.data.frame(coefs_BRIEF_AI_T)
df_BRIEF_MI_T <- as.data.frame(coefs_BRIEF_MI_T)

# Add a column to indicate the dependent variable
df_BRIEF_AI_T$DependentVariable <- "BRIEF_AI_T"
df_BRIEF_MI_T$DependentVariable <- "BRIEF_MI_T"

# Combine the data frames
coefficients_df <- rbind(df_BRIEF_AI_T, df_BRIEF_MI_T)

# Add the parameter names as a column
coefficients_df$Parameter <- rownames(coefficients_df)

# Calculate confidence intervals for each dependent variable
conf_intervals_BRIEF_AI_T <- confint(fit)[, 1:2]
conf_intervals_BRIEF_MI_T <- confint(fit)[, 3:4]

# Combine the confidence intervals with the coefficients data frame
coefficients_df <- coefficients_df %>%
  mutate(LowerCI = ifelse(DependentVariable == "BRIEF_AI_T", conf_intervals_BRIEF_AI_T[rownames(coefficients_df), 1],
                          conf_intervals_BRIEF_MI_T[rownames(coefficients_df), 1]),
         UpperCI = ifelse(DependentVariable == "BRIEF_AI_T", conf_intervals_BRIEF_AI_T[rownames(coefficients_df), 2],
                          conf_intervals_BRIEF_MI_T[rownames(coefficients_df), 2]))

# Now you can use the ggplot code to plot the coefficients_df
ggplot(coefficients_df, aes(y = Parameter, x = Estimate, color = DependentVariable)) +
  geom_point(position = dodge) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, position = dodge) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  theme_minimal() +
  labs(y = "Predictor", x = "Regression Coefficient", color = "Dependent Variable") +
  theme(axis.text.y = element_text(angle = 0)) +
  scale_color_discrete(breaks = rev(levels(coefficients_df$DependentVariable))) # Reverse the legend order