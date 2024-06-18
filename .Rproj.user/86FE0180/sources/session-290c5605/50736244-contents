library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)
library(apaTables)
library(modelsummary)
library(ggplot2)
library(lm.beta)
library(lme4)
library(dplyr)
library(boot)


data_raw <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")


options(max.print = 2500)
describe(data_raw)

model <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(KJONN) + agree + 
                  consci + neuro + BDIsum + BAIsum + SumbisNoFour, 
                data = data_raw)

data_raw |>
  mutate(
    across(c(BRIEF_AI_T, BRIEF_MI_T, consci, agree, open, extra, neuro,
             BDIsum, BAIsum, SumbisNoFour), scale), .before=1) -> 
  data
  



################################################################################
# Multivariate resultater med type 3 ss med Car
model_2 <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(KJONN) + agree + 
                 consci + neuro + BDIsum + BAIsum + SumbisNoFour, 
               data = data_raw)
Anova(model_2, type = "III")

# Univariate resultatet
lm_BRIEF_AI_T <- lm(BRIEF_AI_T ~ as.factor(KJONN) + agree + consci + 
                      neuro + BDIsum + BAIsum + SumbisNoFour, data = data_raw)
lm_BRIEF_MI_T <- lm(BRIEF_MI_T ~ as.factor(KJONN) + agree + consci + neuro + 
                      BDIsum + BAIsum + SumbisNoFour, data = data_raw)

anova_BRIEF_AI_T <- Anova(lm_BRIEF_AI_T, type="III")
anova_BRIEF_MI_T <- Anova(lm_BRIEF_MI_T, type="III")

print(anova_BRIEF_AI_T)
print(anova_BRIEF_MI_T)

# Parameter estimates
summary(lm_BRIEF_AI_T)
summary(lm_BRIEF_MI_T)

# med CI
# BRI
summary_lm_BRIEF_AI_T <- summary(lm_BRIEF_AI_T)
confint_lm_BRIEF_AI_T <- confint(lm_BRIEF_AI_T)
summary_lm_BRIEF_AI_T$coefficients <- cbind(
  summary_lm_BRIEF_AI_T$coefficients, "95% CI Lower" = confint_lm_BRIEF_AI_T[,1], 
  "95% CI Upper" = confint_lm_BRIEF_AI_T[,2])
print(summary_lm_BRIEF_AI_T)

# MI
summary_lm_BRIEF_MI_T <- summary(lm_BRIEF_MI_T)
confint_lm_BRIEF_MI_T <- confint(lm_BRIEF_MI_T)
summary_lm_BRIEF_MI_T$coefficients <- cbind(
  summary_lm_BRIEF_MI_T$coefficients, "95% CI Lower" = confint_lm_BRIEF_MI_T[,1], 
  "95% CI Upper" = confint_lm_BRIEF_MI_T[,2])
print(summary_lm_BRIEF_MI_T)

######
# Bootstrap function for the BRIEF_AI_T model with numeric p-values
boot_function_AI <- function(data, indices) {
  data_sample <- data[indices, ]  # Resample the data
  model <- lm(BRIEF_AI_T ~ as.factor(KJONN) + agree + consci + neuro + BDIsum + BAIsum + SumbisNoFour, data = data_sample)
  p_values <- summary(model)$coefficients[, 4]  # Extract p-values
  return(as.numeric(p_values))  # Ensure p-values are numeric
}

# Bootstrap function for the BRIEF_MI_T model with numeric p-values
boot_function_MI <- function(data, indices) {
  data_sample <- data[indices, ]  # Resample the data
  model <- lm(BRIEF_MI_T ~ as.factor(KJONN) + agree + consci + neuro + BDIsum + BAIsum + SumbisNoFour, data = data_sample)
  p_values <- summary(model)$coefficients[, 4]  # Extract p-values
  return(as.numeric(p_values))  # Ensure p-values are numeric
}

# Perform bootstrap for the BRIEF_AI_T model
boot_results_AI <- boot(data = data_raw, statistic = boot_function_AI, R = 5000)

# Perform bootstrap for the BRIEF_MI_T model
boot_results_MI <- boot(data = data_raw, statistic = boot_function_MI, R = 5000)

# Calculate 95% Confidence Intervals for the p-values from the BRIEF_AI_T model
ci_p_values_AI <- apply(boot_results_AI$t, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

# Calculate 95% Confidence Intervals for the p-values from the BRIEF_MI_T model
ci_p_values_MI <- apply(boot_results_MI$t, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

# Assuming you have the names stored or can retrieve them from the model
predictor_names <- names(coef(lm_BRIEF_AI_T))

# Print the confidence intervals with predictor names for BRIEF_AI_T
cat("Confidence Intervals for P-values (BRIEF_AI_T):\n")
print(data.frame(Predictor = predictor_names, CI_Lower = ci_p_values_AI[1,], CI_Upper = ci_p_values_AI[2,]))

# Assuming similar names for BRIEF_MI_T
cat("Confidence Intervals for P-values (BRIEF_MI_T):\n")
print(data.frame(Predictor = predictor_names, CI_Lower = ci_p_values_MI[1,], CI_Upper = ci_p_values_MI[2,]))

# Extract predictor names from the model
predictor_names_AI <- names(coef(lm_BRIEF_AI_T))
predictor_names_MI <- names(coef(lm_BRIEF_MI_T))

# Print the confidence intervals with predictor names for BRIEF_AI_T
cat("Confidence Intervals for P-values (BRIEF_AI_T):\n")
print(data.frame(Predictor = predictor_names_AI, CI_Lower = ci_p_values_AI[1,], CI_Upper = ci_p_values_AI[2,]))

# Print the confidence intervals with predictor names for BRIEF_MI_T
cat("Confidence Intervals for P-values (BRIEF_MI_T):\n")
print(data.frame(Predictor = predictor_names_MI, CI_Lower = ci_p_values_MI[1,], CI_Upper = ci_p_values_MI[2,]))