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