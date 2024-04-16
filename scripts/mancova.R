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
  
    
  

# Multivariate results
multivariate_type_III <- Anova(model, type = "III")
print(multivariate_type_III)

# Univariate results

model_bri <- lm(cbind(BRIEF_AI_T) ~ factor(KJONN) + SumbisNoFour + consci + 
                  agree + open + extra + neuro + BAIsum + BDIsum, 
                data = data_raw)

model_mi <- lm(cbind(BRIEF_MI_T) ~ factor(KJONN) + SumbisNoFour + consci + 
                 agree + open + extra + neuro + BAIsum + BDIsum, 
               data = data.raw)

univariate_bri <- Anova(model_bri, type = "III")
univariate_mi <- Anova(model_mi, type = "III")

# Calculate partial eta squared for each effect (excluding the residuals row)
partial_eta_sq_bri <- univariate_bri$'Sum Sq'[-nrow(univariate_bri)] / 
  (univariate_bri$'Sum Sq'[-nrow(univariate_bri)] + 
     univariate_bri$'Sum Sq'[nrow(univariate_bri)])

partial_eta_sq_mi <- univariate_mi$'Sum Sq'[-nrow(univariate_mi)] / 
  (univariate_mi$'Sum Sq'[-nrow(univariate_mi)] + 
     univariate_mi$'Sum Sq'[nrow(univariate_mi)])

# Create a new data frame excluding the residuals row for the ANOVA summary tables
univariate_bri_no_res <- univariate_bri[-nrow(univariate_bri), ]
univariate_mi_no_res <- univariate_mi[-nrow(univariate_mi), ]

# Add the partial eta squared values to the new ANOVA summary tables
univariate_bri_no_res$'Partial Eta Sq' <- partial_eta_sq_bri
univariate_mi_no_res$'Partial Eta Sq' <- partial_eta_sq_mi

# Rename the 'Pr(>F)' column to 'p' in the data frames without the residuals row
names(univariate_bri_no_res)[names(univariate_bri_no_res) == "Pr(>F)"] <- "p"
names(univariate_mi_no_res)[names(univariate_mi_no_res) == "Pr(>F)"] <- "p"

# Round the p-values to 3 decimal places in the data frames without the residuals row
univariate_bri_no_res$p <- round(univariate_bri_no_res$p, 3)
univariate_mi_no_res$p <- round(univariate_mi_no_res$p, 3)

# Print the updated ANOVA summary tables with the 'p' column
print(univariate_bri_no_res)
print(univariate_mi_no_res)

print(univariate_bri)
print(univariate_mi)

# Generate an APA-style table 
apa.aov.table(model_bri, filename = "Tabell univariate BRI.doc", table.number = 1)
apa.aov.table(model_mi, filename = "Tabell univariate MI.doc", table.number = 2)


##########################################################################
# Create a vector of new names corresponding to the original predictors
new_names <- c("Sex", "Insomnia", "Conscientiousness", 
               "Agreeableness", "Openness", "Extraversion", 
               "Neuroticism", "BAI", "BDI")

# Extract the original names from the model
original_names <- names(coef(model_bri))

# Replace the original names with the new names
# Skip the first name since it's the intercept
names(coef(model_bri))[-1] <- new_names

# Now, when you display the summary or coefficients of the model,
# it will show the new names instead of the original variable names
summary(model_bri)
coef(model_bri)
################################################################################
# Regression plot with unstandardized coefficients
models <-list(
  "BRI" = lm(model_bri), 
  "MI" = lm(model_mi))
model_bri
# Combine into list
modelsummary(models, statistic = "conf.int")


# Plot
plot <- "" 
  modelplot(models, coef_omit = "Intercept") +
  labs(title = "Regression plot",
       x = "Regression coeffcient",
       y = "Variables") +
  geom_vline(xintercept=0, alpha=.45, linetype="dashed")
print(plot)




new_labels <- c(
  "factor(KJONN)2" = "Gender",
  "SumbisNoFour" = "Sumbis No Four",
  "consci" = "Conscientiousness",
  "agree" = "Agreeableness",
  "open" = "Openness",
  "extra" = "Extraversion",
  "neuro" = "Neuroticism",
  "BAIsum" = "Anxiety Sum",
  "BDIsum" = "Depression Sum"
)

# Plot with updated variable names
plot <- modelplot(models, coef_omit = "Intercept") +
  labs(title = "Regression plot",
       x = "Regression coefficient",
       y = "Variables") +
  geom_vline(xintercept=0, alpha=.45, linetype="dashed") +
  scale_y_discrete(labels = new_labels)  # Update y-axis labels





################################################################################
# Calculate standardized coefficients for model_bri
std_coefs_bri <- lm.beta(model_bri)

# Calculate standardized coefficients for model_mi
std_coefs_mi <- lm.beta(model_mi)

# Extract standardized coefficients for plotting
plot_coefs_bri <- coef(std_coefs_bri)
plot_coefs_mi <- coef(std_coefs_mi)

# Create a data frame for plotting
plot_data <- data.frame(
  term = names(plot_coefs_bri),
  estimate_bri = plot_coefs_bri,
  estimate_mi = plot_coefs_mi
)
# Plot 
plot <- ggplot(plot_data, aes(y = term)) +
  geom_point(aes(x = estimate_bri), color = "red") +
  geom_point(aes(x = estimate_mi), color = "blue") +
  labs(title = "Regression plot",
       x = "Standardized Regression Coefficients",
       y = "Variables")

# Print the plot
print(plot)


lm.beta(model_bri, complete.standardization = TRUE)
confint(std_coefs_mi, level = 0.95)

###############################################################################
# Calculate standardized coefficients for model_bri
std_coefs_bri <- lm.beta(model_bri)

# Calculate standardized coefficients for model_mi
std_coefs_mi <- lm.beta(model_mi)

# Calculate confidence intervals for standardized coefficients
conf_int_bri <- confint(std_coefs_bri, level = 0.95)
conf_int_mi <- confint(std_coefs_mi, level = 0.95)

# Extract standardized coefficients for plotting
plot_coefs_bri <- coef(std_coefs_bri)
plot_coefs_mi <- coef(std_coefs_mi)

# Create a data frame for plotting
plot_data <- data.frame(
  term = names(plot_coefs_bri),
  estimate_bri = plot_coefs_bri,
  estimate_mi = plot_coefs_mi,
  ci_lower_bri = conf_int_bri[, "2.5 %"],
  ci_upper_bri = conf_int_bri[, "97.5 %"],
  ci_lower_mi = conf_int_mi[, "2.5 %"],
  ci_upper_mi = conf_int_mi[, "97.5 %"]
)

# Plot with confidence intervals
plot <- ggplot(plot_data, aes(y = term)) +
  geom_point(aes(x = estimate_bri), color = "red") +
  geom_errorbar(aes(xmin = ci_lower_bri, xmax = ci_upper_bri), color = "red") +
  geom_point(aes(x = estimate_mi), color = "blue") +
  geom_errorbar(aes(xmin = ci_lower_mi, xmax = ci_upper_mi), color = "blue") +
  labs(title = "Regression plot",
       x = "Standardized Regression Coefficients",
       y = "Variables")


# If there are missing values, you may need to investigate and handle them
# For example, you could remove rows with missing values
plot_data <- na.omit(plot_data)

# Print the plot after handling missing values
print(plot)



############
# Multivariate resultater med type 3 ss med Car
model_2 <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(KJONN) + agree + 
                 consci + neuro + BDIsum + BAIsum + SumbisNoFour, 
               data = data_raw)
summary(model_2)
Anova(model_2, type = "III")

# Paramester estimatater
model3 <- lm(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(KJONN) + agree + 
               consci + neuro + BDIsum + BAIsum + SumbisNoFour, 
             data = data_raw)
summary(model3)

summary_aov_model_2 <- summary.aov(model_2)

# Univariate resultatet
lm_BRIEF_AI_T <- lm(BRIEF_AI_T ~ as.factor(KJONN) + agree + consci + neuro + BDIsum + BAIsum + SumbisNoFour, data = data_raw)
lm_BRIEF_MI_T <- lm(BRIEF_MI_T ~ as.factor(KJONN) + agree + consci + neuro + BDIsum + BAIsum + SumbisNoFour, data = data_raw)

anova_BRIEF_AI_T <- Anova(lm_BRIEF_AI_T, type="III")
anova_BRIEF_MI_T <- Anova(lm_BRIEF_MI_T, type="III")

print(anova_BRIEF_AI_T)
print(anova_BRIEF_MI_T)



##################
# Pass en multivariat lineær modell (mlm) ved hjelp av lm() med cbind() for de avhengige variablene
mlm_model <- lm(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(KJONN) + agree + 
                  consci + neuro + BDIsum + BAIsum + SumbisNoFour, 
                data = data_raw)

# Bruk Anova() fra car-pakken for å få univariate resultater med Type III SS
mlm_anova <- Anova(mlm_model, type="III")
# Skriv ut univariate ANOVA-resultater med Type III SS
print(summary(mlm_anova, split=list(as.factor(KJONN)=1, agree=2, consci=3, neuro=4, BDIsum=5, BAIsum=6, SumbisNoFour=7)))
summary(mlm_anova)
