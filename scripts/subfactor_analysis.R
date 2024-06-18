library(haven)
library(psych)
library(ggplot2)
library(dplyr)
library(rwa)
library(modelsummary)


data_raw <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")
options(max.print = 2500)
describe(data_raw)

# Rename variables

names(data_raw)[names(data_raw) == "SumbisNoFour"] <- "Insomnia"
names(data_raw)[names(data_raw) == "consci"] <- "Conscientiousness"
names(data_raw)[names(data_raw) == "agree"] <- "Agreeableness"
names(data_raw)[names(data_raw) == "neuro"] <- "Neuroticism"
names(data_raw)[names(data_raw) == "BAIsum"] <- "Anxiety"
names(data_raw)[names(data_raw) == "BDIsum"] <- "Depression"
names(data_raw)[names(data_raw) == "BRIEF_AI_T"] <- "Behavioral_regulation"
names(data_raw)[names(data_raw) == "BRIEF_MI_T"] <- "Metacognition"
options(max.print = 2500)
describe(data_raw)

data_raw |>
  mutate(
    across(c(Behavioral_regulation, Conscientiousness, Agreeableness, Neuroticism,
             Depression, Anxiety, Insomnia, BRIEF_IMPULSHEMMING_T, BRIEF_FLEKSIBILITET_T, 
             BRIEF_EMOSJONELLKONTROLL_T, BRIEF_SELVMONITORERING_T, BRIEF_INITIERING_T,
             BRIEF_ARBEIDSHUKOMMELSE_T, BRIEF_PLANLEGGING_T, BRIEF_OPPGAVEMONITORERING_T,
             BRIEF_ORGANISERING_T), 
           scale), .before=1) -> 
  data

names(data)[names(data) == "KJONN"] <- "Sex"

impulse <- lm(BRIEF_IMPULSHEMMING_T ~ Sex + Insomnia + Conscientiousness + 
                Agreeableness + Neuroticism + Anxiety + Depression, data = data)

summary(impulse)

 
###############
# Define the function
run_regression <- function(dependent_var, data) {
  # Construct the formula dynamically
  formula <- as.formula(paste(dependent_var, "~ Sex + Insomnia + Conscientiousness + 
                               Agreeableness + Neuroticism + Anxiety + Depression"))
  
  # Run the linear model
  model <- lm(formula, data = data)
  
  # Return the model
  return(model)
}

# Example usage with a different dependent variable
impulse <- run_regression("BRIEF_IMPULSHEMMING_T ", data)
summary(impulse)
flexibility <- run_regression("BRIEF_FLEKSIBILITET_T ", data)
summary(flexibility)


# Plots
modelplot(impulse, coef_omit = 'Interc')
modelplot(flexibility, coef_omit = 'Interc')

###############

# Define the regression function
run_regression <- function(dependent_var, data) {
  formula <- as.formula(paste(dependent_var, "~ Sex + Insomnia + Conscientiousness + 
                               Agreeableness + Neuroticism + Anxiety + Depression"))
  model <- lm(formula, data = data)
  return(model)
}

# Run regression for different dependent variables
impulse <- run_regression("BRIEF_IMPULSHEMMING_T", data)
flexibility <- run_regression("BRIEF_FLEKSIBILITET_T", data)

# Extract coefficients and calculate confidence intervals
extract_data <- function(model) {
  coefs <- summary(model)$coefficients
  data.frame(
    Term = rownames(coefs),
    Estimate = coefs[, "Estimate"],
    StdError = coefs[, "Std. Error"],
    CI_lower = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
    CI_upper = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]
  )
}

impulse_data <- extract_data(impulse)
flexibility_data <- extract_data(flexibility)

# Horizontal plot for Impulse Control
plot_impulse <- ggplot(impulse_data, aes(y = Term, x = Estimate, xmin = CI_lower, xmax = CI_upper)) +
  geom_errorbarh(height = 0.2) +  # height adjusts the height of the horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Impulse Control Regression Coefficients",
       y = "Predictor", x = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0))  # Ensure y-axis labels are horizontal

# Print the Impulse Control plot
print(plot_impulse)

# Horizontal plot for Flexibility
plot_flexibility <- ggplot(flexibility_data[flexibility_data$Term != "(Intercept)", ], 
                           aes(y = Term, x = Estimate, xmin = CI_lower, xmax = CI_upper)) +
  geom_errorbarh(height = 0.1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Flexibility Regression Coefficients",
       y = "Predictor", x = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0))

# Print the Flexibility plot
print(plot_flexibility)
