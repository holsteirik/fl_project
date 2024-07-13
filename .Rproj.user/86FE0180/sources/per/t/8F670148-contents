library(haven)
library(psych)
library(ggplot2)
library(dplyr)
library(rwa)
library(modelsummary)
library(cowplot)

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
predictor_order <- c("Sex", "Insomnia", "Conscientiousness", "Agreeableness", 
                     "Neuroticism", "Anxiety", "Depression")
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
  geom_errorbarh(height = 0.2) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Impulse Control Regression Coefficients",
       y = "Predictor", x = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0))  

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
##############


run_regression_and_plot <- function(dependent_var, plot_title, data, show_y_axis_labels, show_x_axis_label) {
  # Run the regression
  model <- lm(as.formula(paste(dependent_var, "~ Sex + Insomnia + Conscientiousness + 
                               Agreeableness + Neuroticism + Anxiety + Depression")), data = data)
  
  # Extract coefficients and calculate confidence intervals
  coefs <- summary(model)$coefficients
  regression_data <- data.frame(
    Term = factor(rownames(coefs), levels = c("(Intercept)", "Sex", "Insomnia", "Conscientiousness", 
                                              "Agreeableness", "Neuroticism", "Anxiety", "Depression")),
    Estimate = coefs[, "Estimate"],
    CI_lower = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
    CI_upper = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]
  )
  
  # Create the plot
  plot <- ggplot(regression_data[regression_data$Term != "(Intercept)", ], 
                 aes(y = Term, x = Estimate, xmin = CI_lower, xmax = CI_upper)) +
    geom_errorbarh(height = 0.2) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(title = plot_title,
         y = if(show_y_axis_labels) "Predictor" else NULL,
         x = if(show_x_axis_label) "Coefficient Estimate" else NULL) +
    scale_x_continuous(limits = c(-0.70, 0.70)) +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "pt"))
  
  if (!show_y_axis_labels) {
    plot <- plot + theme(axis.text.y = element_blank())
  }
  
  # Return the model and plot
  return(list(model = model, plot = plot))
}

# List of dependent variables and corresponding new names
dependent_vars <- c("BRIEF_IMPULSHEMMING_T", "BRIEF_FLEKSIBILITET_T", "BRIEF_EMOSJONELLKONTROLL_T", 
                    "BRIEF_SELVMONITORERING_T", "BRIEF_INITIERING_T", "BRIEF_ARBEIDSHUKOMMELSE_T", 
                    "BRIEF_PLANLEGGING_T", "BRIEF_OPPGAVEMONITORERING_T", "BRIEF_ORGANISERING_T")

new_names <- c("Inhibit", "Shift", "Emotional Control", "Self Monitor", "Initiate", "Working memory", 
               "Plan/Organize", "Task monitor", "Organization Of Materials")

# Create an empty list to store the plots
plots <- list()

# Loop through the dependent variables and their corresponding new names
for (i in seq_along(dependent_vars)) {
  show_y_axis_labels <- i %in% c(1, 4, 7)
  show_x_axis_label <- i == 8
  result <- run_regression_and_plot(dependent_vars[i], new_names[i], data, show_y_axis_labels, show_x_axis_label)
  plots[[i]] <- result$plot
}


library(patchwork)
plots[[1]] + 
  plots[[2]] + 
  plots[[3]] + 
  plots[[4]] + 
  plots[[5]] +
  plots[[6]] + 
  plots[[7]] +
  plots[[8]] +
  plots[[9]] +
    plot_layout(ncol=3)




