library(haven)
library(psych)
library(ggplot2)
library(dplyr)
library(rwa)


data <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")


# Rename variables
names(data)[names(data) == "KJONN"] <- "Sex"
names(data)[names(data) == "SumbisNoFour"] <- "Insomnia"
names(data)[names(data) == "consci"] <- "Conscientiousness"
names(data)[names(data) == "agree"] <- "Agreeableness"
names(data)[names(data) == "open"] <- "Openness"
names(data)[names(data) == "extra"] <- "Extraversion"
names(data)[names(data) == "neuro"] <- "Neuroticism"
names(data)[names(data) == "BAIsum"] <- "Anxiety"
names(data)[names(data) == "BDIsum"] <- "Depression"
options(max.print = 2500)
describe(data)



fit <- lm(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ as.factor(Sex) + 
            Agreeableness + Conscientiousness + Neuroticism + 
            Depression + Anxiety + Insomnia, data = data)
Sex <- as.numeric(data$Sex)

# Relative weight analysis
behaviour <- data %>%
  rwa(outcome = "BRIEF_AI_T",
      predictors = c("Sex", "Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE,
      plot = TRUE)

meta <- data %>%
  rwa(outcome = "BRIEF_MI_T",
      predictors = c("Sex", "Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = T,
      plot = T)

# Print analysis
behaviour
meta

##############
# Plot RWA
plot_bri <- data.frame(
  Variables = behaviour$result$Variables,
  RescaledRelWeight = behaviour$result$Sign.Rescaled.RelWeight
)

# Plot BRI
ggplot(plot_bri, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for BRI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_bri$RescaledRelWeight), max(plot_bri$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.3))

# Plot MI
plot_mi <- data.frame(
  Variables = meta$result$Variables,
  RescaledRelWeight = meta$result$Sign.Rescaled.RelWeight
)
# Plot MI scale
ggplot(plot_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_mi$RescaledRelWeight), max(plot_mi$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#######################
# med korrekte farger
# Calculate the combined limits for the color scale
combined_min <- min(min(plot_bri$RescaledRelWeight), min(plot_mi$RescaledRelWeight))
combined_max <- max(max(plot_bri$RescaledRelWeight), max(plot_mi$RescaledRelWeight))

# Plot BRI
plot_bri <- data.frame(
  Variables = behaviour$result$Variables,
  RescaledRelWeight = behaviour$result$Sign.Rescaled.RelWeight
)

ggplot(plot_bri, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for BRI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(combined_min, combined_max), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.3)) +
  scale_y_continuous(breaks = seq(floor(combined_min / 10) * 10, ceiling(combined_max / 10) * 10, by = 10))  # Set y-axis breaks in increments of 10

# Plot MI
plot_mi <- data.frame(
  Variables = meta$result$Variables,
  RescaledRelWeight = meta$result$Sign.Rescaled.RelWeight
)

ggplot(plot_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(combined_min, combined_max), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_y_continuous(breaks = seq(floor(combined_min / 10) * 10, ceiling(combined_max / 10) * 10, by = 10))  # Set y-axis breaks in increments of 10


#############################
# Kjønnsforskjeller
data_male <- subset(data, Sex != 1)
data_female <- subset(data, Sex != 2)
# Fit separate linear models for each gender

# Relative weight analysis
male_behaviour <- data_male %>%
  rwa(outcome = "BRIEF_AI_T",
      predictors = c("Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE)

female_behaviour <- data_female %>%
  rwa(outcome = "BRIEF_AI_T",
      predictors = c("Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE)

male_meta <- data_male %>%
  rwa(outcome = "BRIEF_MI_T",
      predictors = c("Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE)

female_meta <- data_female %>%
  rwa(outcome = "BRIEF_MI_T",
      predictors = c("Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE)


#Plot male BRI
plot_male_bri <- data.frame(
  Variables = male_behaviour$result$Variables,
  RescaledRelWeight = male_behaviour$result$Sign.Rescaled.RelWeight
)

# Plot male BRI
ggplot(plot_male_bri, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for BRI (males) ") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_male_bri$RescaledRelWeight), max(plot_male_bri$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
    theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.3))

#Plot female BRI
plot_female_bri <- data.frame(
  Variables = female_behaviour$result$Variables,
  RescaledRelWeight = female_behaviour$result$Sign.Rescaled.RelWeight
)

# Plot female BRI
ggplot(plot_female_bri, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for BRI (females) ") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_female_bri$RescaledRelWeight), max(plot_female_bri$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
    theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.3))

#Plot male mi
plot_male_mi <- data.frame(
  Variables = male_meta$result$Variables,
  RescaledRelWeight = male_meta$result$Sign.Rescaled.RelWeight
)

# Plot male mi
ggplot(plot_male_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI (males) ") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_male_mi$RescaledRelWeight), max(plot_male_mi$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        panel.grid.major = element_line(size = 0.5),  
        panel.grid.minor = element_line(size = 0.5))

#Plot female mi
plot_female_mi <- data.frame(
  Variables = female_meta$result$Variables,
  RescaledRelWeight = female_meta$result$Sign.Rescaled.RelWeight
)

# Plot female mi
ggplot(plot_female_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI (females) ") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_female_mi$RescaledRelWeight), max(plot_female_mi$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none",  
        panel.border = element_rect(colour = "black", fill=NA, size=0.3),
        panel.grid.major = element_line(size = 0.5),  
        panel.grid.minor = element_line(size = 0.5)) 


#####################
# CI for R^2 
library(boot)

# Function to obtain R-squared from a model
get_r_squared <- function(data, indices) {
  # Fit the model only on the bootstrapped indices
  fit <- lm(BRIEF_AI_T ~ Agreeableness + Conscientiousness + 
              Neuroticism + Depression + Anxiety + Insomnia,
            data = data[indices, ])
  return(summary(fit)$r.squared)
}

# Bootstrap R-squared for the male model
set.seed(123) # For reproducibility
boot_male <- boot(data_male, get_r_squared, R = 1000)

# Bootstrap R-squared for the female model
set.seed(123) # For reproducibility
boot_female <- boot(data_female, get_r_squared, R = 1000)

# Compare the bootstrap distributions
# For example, you could compare the mean R-squared values from the bootstrap
mean_r_squared_male <- mean(boot_male$t)
mean_r_squared_female <- mean(boot_female$t)

# You could also create confidence intervals and compare them
ci_male <- boot.ci(boot_male, type = "perc")
ci_female <- boot.ci(boot_female, type = "perc")

# Print the results
print(mean_r_squared_male)
print(ci_male)
print(mean_r_squared_female)
print(ci_female)
##########################
# Fit the model for male AI
fit_male_AI <- lm(BRIEF_AI_T ~ Agreeableness + Conscientiousness + Neuroticism + 
                    Depression + Anxiety + Insomnia, data = data_male)

# Fit the model for male MI
fit_male_MI <- lm(BRIEF_MI_T ~ Agreeableness + Conscientiousness + Neuroticism + 
                    Depression + Anxiety + Insomnia, data = data_male)

# Fit the model for female AI
fit_female_AI <- lm(BRIEF_AI_T ~ Agreeableness + Conscientiousness + Neuroticism + 
                      Depression + Anxiety + Insomnia, data = data_female)

# Fit the model for female MI
fit_female_MI <- lm(BRIEF_MI_T ~ Agreeableness + Conscientiousness + Neuroticism + 
                      Depression + Anxiety + Insomnia, data = data_female)

# Calculate AIC values for each model
aic_male_AI <- AIC(fit_male_AI)
aic_male_MI <- AIC(fit_male_MI)
aic_female_AI <- AIC(fit_female_AI)
aic_female_MI <- AIC(fit_female_MI)

# Print the AIC values
cat("AIC for male AI model:", aic_male_AI, "\n")
cat("AIC for male MI model:", aic_male_MI, "\n")
cat("AIC for female AI model:", aic_female_AI, "\n")
cat("AIC for female MI model:", aic_female_MI, "\n")

male_behaviour$rsquare
female_behaviour$rsquare
male_meta$rsquare
female_meta$rsquare
###############
# Ci for R squared
library(boot)

# Function to calculate R-squared for a given model and data subset
rsquared <- function(data, indices, formula) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(summary(fit)$r.squared)
}

# Set the seed for reproducibility
set.seed(123)

# Define the formulas for each model
formula_male_behaviour <- BRIEF_AI_T ~ Agreeableness + Conscientiousness + Neuroticism + Depression + Anxiety + Insomnia
formula_female_behaviour <- BRIEF_AI_T ~ Agreeableness + Conscientiousness + Neuroticism + Depression + Anxiety + Insomnia
formula_male_meta <- BRIEF_MI_T ~ Agreeableness + Conscientiousness + Neuroticism + Depression + Anxiety + Insomnia
formula_female_meta <- BRIEF_MI_T ~ Agreeableness + Conscientiousness + Neuroticism + Depression + Anxiety + Insomnia

# Perform bootstrapping for the male_behaviour model
boot_male_behaviour <- boot(data = data_male, statistic = rsquared, R = 10000, formula = formula_male_behaviour)
boot_ci_male_behaviour <- boot.ci(boot_male_behaviour, type = "bca")

# Perform bootstrapping for the female_behaviour model
boot_female_behaviour <- boot(data = data_female, statistic = rsquared, R = 10000, formula = formula_female_behaviour)
boot_ci_female_behaviour <- boot.ci(boot_female_behaviour, type = "bca")

# Perform bootstrapping for the male_meta model
boot_male_meta <- boot(data = data_male, statistic = rsquared, R = 10000, formula = formula_male_meta)
boot_ci_male_meta <- boot.ci(boot_male_meta, type = "bca")

# Perform bootstrapping for the female_meta model
boot_female_meta <- boot(data = data_female, statistic = rsquared, R = 10000, formula = formula_female_meta)
boot_ci_female_meta <- boot.ci(boot_female_meta, type = "bca")

# Print the confidence intervals
print(boot_ci_male_behaviour)
print(boot_ci_female_behaviour)
print(boot_ci_male_meta)
print(boot_ci_female_meta)
##############

