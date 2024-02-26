library(igraph)
library(qgraph)
library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)
library(apaTables)
library(modelsummary)
library(ggplot2)
library(lm.beta)

data <- read_sav("data/forskerlinje_all_data_8feb.sav")


options(max.print = 2500)
describe(data)

model <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ factor(KJONN) + extra + agree + 
                  consci + neuro + open + BDIsum + BAIsum + SumbisNoFour, 
                data = data)


# Multivariate results
multivariate_type_III <- Anova(model, type = "III")
print(multivariate_type_III)

# Univariate results
model_bri <- lm(cbind(BRIEF_AI_T) ~ factor(KJONN) + extra + agree + 
                  consci + neuro + open + BDIsum + BAIsum + SumbisNoFour, 
                data = data)


model_mi <- lm(cbind(BRIEF_MI_T) ~ factor(KJONN) + extra + agree + 
                 consci + neuro + open + BDIsum + BAIsum + SumbisNoFour, 
               data = data)

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

# Round the p-values to four decimal places in the data frames without the residuals row
univariate_bri_no_res$p <- round(univariate_bri_no_res$p, 2)
univariate_mi_no_res$p <- round(univariate_mi_no_res$p, 2)

# Print the updated ANOVA summary tables with the 'p' column
print(univariate_bri_no_res)
print(univariate_mi_no_res)

print(univariate_bri)
print(univariate_mi)

# Generate an APA-style table 
apa.aov.table(model_bri, filename = "Tabell univariate BRI.doc", table.number = 1)
apa.aov.table(model_mi, filename = "Tabell univariate BRI.doc", table.number = 2)



################################################################################
# Regression plot
modelplot(model_bri, coef_omit = "Interc")
modelplot(model_mi, coef_omit = "Interc")

models <-list(
  "Behavioural regulation" = lm(model_bri), 
  "Metacogntion" = lm(model_mi))

# Combine into list
modelsummary(models, statistic = "conf.int")

# Plot enkel
modelplot(models, coef_omit = "Interc")

plot <- modelplot(models, coef_omit = "Intercept") +
  labs(title = "Regression plot",
       x = "Regression coeffcients",
       y = "Variables")
print(plot)



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
# Plot using ggplot
plot <- ggplot(plot_data, aes(y = term)) +
  geom_point(aes(x = estimate_bri), color = "red") +
  geom_point(aes(x = estimate_mi), color = "blue") +
  labs(title = "Regression plot",
       x = "Standardized Regression Coefficients",
       y = "Variables")

# Print the plot
print(plot)




###############################################################################
# Function to calculate standardized coefficients and their CIs
get_standardized_coefs <- function(model) {
  coefs <- summary(model)$coefficients
  # Calculate standard deviations for predictors and response
  sd_x <- sapply(model$model[-1], sd, na.rm = TRUE)
  sd_y <- sd(model$model[[1]], na.rm = TRUE)
  # Standardize coefficients
  standardized_coefs <- coefs[, "Estimate"] * sd_x / sd_y
  # Calculate standard errors for standardized coefficients
  standardized_se <- coefs[, "Std. Error"] * sd_x / sd_y
  # Calculate 95% CIs
  ci_lower <- standardized_coefs - qt(0.975, df = df.residual(model)) * standardized_se
  ci_upper <- standardized_coefs + qt(0.975, df = df.residual(model)) * standardized_se
  # Return a data frame
  data.frame(
    term = rownames(coefs),
    estimate = standardized_coefs,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}
plot_data

############################

# Plot using ggplot with error bars for confidence intervals
plot <- ggplot(plot_data, aes(y = term)) +
  geom_point(aes(x = estimate_bri), color = "red") +
  geom_point(aes(x = estimate_mi), color = "blue") +
  geom_errorbarh(aes(xmin = ci_lower_bri, xmax = ci_upper_bri), height = 0.1, color = "red") +
  geom_errorbarh(aes(xmin = ci_lower_mi, xmax = ci_upper_mi), height = 0.1, color = "blue") +
  labs(title = "Regression plot",
       x = "Standardized Regression Coefficients",
       y = "Variables")

# Print the plot
print(plot)


####################
# Calculate standardized coefficients
std_coefs_bri <- lm.beta(model_bri)
std_coefs_mi <- lm.beta(model_mi)

# Calculate 95% confidence intervals
ci_bri <- confint(std_coefs_bri)
ci_mi <- confint(std_coefs_mi)

# Display standardized coefficients and confidence intervals
summary(std_coefs_bri)
summary(std_coefs_mi)

# Display confidence intervals
ci_bri
ci_mi
#######################
# Assuming 'data' is your data frame containing the variables
# Assuming 'sjstats', 'sjPlot', and 'modelplot' packages are installed

# Install and load the 'sjstats' package
install.packages("sjstats")
library(sjstats)

# Fit the multivariate linear models
model_bri <- lm(cbind(BRIEF_AI_T) ~ factor(KJONN) + extra + agree + 
                  consci + neuro + open + BDIsum + BAIsum + SumbisNoFour, 
                data = data)

model_mi <- lm(cbind(BRIEF_MI_T) ~ factor(KJONN) + extra + agree + 
                 consci + neuro + open + BDIsum + BAIsum + SumbisNoFour, 
               data = data)

# Calculate standardized coefficients using 'standardize'
std_coefs_bri <- standardize(model_bri)
std_coefs_mi <- standardize(model_mi)

# Calculate 95% confidence intervals
ci_bri <- confint(std_coefs_bri)
ci_mi <- confint(std_coefs_mi)

# Create a data frame for plotting
plot_data <- data.frame(
  term = rownames(coef(std_coefs_bri)),
  estimate_bri = coef(std_coefs_bri),
  ci_lower_bri = ci_bri[, 1],
  ci_upper_bri = ci_bri[, 2],
  estimate_mi = coef(std_coefs_mi),
  ci_lower_mi = ci_mi[, 1],
  ci_upper_mi = ci_mi[, 2]
)

# Print the data frame
print(plot_data)
