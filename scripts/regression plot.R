library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)
library(apaTables)
library(modelsummary)
library(lm.beta)
library(dplyr)
library(broom)

data_raw <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")


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

#  mutate to standardized 
data_raw |>
  mutate(
    across(c(BRIEF_AI_T, BRIEF_MI_T, Conscientiousness, Agreeableness, Openness,
             Extraversion, Neuroticism,
             Depression, Anxiety, Insomnia),
           scale), .before=1) -> 
  data


##################################################
# Model uten E og O
#make mancova model
model <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ factor(Sex) +
                  Agreeableness + Conscientiousness + Neuroticism 
                + Depression + Anxiety + Insomnia, data = data_raw)
summary(model)
model_bri_uten <- lm(cbind(BRIEF_AI_T) ~ factor(Sex) + Insomnia + Conscientiousness + 
                  Agreeableness + + Neuroticism + Anxiety + Depression, 
                data = data)

model_mi_uten <- lm(cbind(BRIEF_MI_T) ~ factor(Sex) + Insomnia + Conscientiousness + 
                 Agreeableness + Neuroticism + Anxiety + Depression, 
               data = data)

# Regression plot 
models <-list(
  "Metacognition" = lm(model_mi_uten),
  "Behavioral regulation" = lm(model_bri_uten)
)


model_bri
# Combine into list
modelsummary(models, statistic = "conf.int")


# Your existing code for creating the plot 
plot <- modelplot(models, coef_omit = "Intercept") +
  labs(title = "",
       x = "Regression coefficient",
       y = "") +
  geom_vline(xintercept = 0, alpha = .45, linetype = "dashed") +
  theme(
    text = element_text(family = "", size = 15),
    axis.title = element_text(family = "", size = 15),
    axis.text = element_text(family = "", size = 15),
    plot.title = element_text(family = "", size = 20),
  )


# Print or save the modified plot
print(plot)
###############################################################################
# Regression plot 
models <-list(
  "Metacognition" = lm(model_mi_uten),
  "Behavioral regulation" = lm(model_bri_uten)
  )
##############################################

  
model_bri
# Combine into list
modelsummary(models, statistic = "conf.int")


# Your existing code for creating the plot 
plot <- modelplot(models, coef_omit = "Intercept") +
  labs(title = "",
       x = "Regression coefficient",
       y = "") +
  geom_vline(xintercept = 0, alpha = .45, linetype = "dashed") +
  theme(
    text = element_text(family = "", size = 15),
    axis.title = element_text(family = "", size = 15),
    axis.text = element_text(family = "", size = 15),
    plot.title = element_text(family = "", size = 20),
  )
    

# Print or save the modified plot
print(plot)



################################################################
# Parametre fra spss *sigh*
library(ggplot2)

# Create a data frame with the parameter estimates and confidence intervals, excluding the intercept
coefficients_df <- data.frame(
  DependentVariable = rep(c("BRIEF_AI_T", "BRIEF_MI_T"), each = 6),
  Parameter = c("BDIsum", "BAIsum", "SumbisNoFour", "agree", "consci", "neuro",
                "BDIsum", "BAIsum", "SumbisNoFour", "agree", "consci", "neuro"),
  Estimate = c(0.339, 0.184, -0.134, -1.716, -2.044, 1.339,
               0.225, 0.128, 0.023, 0.075, -5.285, -0.128),
  LowerCI = c(0.185, 0.037, -0.270, -2.805, -2.959, 0.319,
              0.075, -0.014, -0.108, -0.976, -6.168, -1.113),
  UpperCI = c(0.494, 0.331, 0.001, -0.626, -1.129, 2.359,
              0.374, 0.271, 0.154, 1.127, -4.402, 0.856)
)

# Create the coefficient plot with horizontal confidence intervals and dodging
dodge <- position_dodge(width = 0.25) # Adjust the width to dodge as needed

ggplot(coefficients_df, aes(y = Parameter, x = Estimate, color = DependentVariable)) +
  geom_point(position = dodge) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, position = dodge) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  labs(y = "Predictor", x = "Estimate", color = "Dependent Variable") +
  theme(axis.text.y = element_text(angle = 0)) # No need to rotate y-axis labels
#################################################
fit <- lm(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ factor(Sex) + 
            Agreeableness + Conscientiousness + Neuroticism + 
            Depression + Anxiety + Insomnia, data = data)



# Create a data frame with the parameter estimates and confidence intervals
coefficients_df <- data.frame(
  DependentVariable = rep(c("Behavioral Regulation", "Metacognition"), each = 7),
  Parameter = c("Depression", "Anxiety", "Neuroticism", "Agreeableness", "Conscientiousness", "Insomnia", "Sex",
                "Depression", "Anxiety", "Neuroticism", "Agreeableness", "Conscientiousness", "Insomnia", "Sex"),
  Estimate = c(0.293, 0.158, 0.144, -0.152, -0.219, -0.115, 0.248,
               0.186, 0.106, -0.013, 0.006, -0.542, 0.019, 0.014),
  LowerCI = c(0.160, 0.032, 0.034, -0.248, -0.317, -0.231, 0.045,
              0.063, -0.011, -0.115, -0.083, -0.633, -0.088, -0.174),
  UpperCI = c(0.427, 0.285, 0.254, -0.055, -0.121, 0.001, 0.450,
              0.310, 0.223, 0.088, 0.096, -0.452, 0.126, 0.201)
)

# Reverse the factor levels for Parameter to get the desired order from top to bottom
coefficients_df$Parameter <- factor(
  coefficients_df$Parameter,
  levels = rev(c("Depression", "Anxiety", "Neuroticism", "Agreeableness", "Conscientiousness", "Insomnia", "Sex"))
)

# Set the factor levels for DependentVariable to switch the order
coefficients_df$DependentVariable <- factor(
  coefficients_df$DependentVariable,
  levels = c("Metacognition", "Behavioral Regulation")
)

# Create the coefficient plot with horizontal confidence intervals and dodging
dodge <- position_dodge(width = 0.25)

ggplot(coefficients_df, aes(y = Parameter, x = Estimate, color = DependentVariable)) +
  geom_point(position = dodge) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, position = dodge) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  theme_minimal() +
  labs(y = "Predictor", x = "         Regression coefficient", color = "Dependent Variable") +
  theme(axis.text.y = element_text(angle = 0)) +
  scale_color_discrete(breaks = rev(levels(coefficients_df$DependentVariable))) # Reverse the legend order
  
########################################
# Load necessary library


bri <- lm(cbind(BRIEF_AI_T) ~ Depression + Anxiety + Neuroticism + 
            Agreeableness + Conscientiousness + Insomnia + as.factor(Sex), 
                    data = data)


mi <- lm(cbind(BRIEF_MI_T) ~ Depression + Anxiety + Neuroticism + 
            Agreeableness + Conscientiousness + Insomnia + as.factor(Sex), 
                    data = data)

# Get tidy summaries of the models
tidy_bri <- tidy(bri)
tidy_mi <- tidy(mi)

# Add a column to distinguish the models
tidy_mi$DependentVariable <- "Metacognition"
tidy_bri$DependentVariable <- "Behavioral regulation"


# Combine the data from both models
coefficients_df <- rbind(tidy_bri, tidy_mi)

# Calculate confidence intervals
coefficients_df$LowerCI <- coefficients_df$estimate - 1.96 * coefficients_df$std.error
coefficients_df$UpperCI <- coefficients_df$estimate + 1.96 * coefficients_df$std.error


# Create the plot
plot <- ggplot(coefficients_df, aes(y = term, x = estimate, color = DependentVariable)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, position = position_dodge(width = 0.25)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal() +
  labs(y = "Predictor", x = "Regression coefficient", color = "Dependent Variable") +
  theme(axis.text.y = element_text(angle = 0))  

# Print the plot
print(plot)

