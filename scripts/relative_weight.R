library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)
library(apaTables)
library(modelsummary)
library(lm.beta)
library(dplyr)
library(rwa)
library(tidyverse)

data <- read_sav("data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_01.mars2024_runar.sav")


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
Sex <- as.factor(data$Sex)

# Relative weight analysis
behaviour <- data %>%
  rwa(outcome = "BRIEF_AI_T",
      predictors = c("Sex", "Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE)

meta <- data %>%
  rwa(outcome = "BRIEF_MI_T",
      predictors = c("Sex", "Agreeableness", "Conscientiousness", 
                     "Neuroticism", "Depression", "Anxiety", "Insomnia"),
      applysigns = TRUE)

behaviour
meta
##############

# Plot BRI
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
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_data$RescaledRelWeight), max(plot_data$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.3))


# Plot MI scale
ggplot(plot_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_data$RescaledRelWeight), max(plot_data$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none",  # Hide the legend if not needed
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

##############
# Plot MI differnet color scale
plot_mi <- data.frame(
  Variables = meta$result$Variables,
  RescaledRelWeight = meta$result$Sign.Rescaled.RelWeight
)

# Create the bar plot
ggplot(plot_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(plot_data$RescaledRelWeight), max(plot_data$RescaledRelWeight)), space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none")  # Hide the legend if not needed
# Create the bar plot
ggplot(plot_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(min(plot_mi$RescaledRelWeight), max(plot_mi$RescaledRelWeight)), 
                       space = "Lab", name="Relative\nWeight") +
  theme(legend.position = "none")  # Hide the legend if not needed

################
# MI with dark blue
ggplot(plot_mi, aes(x = reorder(Variables, RescaledRelWeight), y = RescaledRelWeight, fill = RescaledRelWeight)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes to make it a horizontal bar plot
  labs(x = "Predictor Variables", y = "Rescaled Relative Weight (%)", title = "Relative Importance of Predictors for MI") +
  theme_minimal() +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),
    values = scales::rescale(c(min(plot_mi$RescaledRelWeight), 0, max(plot_mi$RescaledRelWeight))),
    limits = c(min(plot_mi$RescaledRelWeight), max(plot_mi$RescaledRelWeight)),
    breaks = c(min(plot_mi$RescaledRelWeight), 0, max(plot_mi$RescaledRelWeight)),
    labels = c("Negative", "Neutral", "Positive"),
    guide = "colourbar"
  ) +
  theme(
    legend.position = "none",  # Hide the legend if not needed
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Add a box around the plot
  )