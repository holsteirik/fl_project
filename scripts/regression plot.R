library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)
library(apaTables)
library(modelsummary)
library(lm.beta)
library(dplyr)

data_raw <- read_sav("data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_01.mars2024_runar.sav")


options(max.print = 2500)
describe(data)

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

#make mancova model
model <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ factor(Sex) + Extraversion + 
                  Agreeableness + Conscientiousness + Neuroticism + Openness 
                + Depression + Anxiety + Insomnia, data = data_raw)

#  mutate to standardized 
data_raw |>
  mutate(
    across(c(BRIEF_AI_T, BRIEF_MI_T, Conscientiousness, Agreeableness, Openness,
             Extraversion, Neuroticism,
             Depression, Anxiety, Insomnia),
           scale), .before=1) -> 
  data


# Univariate results

model_bri <- lm(cbind(BRIEF_AI_T) ~ factor(Sex) + Insomnia + Conscientiousness + 
                  Agreeableness + Openness + Extraversion + Neuroticism + Anxiety + Depression, 
                data = data)

model_mi <- lm(cbind(BRIEF_MI_T) ~ factor(Sex) + Insomnia + Conscientiousness + 
                 Agreeableness + Openness + Extraversion + Neuroticism + Anxiety + Depression, 
               data = data)


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
  "Metacognition" = lm(model_mi),
  "Behavioral regulation" = lm(model_bri)
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

##########################################################


