library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)
library(apaTables)
library(modelsummary)
library(lm.beta)
library(dplyr)

data_raw <- read_sav("data/forskerlinje_all_data_8feb.sav")


options(max.print = 2500)
describe(data)

# Rename variables
names(data_raw)[names(data_raw) == "KJONN"] <- "Sex"
names(data_raw)[names(data_raw) == "SumbisNoFour"] <- "BIS"
names(data_raw)[names(data_raw) == "consci"] <- "Conscientiousness"
names(data_raw)[names(data_raw) == "agree"] <- "Agreeableness"
names(data_raw)[names(data_raw) == "open"] <- "Openness"
names(data_raw)[names(data_raw) == "extra"] <- "Extraversion"
names(data_raw)[names(data_raw) == "neuro"] <- "Neuroticism"
names(data_raw)[names(data_raw) == "BAIsum"] <- "BAI"
names(data_raw)[names(data_raw) == "BDIsum"] <- "BDI"

#make mancova model
model <- manova(cbind(BRIEF_AI_T, BRIEF_MI_T) ~ factor(Sex) + Extraversion + 
                  Agreeableness +
                  Conscientiousness + Neuroticism + Openness + BDI + BAI +
                  BIS,
                data = data_raw)

#  mutate to standardized 
data_raw |>
  mutate(
    across(c(BRIEF_AI_T, BRIEF_MI_T, Conscientiousness, Agreeableness, Openness,
             Extraversion, Neuroticism,
             BDI, BAI, BIS),
           scale), .before=1) -> 
  data


# Univariate results

model_bri <- lm(cbind(BRIEF_AI_T) ~ factor(Sex) + BIS + Conscientiousness + 
                  Agreeableness + Openness + Extraversion + Neuroticism + BAI + BDI, 
                data = data)

model_mi <- lm(cbind(BRIEF_MI_T) ~ factor(Sex) + BIS + Conscientiousness + 
                 Agreeableness + Openness + Extraversion + Neuroticism + BAI + BDI, 
               data = data)



###############################################################################
# Regression plot 
models <-list(
  "MI" = lm(model_mi),
  "BRI" = lm(model_bri)) 
  
model_bri
# Combine into list
modelsummary(models, statistic = "conf.int")


# Your existing code for creating the plot (replace "" with your actual code)
plot <- modelplot(models, coef_omit = "Intercept") +
  labs(title = "",
       x = "Regression coefficient",
       y = "Variables") +
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
# Your existing code for creating the plot (replace "" with your actual code)
plot <- modelplot(models, coef_omit = "Intercept") +
  labs(title = "Regression plot",
       x = "Regression coefficient",
       y = "Variables") +
  geom_vline(xintercept = 0, alpha = .45, linetype = "dashed")

# Save the plot in high resolution (adjust file type and parameters as needed)
ggsave("your_plot_filename.png", plot, width = 10, height = 8, units = "in", dpi = 300)


