library(igraph)
library(qgraph)
library(haven)
library(psych)
library(ggplot2)
library(stats)
library(car)

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
print(univariate_bri)
print(univariate_mi)
