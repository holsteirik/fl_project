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

