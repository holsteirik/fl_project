#install.packages(c("tidyverse", "corrr", "ggcorrplot","lm.beta","lavaan","semTools","semTable","semPlot","sjPlot", "flextable"))

library(lm.beta) # Get standardized regression coefficients
library(lavaan) # The lavaan SEM package
library(semPlot) # Plotting of path models (the plot_model function uses this)
library(psych)
library(haven)
library(lavaanPlot)
library(semTools)
library(qgraph)
library(semTools)

# Read the data 

data <- read_sav("C:/Users/eho093/OneDrive - UiT Office 365/Documents/GitHub/fl_project/data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_13.januar2024(2) (1).sav")
#data <- read_sav("C:/Users/holst/OneDrive - UiT Office 365/Documents/GitHub/fl_project/data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_13.januar2024(2) (1).sav")
options(max.print = 2500)
describe(data)

windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")
# ==============================================================
# Helper code to format the path diagrams

# This variable contains the colour for the path diagrams
PSY9140_diag_color= list(
  lat = rgb(0.95, 0.95, 0.65), 
  man = rgb(0.75, 0.95, 0.90))


# The following function must be read for the "plot_model()" calls to work.
# It is simply a wrapper for the semPaths() function in the semPlot package,
# but means we don't have to write so much code to get decent formatting on our figures.
plot_model <- function (model, whatLabels="est", title="" )
{
  semPaths(model, layout="tree", style="openmx", intercepts=TRUE, edge.label.position = 0.80,
           whatLabels=whatLabels, edge.label.cex=1, label.cex=1,  sizeMan=5,color = PSY9140_diag_color,
           sizeLat=7, residuals=TRUE,esize=1,asize=3,fade=TRUE, mar=c(3,3,3,3),edge.label.margin =0.01)
  title(title, line=3, adj =0)
  
}

# Enfaktor modell for EF
model1 <- "EF =~ 
            BRIEF_IMPULSHEMMING_RAW + BRIEF_FLEKSIBILITET_RAW + 
            BRIEF_EMOSJONELLKONTROLL_RAW + BRIEF_SELVMONITORERING_RAW + 
            BRIEF_INITIERING_RAW + BRIEF_ARBEIDSHUKOMMELSE_RAW + 
            BRIEF_PLANLEGGING_RAW + BRIEF_OPPGAVEMONITORERING_RAW +
            BRIEF_ORGANISERING_RAW"

# Running the model
# Fit the model using RMLE estimator
model1_fit <- sem(model1, meanstructure = TRUE, data = data, estimator = "MLR")


# Providing output for the model
model1_summary <- summary(model1_fit,  fit.measures = TRUE, standardized=TRUE, modindices = TRUE)

model1_summary
plot_model(model1_fit, "std")

# Tofaktor modell for EF
model2 <- "AI =~ 
            BRIEF_IMPULSHEMMING_RAW + BRIEF_FLEKSIBILITET_RAW + 
            BRIEF_EMOSJONELLKONTROLL_RAW + BRIEF_SELVMONITORERING_RAW
          MI =~ 
            BRIEF_INITIERING_RAW + BRIEF_ARBEIDSHUKOMMELSE_RAW + 
            BRIEF_PLANLEGGING_RAW + BRIEF_OPPGAVEMONITORERING_RAW +
            BRIEF_ORGANISERING_RAW
          AI ~~ MI"

model2_fit <- sem(model2, meanstructure = TRUE, data =data,estimator = "MLR")
summary(model2_fit, fit.measures = TRUE, standardized=TRUE, modindices = TRUE)
model2_summary <- summary(model2_fit)
plot_model(model2_fit, "std")
anova(model1_fit, model2_fit)

# Trefaktor modell
model3 <- "Behavioural regulation =~ 
            BRIEF_IMPULSHEMMING_RAW + BRIEF_SELVMONITORERING_RAW
           Emotinal regulation =~ 
            BRIEF_FLEKSIBILITET_RAW + BRIEF_EMOSJONELLKONTROLL_RAW
           Metacognition =~ 
            BRIEF_INITIERING_RAW + BRIEF_ARBEIDSHUKOMMELSE_RAW + 
            BRIEF_PLANLEGGING_RAW + BRIEF_OPPGAVEMONITORERING_RAW +
            BRIEF_ORGANISERING_RAW" 

model3_fit <- sem(model3, meanstructure = TRUE, data = data, estimator = "MLR")



# Print the summary
correlations <- inspect(model3_fit, "cor.lv")
print(correlations)
summary(model3_fit, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
summary(model3_fit, fit.measures = TRUE, standardized = FALSE, modindices = TRUE)

model3_summary <- summary(model3_fit)
plot_model(model3_fit)
plot_model(model3_fit, "std")

# Firefakotor modell
model4 <- "BR =~ 
            BRIEF_IMPULSHEMMING_RAW + BRIEF_SELVMONITORERING_RAW
           ER =~ 
            BRIEF_FLEKSIBILITET_RAW + BRIEF_EMOSJONELLKONTROLL_RAW
           MI_external =~ 
            BRIEF_INITIERING_RAW + BRIEF_ARBEIDSHUKOMMELSE_RAW + 
            BRIEF_PLANLEGGING_RAW 
           MI_internal =~
            BRIEF_OPPGAVEMONITORERING_RAW +
            BRIEF_ORGANISERING_RAW" 
                        
model4_fit <- lavaan::sem(model4, meanstructure = TRUE, data = data, estimator = "MLR")

summary(model4_fit, fit.measures = TRUE, standardized=TRUE, modindices = TRUE)
model4_summary <- summary(model4_fit)
plot_model(model4_fit, "std")


# Retrieve CFI scores
cfi_model1 <- fitMeasures(model1_fit, c("cfi"))["cfi"]
cfi_model2 <- fitMeasures(model2_fit, c("cfi"))["cfi"]
cfi_model3 <- fitMeasures(model3_fit, c("cfi"))["cfi"]
cfi_model4 <- fitMeasures(model4_fit, c("cfi"))["cfi"]

# sjikvadrattest av modellene
anova(model1_fit, model2_fit)
anova(model2_fit, model3_fit)
anova(model3_fit, model4_fit)
anova(model2_fit, model4_fit)

# Print CFI scores
cat("CFI for Model 1:", cfi_model1, "\n")
cat("CFI for Model 2:", cfi_model2, "\n")
cat("CFI for Model 3:", cfi_model3, "\n")
cat("CFI for Model 4:", cfi_model4, "\n")


# Compute and store the fit measures including the RMSEA interval
fit_measures_model1 <- fitMeasures(model1_fit, c("chisq", "df", "cfi", "rmsea", "srmr", "tli"), rmsea.ci = TRUE)
fit_measures_model2 <- fitMeasures(model2_fit, c("chisq", "df", "cfi", "rmsea", "srmr", "tli"), rmsea.ci = TRUE)
fit_measures_model3 <- fitMeasures(model3_fit, c("chisq", "df", "cfi", "rmsea", "srmr", "tli"), rmsea.ci = TRUE)
fit_measures_model4 <- fitMeasures(model4_fit, c("chisq", "df", "cfi", "rmsea", "srmr", "tli"), rmsea.ci = TRUE)

# Print the fit measures for each model
print(fit_measures_model1)
print(fit_measures_model2)
print(fit_measures_model3)
print(fit_measures_model4)

# Calculate mean for each column and round to 2 decimal places
mean_values <- round(colMeans(data[, c("BRIEF_IMPULSHEMMING_RAW", "BRIEF_FLEKSIBILITET_RAW", 
                                       "BRIEF_EMOSJONELLKONTROLL_RAW", "BRIEF_SELVMONITORERING_RAW", 
                                       "BRIEF_INITIERING_RAW", "BRIEF_ARBEIDSHUKOMMELSE_RAW",
                                       "BRIEF_PLANLEGGING_RAW", "BRIEF_OPPGAVEMONITORERING_RAW",
                                       "BRIEF_ORGANISERING_RAW")]), 2)

# Calculate standard deviation for each column and round to 2 decimal places
sd_values <- round(apply(data[, c("BRIEF_IMPULSHEMMING_RAW", "BRIEF_FLEKSIBILITET_RAW", 
                                  "BRIEF_EMOSJONELLKONTROLL_RAW", "BRIEF_SELVMONITORERING_RAW", 
                                  "BRIEF_INITIERING_RAW", "BRIEF_ARBEIDSHUKOMMELSE_RAW",
                                  "BRIEF_PLANLEGGING_RAW", "BRIEF_OPPGAVEMONITORERING_RAW",
                                  "BRIEF_ORGANISERING_RAW")], 2, sd), 2)

# Display the mean values and standard deviations
mean_values
sd_values
# Select the specified columns and calculate the correlation matrix
cor_matrix <- cor(data[, c("BRIEF_IMPULSHEMMING_RAW", "BRIEF_FLEKSIBILITET_RAW", 
                           "BRIEF_EMOSJONELLKONTROLL_RAW", "BRIEF_SELVMONITORERING_RAW", 
                           "BRIEF_INITIERING_RAW", "BRIEF_ARBEIDSHUKOMMELSE_RAW",
                           "BRIEF_PLANLEGGING_RAW", "BRIEF_OPPGAVEMONITORERING_RAW",
                           "BRIEF_ORGANISERING_RAW")])

# Display the correlation matrix
cor_matrix



# lag penere 3 faktormodell
semPaths(model3_fit, layout="tree", style="lisrel", intercepts=FALSE, 
         edge.label.position = 0.60, whatLabels="std", 
         edge.label.cex=0.8, label.cex=0.8, sizeMan=8, 
         color = "white", sizeLat=10, residuals=TRUE, 
         esize=2, asize=3, fade=FALSE, mar=c(5,5,5,5), 
         edge.label.margin =0.05, rotation = 2, nCharNodes = 20)


