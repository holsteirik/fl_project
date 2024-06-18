library(lm.beta) # Get standardized regression coefficients
library(lavaan) # The lavaan SEM package
library(semPlot) # Plotting of path models (the plot_model function uses this)
library(psych)
library(haven)
library(lavaanPlot)
library(semTools)
library(semTools)
library(tidyverse)
library(writexl)

# Read the data 

data <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")

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
            BRIEF_IMPULSHEMMING_T + BRIEF_FLEKSIBILITET_T + 
            BRIEF_EMOSJONELLKONTROLL_T + BRIEF_SELVMONITORERING_T + 
            BRIEF_INITIERING_T + BRIEF_ARBEIDSHUKOMMELSE_T + 
            BRIEF_PLANLEGGING_T + BRIEF_OPPGAVEMONITORERING_T +
            BRIEF_ORGANISERING_T"

# Running the model
# Fit the model using RMLE estimator
model1_fit <- sem(model1, meanstructure = TRUE, data = data, estimator = "ML")


# Providing output for the model
model1_summary <- summary(model1_fit,  fit.measures = TRUE, standardized=TRUE, 
                          modindices = TRUE)

model1_summary
plot_model(model1_fit, "std")

# Tofaktor modell for EF
model2 <- "AI =~ 
            BRIEF_IMPULSHEMMING_T + BRIEF_FLEKSIBILITET_T + 
            BRIEF_EMOSJONELLKONTROLL_T + BRIEF_SELVMONITORERING_T
          MI =~ 
            BRIEF_INITIERING_T + BRIEF_ARBEIDSHUKOMMELSE_T + 
            BRIEF_PLANLEGGING_T + BRIEF_OPPGAVEMONITORERING_T +
            BRIEF_ORGANISERING_T
          AI ~~ MI"

model2_fit <- sem(model2, meanstructure = TRUE, data =data,estimator = "ML")
summary(model2_fit, fit.measures = TRUE, standardized=TRUE, modindices = TRUE)
model2_summary <- summary(model2_fit)
plot_model(model2_fit, "std")
anova(model1_fit, model2_fit)

# Trefaktor modell
model3 <- "Behavioural regulation =~ 
            BRIEF_IMPULSHEMMING_T + BRIEF_SELVMONITORERING_T
           Emotinal regulation =~ 
            BRIEF_FLEKSIBILITET_T + BRIEF_EMOSJONELLKONTROLL_T
           Metacognition =~ 
            BRIEF_INITIERING_T + BRIEF_ARBEIDSHUKOMMELSE_T + 
            BRIEF_PLANLEGGING_T + BRIEF_OPPGAVEMONITORERING_T +
            BRIEF_ORGANISERING_T" 

model3_fit <- sem(model3, meanstructure = TRUE, data = data, estimator = "ML")



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
            BRIEF_IMPULSHEMMING_T + BRIEF_SELVMONITORERING_T
           ER =~ 
            BRIEF_FLEKSIBILITET_T + BRIEF_EMOSJONELLKONTROLL_T
           MI_external =~ 
            BRIEF_INITIERING_T + BRIEF_ARBEIDSHUKOMMELSE_T + 
            BRIEF_PLANLEGGING_T 
           MI_internal =~
            BRIEF_OPPGAVEMONITORERING_T +
            BRIEF_ORGANISERING_T" 

model4_fit <- lavaan::sem(model4, meanstructure = TRUE, data = data, 
                          estimator = "ML")

summary(model4_fit, fit.measures = TRUE, standardized=TRUE, modindices = TRUE)
model4_summary <- summary(model4_fit)
plot_model(model4_fit, "std")


# Retrieve CFI scores
cfi_model1 <- fitMeasures(model1_fit, c("cfi"))["cfi"]
cfi_model2 <- fitMeasures(model2_fit, c("cfi"))["cfi"]
cfi_model3 <- fitMeasures(model3_fit, c("cfi"))["cfi"]
cfi_model4 <- fitMeasures(model4_fit, c("cfi"))["cfi"]

# sjikvadrattest av modellene
anova(model1_fit, model2_fit, use.scaled = TRUE)
anova(model2_fit, model3_fit)
anova(model3_fit, model4_fit)
anova(model2_fit, model4_fit)

# Print CFI scores
cat("CFI for Model 1:", cfi_model1, "\n")
cat("CFI for Model 2:", cfi_model2, "\n")
cat("CFI for Model 3:", cfi_model3, "\n")
cat("CFI for Model 4:", cfi_model4, "\n")


# Compute and store the fit measures including the RMSEA interval
fit_measures_model1 <- fitMeasures(model1_fit, 
                                   c("chisq", "df", "cfi", "rmsea.ci", "srmr", "tli", "aic", "bic"), rmsea.ci = TRUE)
fit_measures_model2 <- fitMeasures(model2_fit, 
                                   c("chisq", "df", "cfi", "rmsea.ci", "srmr", "tli", "aic", "bic"), rmsea.ci = TRUE)
fit_measures_model3 <- fitMeasures(model3_fit, 
                                   c("chisq", "df", "cfi", "rmsea.ci", "srmr", "tli", "aic", "bic"), rmsea.ci = TRUE)
fit_measures_model4 <- fitMeasures(model4_fit, 
                                   c("chisq", "df", "cfi", "rmsea.ci", "srmr", "tli", "aic", "bic"), rmsea.ci = TRUE)

# Print the fit measures for each model
print(fit_measures_model1)
print(fit_measures_model2)
print(fit_measures_model3)
print(fit_measures_model4)

#####
# lag tabell i excel
# Install and load the writexl package if not already installed


# Combine the fit measures into a data frame
fit_measures <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Chisq = c(fit_measures_model1["chisq"], fit_measures_model2["chisq"], fit_measures_model3["chisq"], fit_measures_model4["chisq"]),
  DF = c(fit_measures_model1["df"], fit_measures_model2["df"], fit_measures_model3["df"], fit_measures_model4["df"]),
  CFI = c(fit_measures_model1["cfi"], fit_measures_model2["cfi"], fit_measures_model3["cfi"], fit_measures_model4["cfi"]),
  RMSEA = c(fit_measures_model1["rmsea"], fit_measures_model2["rmsea"], fit_measures_model3["rmsea"], fit_measures_model4["rmsea"]),
  SRMR = c(fit_measures_model1["srmr"], fit_measures_model2["srmr"], fit_measures_model3["srmr"], fit_measures_model4["srmr"]),
  TLI = c(fit_measures_model1["tli"], fit_measures_model2["tli"], fit_measures_model3["tli"], fit_measures_model4["tli"]),
  AIC = c(fit_measures_model1["aic"], fit_measures_model2["aic"], fit_measures_model3["aic"], fit_measures_model4["aic"]),
  BIC = c(fit_measures_model1["bic"], fit_measures_model2["bic"], fit_measures_model3["bic"], fit_measures_model4["bic"])
)

# Write the fit measures to an Excel file
write_xlsx(fit_measures, "fit_measures.xlsx")

