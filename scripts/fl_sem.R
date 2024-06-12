library(tidyverse) # Collection of core packages (most notably dplyr and ggplot2)
library(corrr) # Prettier correlation matrices compatible with tidyverse
library(ggcorrplot) # Plot correlation matrices
library(lm.beta) # Get standardized regression coefficients
library(lavaan) # The lavaan SEM package
library(semTools) # Helper functions for the lavaan package
library(semTable) # Make APA formatted tables of estimates from SEM models
library(semPlot) # Plotting of path models (the plot_model function uses this)
library(sjPlot)
library(flextable)
library(psych)
library(haven)

# Read the dt file
dt <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")

# Rename variables
names(dt)[names(dt) == "KJONN"] <- "Sex"
names(dt)[names(dt) == "SumbisNoFour"] <- "Insomnia"
names(dt)[names(dt) == "consci"] <- "Conscientiousness"
names(dt)[names(dt) == "agree"] <- "Agreeableness"
names(dt)[names(dt) == "open"] <- "Openness"
names(dt)[names(dt) == "extra"] <- "Extraversion"
names(dt)[names(dt) == "neuro"] <- "Neuroticism"
names(dt)[names(dt) == "BAIsum"] <- "Anxiety"
names(dt)[names(dt) == "BDIsum"] <- "Depression"
names(dt)[names(dt) == "BRIEF_AI_T"] <- "bri"
names(dt)[names(dt) == "BRIEF_MI_T"] <- "mi"
dt$Sex <- as.factor(dt$Sex)

# This variable contains the colour for the path diagrams
PSY9140_diag_color= list(
  lat = rgb(0.95, 0.95, 0.65), 
  man = rgb(0.75, 0.95, 0.90))


# plot_model()
plot_model <- function (model, whatLabels="std", title="" )
{
  semPaths(model, layout="tree", style="openmx", intercepts=TRUE, edge.label.position = 0.80,
           whatLabels=whatLabels, edge.label.cex=0.5, label.cex=0.8,  sizeMan=5,color = PSY9140_diag_color,
           sizeLat=7, residuals=TRUE,esize=1,asize=3,fade=TRUE, mar=c(3,3,3,3),edge.label.margin =0.01)
  title(title, line=3, adj =0)
  
}

model_1 <- "
   bri ~ Sex + 
            Agreeableness + Conscientiousness + Neuroticism + 
            Depression + Anxiety + Insomnia
   mi ~ Sex + 
            Agreeableness + Conscientiousness + Neuroticism + 
            Depression + Anxiety + Insomnia
   bri ~~ 0* mi"
fit <- sem(model_1, data = dt)
plot_model(fit)
summary(fit, standardized = T, fit.measures = T)


