# Load the required libraries
library(psych)
library(GPArotation)
library(papaja)
library(haven)
library(apaTables)
library(ggplot2)
library(ggcorrplot)


# Read the data
data <- haven::read_sav("C:/Users/eho093/OneDrive - UiT Office 365/Documents/GitHub/fl_project/data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_13.januar2024(2) (1).sav")

# Assign items to each subfactor
inhibit <- data[, c('BRIEF_5', 'BRIEF_16', 'BRIEF_29', 'BRIEF_36', 'BRIEF_43', 'BRIEF_55', 'BRIEF_58', 'BRIEF_73')]
shift <- data[, c("BRIEF_8", "BRIEF_22", "BRIEF_32", "BRIEF_44", "BRIEF_61", "BRIEF_67")]
emotional_control <- data[, c("BRIEF_1", "BRIEF_12", "BRIEF_19", "BRIEF_28", "BRIEF_33", "BRIEF_42", "BRIEF_51", "BRIEF_57", "BRIEF_69", "BRIEF_72")]
self_monitoring <- data[, c("BRIEF_13", "BRIEF_23", "BRIEF_37", "BRIEF_50", "BRIEF_64", "BRIEF_70")]
initiate <- data[, c("BRIEF_6", "BRIEF_14", "BRIEF_20", "BRIEF_25", "BRIEF_45", "BRIEF_49", "BRIEF_53", "BRIEF_62")]
working_memory <- data[, c("BRIEF_4", "BRIEF_11", "BRIEF_17", "BRIEF_26", "BRIEF_35", "BRIEF_46", "BRIEF_56", "BRIEF_68")]
plan_organize <- data[, c("BRIEF_9", "BRIEF_15", "BRIEF_21", "BRIEF_34", "BRIEF_39", "BRIEF_48", "BRIEF_54", "BRIEF_63", "BRIEF_66", "BRIEF_71")]
monitoring <- data[, c("BRIEF_2", "BRIEF_18", "BRIEF_24", "BRIEF_41", "BRIEF_52", "BRIEF_75")]
organization <- data[, c("BRIEF_3", "BRIEF_7", "BRIEF_29", "BRIEF_30", "BRIEF_40", "BRIEF_60", "BRIEF_65", "BRIEF_73")]

# Calculate alpha and omega reliabilities for each subfactor
alpha_inhibit <- psych::alpha(inhibit)$total$raw_alpha
alpha_shift <- psych::alpha(shift)$total$raw_alpha
alpha_emotional_control <- psych::alpha(emotional_control)$total$raw_alpha
alpha_self_monitoring <- psych::alpha(self_monitoring)$total$raw_alpha
alpha_initiate <- psych::alpha(initiate)$total$raw_alpha
alpha_working_memory <- psych::alpha(working_memory)$total$raw_alpha
alpha_plan_organize <- psych::alpha(plan_organize)$total$raw_alpha
alpha_monitoring <- psych::alpha(monitoring)$total$raw_alpha
alpha_organization <- psych::alpha(organization)$total$raw_alpha

organization_omega

inhibit_omega <- psych::omega(inhibit)
shift_omega <- psych::omega(shift)
emotional_control_omega <- psych::omega(emotional_control)
self_monitoring_omega <- psych::omega(self_monitoring)
initiate_omega <- psych::omega(initiate)
working_memory_omega <- psych::omega(working_memory)
plan_organize_omega <- psych::omega(plan_organize)
monitoring_omega <- psych::omega(monitoring)
organization_omega <- psych::omega(organization)

# Print the alpha reliabilities
cat("Alpha Reliabilities:\n")
cat("Inhibit - ", alpha_inhibit, "\n")
cat("Shift - ", alpha_shift, "\n")
cat("Emotional Control - ", alpha_emotional_control, "\n")
cat("Self Monitoring - ", alpha_self_monitoring, "\n")
cat("Initiate - ", alpha_initiate, "\n")
cat("Working Memory - ", alpha_working_memory, "\n")
cat("Plan/Organize - ", alpha_plan_organize, "\n")
cat("Monitoring - ", alpha_monitoring, "\n")
cat("Organization - ", alpha_organization, "\n\n")

# Print the omega reliabilities
cat("Omega Reliabilities:\n")
cat("Inhibit - ", inhibit_omega$omega_h, "\n")
cat("Shift - ", shift_omega$omega_h, "\n")
cat("Emotional Control - ", emotional_control_omega$omega_h, "\n")
cat("Self Monitoring - ", self_monitoring_omega$omega_h, "\n")
cat("Initiate - ", initiate_omega$omega_h, "\n")
cat("Working Memory - ", working_memory_omega$omega_h, "\n")
cat("Plan/Organize - ", plan_organize_omega$omega_h, "\n")
cat("Monitoring - ", monitoring_omega$omega_h, "\n")
cat("Organization - ", organization_omega$omega_h, "\n")


# Assuming 'data' is your data frame
selected_items <- data[, c(
  "BRIEF_IMPULSHEMMING_RAW",
  "BRIEF_FLEKSIBILITET_RAW",
  "BRIEF_EMOSJONELLKONTROLL_RAW",
  "BRIEF_SELVMONITORERING_RAW",
  "BRIEF_INITIERING_RAW",
  "BRIEF_ARBEIDSHUKOMMELSE_RAW",
  "BRIEF_PLANLEGGING_RAW",
  "BRIEF_OPPGAVEMONITORERING_RAW",
  "BRIEF_ORGANISERING_RAW"
)]

# Compute the correlation matrix
cor_matrix <- cor(selected_items, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)




selected_items <- data[, c(
  "BRIEF_IMPULSHEMMING_RAW",
  "BRIEF_FLEKSIBILITET_RAW",
  "BRIEF_EMOSJONELLKONTROLL_RAW",
  "BRIEF_SELVMONITORERING_RAW",
  "BRIEF_INITIERING_RAW",
  "BRIEF_ARBEIDSHUKOMMELSE_RAW",
  "BRIEF_PLANLEGGING_RAW",
  "BRIEF_OPPGAVEMONITORERING_RAW",
  "BRIEF_ORGANISERING_RAW"
)]

# Assuming you have already calculated the correlation matrix for the selected items and stored it in 'correlation_matrix'

# Assuming you have already calculated the correlation matrix for the selected items and stored it in 'correlation_matrix'

# Convert the correlation matrix to a data frame
cor_df <- as.data.frame(cor_matrix)

# Create a blank plot with no color
plot <- ggplot() +
  geom_blank(data = cor_df, aes(x = Var1, y = Var2)) +
  geom_text(data = cor_df, aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", size = 3) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_fixed()

# Adjust plot margins
plot <- plot + theme(plot.margin = margin(5, 5, 20, 20))

# Display the plot
print(plot)
