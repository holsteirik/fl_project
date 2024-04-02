library(haven)

data <- read_sav("data/forskerlinje_friskestudenter_bdi_og_insomni_variabler_retta_01.mars2024_runar.sav")

cor(data[c("BDI_sum", "BAI_sum")], method = "pearson")

cor_result <- cor(data(na.omit[c("BDI_sum", "BAI_sum")], method = "pearson")


cor_matrix <- round(cor(na.omit(data[c("BRIEF_AI_T", "BRIEF_MI_T", "BDIsum", 
                                       "BAIsum", "neuro", "extra", "open", 
                                       "agree", "consci", "SumbisNoFour", "KJONN")], 
                                method = "spearman"), 
                        use = "pairwise.complete.obs"), 2)
cor_matrix
         