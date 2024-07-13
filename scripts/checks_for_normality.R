library(performance)
library(haven)


data_raw <- read_sav("data/forskerlinje_friskestudenter_rettet_16_april.sav")

names(data_raw)[names(data_raw) == "SumbisNoFour"] <- "Insomnia"
names(data_raw)[names(data_raw) == "consci"] <- "Conscientiousness"
names(data_raw)[names(data_raw) == "agree"] <- "Agreeableness"
names(data_raw)[names(data_raw) == "neuro"] <- "Neuroticism"
names(data_raw)[names(data_raw) == "BAIsum"] <- "Anxiety"
names(data_raw)[names(data_raw) == "BDIsum"] <- "Depression"
names(data_raw)[names(data_raw) == "BRIEF_AI_T"] <- "Behavioral_regulation"
names(data_raw)[names(data_raw) == "BRIEF_MI_T"] <- "Metacognition"
names(data_raw)[names(data_raw) == "KJONN"] <- "Sex"



bri <- lm(Behavioral_regulation ~ as.factor(Sex) + Insomnia + Conscientiousness +
            Agreeableness + Neuroticism + Anxiety + Depression, data = data_raw)

mi <- lm(Metacognition ~ as.factor(Sex) + Insomnia + Conscientiousness +
           Agreeableness + Neuroticism + Anxiety + Depression, data = data_raw)

check_model(bri)
check_model(mi)
