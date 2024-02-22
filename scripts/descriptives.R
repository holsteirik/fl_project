library(igraph)
library(qgraph)
library(haven)
library(psych)
library(ggplot2)

data <- read_sav("data/forskerlinje_all_data_8feb.sav")

options(max.print = 2500)
describe(data)

theme_set(theme_bw())

g <- ggplot(data, aes(x = BRIEF_MI_RAW, y = BDIsum))

g + geom_point(color = "red", shape = "circle", size = 1.5) + 
  geom_line(color = "red", linetype = "dotted", lwd = .3) +
  labs(x = "Metacogniton", y = "Symptoms of Depression") +
  theme(axis.title.x = element_text(margin = margin(t = 10, size = 13), 
                                    axis.title.y =element_text(vjust = 2, size =13))

