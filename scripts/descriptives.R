library(igraph)
library(qgraph)
library(haven)
library(psych)
library(ggplot2)

data <- read_sav("data/forskerlinje_all_data_8feb.sav")

options(max.print = 2500)
describe(data)

g <- ggplot(data, aes(x = BRIEF_MI_T, y = BDIsum))
g + geom_point()
