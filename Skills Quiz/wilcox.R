library(car)
library(mosaic)
library(tidyverse)

duncan <- Duncan %>% 
  dplyr::filter(type != "bc")

wilcox.test(prestige ~ type, data = duncan, alternative = "greater")

qqPlot(prestige ~ type, data = Duncan) 

davis <- Davis %>% 
  dplyr::filter(sex == "M")

davis$diff <- davis$weight - davis$repwt

wilcox.test(davis$diff)
