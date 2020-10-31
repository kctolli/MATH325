library(car)
<<<<<<< Updated upstream
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
=======
library(pander)
library(mosaic)

duncan <- Duncan %>% filter(type != "bc")

pander(wilcox.test(prestige ~ type, data = duncan))

View(Salaries)

salaries <- Salaries %>% filter(rank == "Prof")

pander(favstats(salary ~ sex, data = salaries))

pander(wilcox.test(salary ~ sex, data = salaries))
>>>>>>> Stashed changes
