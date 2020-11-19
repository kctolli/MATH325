library(mosaic)
library(ResourceSelection) 
library(pander)
library(tidyverse)
library(DT) 

data(challenger)

pandary <- function(x){pander(summary(x))}

glm <- glm(sex == "B" ~ length, data = KidsFeet, family = binomial)
pandary(glm)

pander(predict(glm, data.frame(length = 25), type = "response"))

# chall.glm <- glm(Fail>0 ~ Temp, data=challeng, family=binomial)
# pandary(chall.glm)

View(infert)
?infert
infert.glm <- glm( (spontaneous > 0) ~ age, data=infert, family=binomial)

pandary(infert.glm)

plot( (spontaneous > 0) ~ age, data=infert)
curve( exp(1.487 + -0.05616*x)/(1 + exp(1.487 + -0.05616*x)), add=TRUE)
#This code only works if you replace b0 and b1 with numbers.

table(infert$age)

pchisq(334.0, 246, lower.tail=FALSE)

View(Galton)

glm <- glm(sex == "M" ~ height, data = Galton, family = binomial)
pandary(glm)

plot( (sex == "M") ~ height, data=Galton)
curve( exp(-52.98 + 0.7968*x)/(1 + exp(-52.98 + 0.7968*x)), add=TRUE)

pander(predict(glm, data.frame(height = 65), type = "response"))

pchisq(626.2, 896, lower.tail=FALSE)

hoslem.test(glm$y, glm$fitted, g=10)
