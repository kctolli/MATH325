library(mosaic)
library(car)
library(tidyverse)
library(pander)
library(ResourceSelection) 

pandary <- function(x){pander(summary(x))}

YoungAdults <- read_csv("https://raw.githubusercontent.com/saundersg/Statistics-Notebook/master/Data/YoungAdults.csv")
View(YoungAdults)


mylm <- lm(Weight ~ Height, data = YoungAdults)
pandary(mylm)

predict(mylm, data.frame(Height = 183))


myaov <- aov(Religiosity_score ~ Lying , data = YoungAdults)
pandary(myaov)

mynpar <- wilcox.test(Religiosity_score ~ Dominant_hand, data = YoungAdults, mu = 0, alternative = "two.sided", conf.level = 0.95)
pander(mynpar)


newadults <- YoungAdults %>% 
  filter(Number_of_siblings == 0) %>% 
  filter(Smoking == "never smoked") %>% 
  filter(Lying == "never")

View(newadults)


YoungAdults <- YoungAdults %>% mutate(BMI = Weight/(Height/100)^2)

ggplot(data = YoungAdults, aes(x = Gender, y = BMI)) + 
  geom_boxplot()

mypar <- lm(Fear_score ~ Number_of_siblings, data = YoungAdults)
pandary(mypar)

YoungAdults %>% mean(as.numeric(Fear_score))


drink <- kruskal.test(Religiosity_score ~ Alcohol, data = YoungAdults)
pander(drink)

pander(favstats(Religiosity_score ~ Alcohol, data = YoungAdults)[,-10])

x <- table(YoungAdults$Punctuality, YoungAdults$Lying)

chisq.test(x)


YA2 <- YoungAdults %>% 
  select(I_like_music, Music_diversity_score) %>%
  filter(I_like_music %in% c("Agree","Disagree","Neutral","Strongly Agree", "Strongly Disagree")) %>%
  mutate(I_like_music = factor(I_like_music, levels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered=TRUE)) %>%
  na.omit()


YoungAdults.hwg <- YoungAdults %>%
  select(Height,Weight,Gender)%>%
  na.omit() 

ggplot(YoungAdults.hwg, aes(x=Height, y=Weight, color=Gender))+
  geom_point() +
  geom_smooth(method="lm", se=F)

mymlm <- lm(Weight ~ Height + Gender + Height:Gender, data = YoungAdults.hwg)
pandary(mymlm)

wilcox <- wilcox.test(Religiosity_score ~ Happiness_score, data = YoungAdults, mu = 0, alternative = "two.sided", conf.level = 0.95)

YA3 <- YoungAdults %>%  
  filter(Alcohol == "never") %>% 
  filter(Smoking == "never smoked") %>% 
  filter(Lying == "never")

ggplot(data = YA3, aes(x = Age, y = Height, color = factor(Gender))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#Step 1 Compute a test statistic for the original data.
myTest <- t.test(Music_diversity_score ~ Gender, data = YoungAdults, mu = 0, alternative = "two.sided", conf.level = 0.95)
observedTestStat <- myTest$statistic

#Step 2
N <- 10000      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permutedData <- sample(x=YoungAdults$Gender)
  permutedTest <- t.test(Music_diversity_score~permutedData ,data = YoungAdults,mu=0)
  permutedTestStats[i] <- permutedTest$statistic
}

hist(permutedTestStats)
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N


welch <- t.test(Happiness_score ~ Gender, data = YoungAdults, mu = 0, alternative = "less", conf.level = 0.95)
welch


#Part A

YoungAdults_biggerfam <- YoungAdults %>%
  filter(Number_of_siblings > 3)
View(YoungAdults_biggerfam)

plot(Religiosity_score ~ Fear_score, data=YoungAdults_biggerfam)
relig_vs_fear.lm <- lm(Religiosity_score ~ Fear_score, data=YoungAdults_biggerfam)
abline(relig_vs_fear.lm)



#Part B

N <- 2000
permutedTestStats <- rep(NA, N)
set.seed(121)
for (i in 1:N){
  permutedData <- sample(YoungAdults_biggerfam$Religiosity_score)
  permutedTest <- with(YoungAdults_biggerfam, lm(Fear_score ~ permutedData))
  permutedTestStats[i] <- permutedTest$statistic
}

hist(permutedTestStats)
abline(v=observedTestStat)


pandary(aov(Weight ~ I_live_a_healthy_lifestyle + Gender + I_live_a_healthy_lifestyle:Gender, data=YoungAdults))

xyplot(Weight ~ I_live_a_healthy_lifestyle, data=YoungAdults, groups=Gender, type=c("p","a"), main="", auto.key=list(corner=c(1,1)))

YA4 <- YoungAdults %>% filter(Gender == "male", I_live_a_healthy_lifestyle == "Agree") %>% na.omit()

mean(YA4$Weight)

YoungAdults %>%
  filter(I_like_music != "#N/A", !is.na(Gender), !is.na(Punctuality)) %>%
  mutate(I_like_music = factor(I_like_music, levels=c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered=TRUE)) %>%
  ggplot(aes(x=I_like_music,y=Happiness_score)) + 
  geom_boxplot() + 
  facet_wrap(~Punctuality) +
  coord_flip()

kruskal.test(Music_diversity_score ~ Education, data = YoungAdults)
