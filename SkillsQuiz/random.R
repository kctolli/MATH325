library(tidyverse)
library(mosaic)
library(pander)

#Step 1
mycars <- mtcars %>% filter(cyl %in% c(4,8))

myTest <- t.test(wt ~ cyl, data = mycars, mu = 0)
observedTestStat <- myTest$statistic; observedTestStat

#Step 2
N <- 2000      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permutedData <- sample(mycars$cyl) 
  permutedTest <- t.test(wt ~ permutedData, data = mycars, mu = 0)
  permutedTestStats[i] <- permutedTest$statistic
}
hist(permutedTestStats, xlim = c(-7,7))
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N
observedTestStat


# Create the data:
set.seed(1140411)
sample1 <- rnorm(30, 69, 2.5)
sample2 <- rnorm(30, 69, 2.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30))
#View(theData)
boxplot(values ~ group, data = theData)



# Run the permutation test:

myTest <-  t.test(values~group,data = theData,mu=0)
observedTestStat <- myTest$statistic; observedTestStat

N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=theData$group)
  permutedTest <- t.test(values~permutedData ,data = theData,mu=0)
  permutedTestStats[i] <- permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N
sum(permutedTestStats %in% observedTestStat)/N


set.seed(121)
sample1 <- rnorm(30, 185, 8)
sample2 <- sample1 - rnorm(30, 0, 3.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30), id = rep(c(1:30),times=2))
#View(theData)
with(theData, hist(values[group==1] - values[group==2]))

# Perform the permutation test:

myTest <- t.test(values ~ group, data = theData, paired = TRUE, mu = 0)
observedTestStat <- myTest$statistic

N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=c(1,-1), size=30, replace = TRUE)
  permutedTest <- with(theData, t.test(permutedData*(values[group == 1] - values[group == 2]), mu = 0))
  permutedTestStats[i]  <-  permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N


?SaratogaHouses
View(SaratogaHouses)
table(SaratogaHouses$fuel)


kruskal.test(price ~ fuel, data=SaratogaHouses)
boxplot(price ~ fuel, data=SaratogaHouses)


?ToothGrowth

xyplot(len ~ dose, groups=supp, data=ToothGrowth, type=c("p","a"), auto.key=TRUE)

summary(aov(len ~ dose+supp+dose:supp, data=ToothGrowth))


boxplot(cloudcover ~ weekday, data=RailTrail, names=c("Weekend/Holiday", "Weekday"), ylab="Cloud Cover Measurement (in oktas)")


t.test(cloudcover ~ weekday, data=RailTrail, mu = 0, alternative = "two.sided", conf.level = 0.95)
