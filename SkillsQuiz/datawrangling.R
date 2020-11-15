library(mosaic)
library(car)
library(DT)
library(pander)
library(tidyverse)

?airquality
View(airquality) 

airquality %>% 
  group_by(Month) %>% 
  summarize(mean = mean(Temp)) %>% 
  pander()

hist(airquality$Temp, xlab="Daily Temperature", main="LaGuardia Airport (May to September 1973)", col="slategray")

plot(Temp ~ Month, data=airquality, xlab="Month", ylab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray")

boxplot(Temp ~ Month, data=airquality, xlab="Month", ylab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray")

stripchart(Temp ~ Month, data=airquality, ylab="Month", xlab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray", method="stack")

plot(Temp ~ Day, data=airquality, xlab="Day of the Month", ylab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray")

?Orange
View(Orange)

Orange %>% 
  group_by(age) %>% 
  summarize(med = median(circumference)) %>% 
  pander()


plot(circumference ~ age, data=Orange, ylab="Trunk Circumference (mm)", xlab="Age of Trees (days)", main="Trunk Circumference of Orange Trees", col="ivory3", pch=15)
Orange.m <- median(circumference ~ age, data=Orange)
lines(names(Orange.m), Orange.m, col="ivory3")
legend("topleft", legend="Median Growth", lty=1, col='ivory3', bty='n')

boxplot(circumference ~ age, data=Orange, ylab="Trunk Circumference (mm)", xlab="Age of Trees (days)", main="Trunk Circumference of Orange Trees", col="ivory3")

stripchart(circumference ~ age, data=Orange, ylab="Trunk Circumference (mm)", xlab="Age of Trees (days)", main="Trunk Circumference of Orange Trees", col="ivory3", pch=15, method="stack", vertical=TRUE)

boxplot(Orange, xlab="Age of Tree (days)", main="Trunk Circumference of Orange Trees", col="ivory3")
