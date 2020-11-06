library(mosaic)
library(car)


SH2 <- filter(SaratogaHouses, bedrooms == 3,  newConstruction=="Yes")

View(SH2)

sh2.lm <- lm(price  ~  livingArea +  fireplaces + livingArea:fireplaces, data=SH2)
pander(summary(sh2.lm))       

sh3.lm <- lm(price  ~  livingArea +  livingArea:fireplaces, data=SH2)
pander(summary(sh3.lm))

predict(sh3.lm, data.frame(livingArea = 2500, fireplaces = 1))

par(mfrow=c(1,3))
plot(sh3.lm, which=1)
qqPlot(sh3.lm$residuals, id=FALSE)
plot(sh3.lm$residuals)

plot(price ~ livingArea, data=SH2, col=as.factor(fireplaces),main="Houses", pch=16)
abline(-87105, 143.1, col=palette()[1])
abline(-87105, 143.1+16.48, col=palette()[2])
legend("topleft", legend=c("No fireplaces", "fireplaces"), pch=1, col=palette(), title="fireplaces", bty="n")


View(Highway1)

highlm <- lm(rate ~ slim + shld + trks, data = Highway1)
pander(summary(highlm))   

plot(highlm, which=1)

qqPlot(highlm$residuals, id=FALSE)

plot(highlm$residuals)

predict(highlm, data.frame(slim = 55, shld = 6, trks = 10))


?mpg
View(mpg)

plot(hwy ~ cty, data = mpg)
mpg.lm <- lm(hwy ~ cty, data=mpg)
pander(summary(mpg.lm))

par(mfrow=c(1,2)) 
plot(mpg.lm, which=1:2)

par(mfrow=c(1,3))
plot(mpg.lm$residuals ~ cyl , data=mpg)
plot(mpg.lm$residuals ~ as.factor(drv), data=mpg)
plot(mpg.lm$residuals ~ displ, data=mpg)

mpg.lm <- lm(hwy ~ cty + cyl, data=mpg)
pander(summary(mpg.lm))

mpg.lm <- lm(hwy ~ cty + displ, data=mpg)
pander(summary(mpg.lm))

mpg.lm <- lm(hwy ~ cty + drv, data=mpg)
pander(summary(mpg.lm))
