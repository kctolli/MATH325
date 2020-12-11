library(mosaic)

glasses <- cbind( Males = c(Glasses = 5, Contacts = 12, None = 18), Females = c(Glasses = 4, Contacts = 14, None = 22))
glasses
barplot(glasses, beside=TRUE, legend.text=TRUE, args.legend=list(x = "topleft", bty="n"))

chis.glasses <- chisq.test(glasses); chis.glasses

chis.glasses$expected 




education <- cbind( `United States` = c(Engineering = 61941, `Natural Science` = 111158, `Social Science` = 182166), `Western Europe` = c(Engineering = 158931, `Natural Science` = 140126, `Social Science` = 116353), Asia = c(280772, 242879, 236018))
education

barplot(education, beside=TRUE, legend.text=TRUE, args.legend=list(x = "topleft", bty="n"))

chis.education <- chisq.test(education); chis.education

chis.education$expected
chis.education$residuals

mean(chis.education$expected)
mean(chis.education$residuals)


?InsectSprays

summary(aov(count~spray, data = InsectSprays))

sprays <- InsectSprays %>% 
  group_by(spray) %>% 
  summarise(count = sum(count))

chis.sprays <- chisq.test(sprays); chis.sprays
