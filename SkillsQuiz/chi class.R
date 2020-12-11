library(tidyverse)
GSS2012 <- read_delim("G:/MATH325/Data/GSS2012.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

names(GSS2012)

table(GSS2012$widowed); barplot(table(GSS2012$widowed))

table(GSS2012$version); barplot(table(GSS2012$version))

x <- table(GSS2012$widowed, GSS2012$version); x

barplot(x)

testresults <- chisq.test(x)

testresults$expected
testresults$residuals
