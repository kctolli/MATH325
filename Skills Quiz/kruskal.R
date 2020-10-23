library(mosaic)

?SaratogaHouses
View(SaratogaHouses)
table(SaratogaHouses$fuel)

pander(kruskal.test(price ~ fuel, data = SaratogaHouses))

pander(favstats(price ~ fuel, data = SaratogaHouses)[,-10])
