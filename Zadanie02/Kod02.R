#Adam Nadoba
#Zadanie 02
#Zestaw 8

# pakiety 'zoo' i 'ts' sa uzywane do Szeregow czasowych
#install.packages('zoo')
library(zoo)

setwd('/users/adam/Studia/r-laboratoria/Zadanie02')

sczytaneIndeksy <- read.csv(file='Dane02.csv', head=TRUE)
#indeksy[,2] <- as.character(indeksy[,2])

indeksy <- sczytaneIndeksy[,0-2]
indeksy[,1] <- as.yearmon(paste(sczytaneIndeksy[,1], sczytaneIndeksy[,2], sep="-"))
names(indeksy)[1] <- 'data'
indeksy[,2] <- sczytaneIndeksy[,3]

szereg <- zoo(indeksy$wartosc, indeksy$data)
plot.zoo(szereg)

# srednia chronologiczna
# ostatnie wyniki sie inaczej wazy, przy sumowaniu dodajemy tylko polowe pierwszego i ostatniego elementu
# i dzielimy przez liczbe o jeden mniejsza
(1/2*indeksy[,2][1] + sum(indeksy[,2][2:16]) + 1/2*indeksy[,2][17] )/23

# szereg czasowy indekow lancuchowych
plot(indeksy[,2][-1] / indeksy[,2][-17])

# jednopodstawowe
plot(indeksy[,2][-1] / indeksy[,2][1])

# gdy mowimy o indeksach zawsze mamy na mysli srednia geometryczna
# iloczyn elementow od 1 do n a potem calosc do potegi 1/n
# i po uproszczeniu e do potegi
exp(mean(log(indeksy[,2][-1]/ indeksy[,2][-17])))

exp(mean(log(indeksy[,2][-1]/ indeksy[,2][1])))

#format(indeksy[,1], "%m-%Y")



# przygotowac cos z pakietu MCMC
# https://cran.r-project.org/web/packages/mcmc/index.html
