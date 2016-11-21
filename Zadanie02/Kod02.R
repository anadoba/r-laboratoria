#Adam Nadoba
#Problem 2

# pakiety 'zoo' i 'ts' sa uzywane do Szeregow czasowych
#install.packages('zoo')
library(zoo)

setwd('c:/r-laboratoria/Zadanie02')


sczytaneIndeksy <- read.csv(file='Dane02.csv', head=TRUE)
IloscRekordow <- nrow(sczytaneIndeksy)

indeksy <- sczytaneIndeksy[,0-2]
indeksy[,1] <- as.yearmon(paste(sczytaneIndeksy[,1], sczytaneIndeksy[,2], sep="-"))
names(indeksy)[1] <- 'data'
indeksy[,2] <- sczytaneIndeksy[,3]

szereg <- zoo(indeksy$wartosc, indeksy$data)
plot.zoo(szereg, xlab="Oœ czasu", ylab="Indeks dyskontowy obligacji TP10J18", xaxt="n", type="b")
axis(1, at = indeksy[,1], labels = format(indeksy[,1], "%m-%Y"))

# srednia chronologiczna
# ostatnie wyniki sie inaczej wazy, przy sumowaniu dodajemy tylko polowe pierwszego i ostatniego elementu
# i dzielimy przez liczbe elementow-1
sredniaChrono <- (1/2*indeksy[,2][1] + sum(indeksy[,2][2:(IloscRekordow-1)]) + 1/2*indeksy[,2][IloscRekordow] )/(IloscRekordow-1)
print(round(sredniaChrono, digits = 2))

# szereg czasowy indekow lancuchowych
indeksyLancuchowe <- (indeksy[,2][-1] / indeksy[,2][-IloscRekordow])
print(round(indeksyLancuchowe * 100, digits = 2))

# jednopodstawowe
indeksyJednopodstawowe <- (indeksy[,2][-1] / indeksy[,2][1])
print(round(indeksyJednopodstawowe * 100, digits = 2))

# gdy mowimy o indeksach zawsze mamy na mysli srednia geometryczna
# iloczyn elementow od 1 do n a potem calosc do potegi 1/n
# i po uproszczeniu e do potegi
sredniaIndeksLancuchowy <- exp(mean(log(indeksy[,2][-1]/ indeksy[,2][-IloscRekordow])))
print(round(sredniaIndeksLancuchowy, digits = 2))

sredniaIndeksJednopodstawowy <- exp(mean(log(indeksy[,2][-1]/ indeksy[,2][1])))
print(round(sredniaIndeksJednopodstawowy, digits = 2))
