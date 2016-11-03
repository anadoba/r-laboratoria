#Adam Nadoba
#Zadanie 01
#Zestaw 8

setwd("C:/r-laboratoria/Zadanie01")

spolki <- read.csv(file="Dane01.csv", head=TRUE)

#Wielkoœæ spó³ek
wielkoscKwartyle <- quantile(spolki[,2], type=6)
#Q1 = kwartyle[2] itd
wielkoscQ1 <- wielkoscKwartyle[2]
wielkoscQ2 <- wielkoscKwartyle[3]
wielkoscQ3 <- wielkoscKwartyle[4]

#dplyr przykrywa standardowe 'filter'
library(dplyr)

spolkiBardzoMale <- filter(spolki, spolki[2] <= wielkoscQ1)
spolkiMale <- filter(spolki, wielkoscQ1 < spolki[2] & spolki[2] <= wielkoscQ2)
spolkiSrednie <- filter(spolki, wielkoscQ2 < spolki[2] & spolki[2] <= wielkoscQ3)
spolkiDuze <- filter(spolki, wielkoscQ3 < spolki[2])

sredniaCzSpolkiBardzoMale <- mean(spolkiBardzoMale[,3])
medianaCzSpolkiBardzoMale <- median(spolkiBardzoMale[,3])
odchylenieCzSpolkiBardzoMale <- sqrt(sum((spolkiBardzoMale[,3] - sredniaCzSpolkiBardzoMale)^2)/nrow(spolkiBardzoMale))

sredniaCzSpolkiMale <- mean(spolkiMale[,3])
medianaCzSpolkiMale <- median(spolkiMale[,3])
odchylenieCzSpolkiMale <- sqrt(sum((spolkiMale[,3] - sredniaCzSpolkiMale)^2)/nrow(spolkiMale))

sredniaCzSpolkiSrednie <- mean(spolkiSrednie[,3])
medianaCzSpolkiSrednie <- median(spolkiSrednie[,3])
odchylenieCzSpolkiSrednie <- sqrt(sum((spolkiSrednie[,3] - sredniaCzSpolkiSrednie)^2)/nrow(spolkiSrednie))

sredniaCzSpolkiDuze <- mean(spolkiDuze[,3])
medianaCzSpolkiDuze <- median(spolkiDuze[,3])
odchylenieCzSpolkiDuze <- sqrt(sum((spolkiDuze[,3] - sredniaCzSpolkiDuze)^2)/nrow(spolkiDuze))