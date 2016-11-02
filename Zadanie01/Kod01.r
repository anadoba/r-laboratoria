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
spolkiDuze <- filter(spolki, wielkoscQ3 < spolki[2] & spolki[2])

