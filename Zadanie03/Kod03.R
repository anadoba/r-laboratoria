#Adam Nadoba
#Test 08
#Zestaw 8

set.seed(123)

wzrostCh <- rnorm(100, 173, 5)
wzrostDz <- rnorm(110, 169, 5)
boxplot(wzrostCh, wzrostDz)
sredniaCh <- mean(wzrostCh)
sredniaDz <- mean(wzrostDz)
wariancjaCh <- var(wzrostCh)
wariancjaDz <- var(wzrostDz)

(sredniaCh - sredniaDz) / (wariancjaCh + wariancjaDz)^0.5

# hipoteze mozna odrzucic
# albo mozna nie odrzucic
# NIE MOZNA 'PRZYJAC'!

n=100
p=1/2
sample(0:1,100,prob=c(1/2,1/2),rep=T)

k <- 0:100
prawdo <- choose(100,k)/2^100
sum(prawdo)

plot(prawdo)


# kumulacyjne prawdopodobienstwa

cumsum(prawdo)
round(cumsum(prawdo), digits = 4)

# 1. sformulowanie hipotezy w mowie potocznej
# 2. modelowanie
# 3. ustalenie parametrow
# 4. postawienie hipotez
# 5. ustalenie wartosci krytycznej i zbioru krytycznego
# 6. decyzja (odrzucamy lub nie odrzucamy)


# wlasciwe zadanka

# PRZYJMUJEMY

# W ~ N(mi, sigma^2)
# H0 : mi = 0
# H1: mi != 0

# patrzymy na t_n-1
# obszar krytyczny (obszar odrzucenia)
# |t_n-1| > x_a   =>> odrzucamy
# |t_n-1| <= x_A  =>> nie odrzucamy

# x_a (x ze znaczkiem alfa) jest to wartosc krytyczna



W <- round(rnorm(136,0.01,1),2)
# obliczanie 't_n-1'
(mean(W)/sd(W))*(length(W))^0.5

# jak liczb bylo 29 to biore n-1=28 df 28 -> tdf
# jak mi wyjdzie powyzej wieksza od 2.045 to hipoteza odrzucona(?)
t.test(W,mu=0,alt="two.sided")
# powyzszym poleceniem mamy policzone to 't_n-1', df - liczba sotpni swobody (o jeden mniej)
# p-value - wartosc krytyczna prawdopodobienstwa bledu

# jak jest powyzej 0.05 - decyzja: 'nie ma podstaw do odrzucenia'





# robimy konkretne zadanka
x <- c(-0.53,0.37,0.82,0.07,0.48,-1.31,-1.62,-0.62,0.10,-1.54,0.61,-0.39,-1.97,0.94,-0.92,0.49,0.04,0.85,0.68,0.41,0.10,-0.74,0.62,0.52,0.23)

mean(x)

sd(x)

# t_n-1
(mean(x)/sd(x))*(length(x))^0.5

#1 - nie ma podstaw do odrzucenia
t.test(x,mu=0,alt="two.sided")
#2 - nie ma podstaw do odrzucenia
t.test(x,mu=0,alt="greater")
#3 - nie ma podstaw do odrzucenia
t.test(x,mu=0,alt="less")
