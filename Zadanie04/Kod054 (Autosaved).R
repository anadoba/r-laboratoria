###########################################################
# Wyznaczenie warto�ci referencyjnych na bazie ROC = Receiver Operating Characteristic
###########################################################
#install.packages("pROC")
library(pROC)
# Funkcja w�asna w kt�rej obliczane s� Swoisto�� i Czu�o�� i przedzia�y ufno�ci
source('C:\\GKBibliotekiInf\\GKProgramyHelpWlasny\\GKR\\RFunkcje\\CzuloscSwoistosc.R')
setwd('/users/adam/Studia/r-laboratoria/Zadanie04')

Plik<-'Test54AdamNadoba 8.csv'
Dane <- read.csv2(Plik,header=T,sep=';')

DB <- data.frame(LVEF=LF,Incydent=In)
DB <- DB[sample(1:LiczbaPacjentow,LiczbaPacjentow),]

#----- Znalezienie zale�no�ci Swoisto�ci i Czu�o�ci
  jROC<-roc(Incydent~LVEF,data=Dane, percent=TRUE) 
#----- Znalezienie warto�ci progu i pozosta�ych
  jW<-coords(jROC, 'best', 
   ret=c('threshold', 'specificity', 'sensitivity', 'accuracy',
   'tn', 'tp', 'fn', 'fp', 'npv', 'ppv', '1-specificity',
   '1-sensitivity', '1-npv', '1-ppv'))
Pred <- t(as.table(round(jW[c(1:3,9:10)],2)))
colnames(Pred) <- c('Pr�g','Swoisto��','Czu�o��','Predykcja ujemna','Predykcja dodatnia')
Tab <- t(as.table(round(jW[c(5,7,8,6)],0)))
colnames(Tab) <- c('$n_{11}$','$n_{12}$','$n_{21}$','$n_{22}$')
PredCI<-as.table(CzuloscSwoistosc(jW[c(5,7,8,6)]))

#Czu�o�� = Sensitivity
#Swoisto�� = Specificity
#Predykcja dodatnia = ppv
#Predykcja ujemna = npv

###########################################################
#       |  Rzeczywistosc=N    |  Rzeczwistosv=T
#Test=N |Swoisto�� (CI)       |           X          
#       |predykcja ujemna (CI)|           X          
#Test=T |         X           |czu�o�� (CI)           
#       |         X           |predykcja dodatnia (CI)
###########################################################
#install.packages("cwhmisc")
library(cwhmisc)
#install.packages("Hmisc")
library(Hmisc)
source('C:\\GKBibliotekiInf\\GKProgramyHelpWlasny\\GKR\\RFunkcje\\Wydruk.R')

CzuloscSwoistosc<-function(x){
# x jest tablic� 2x2 jak wy�ej
# n11 n12 | n1
# n21 n22 | n2
  n11<-x[1];n12<-x[2];n21<-x[3];n22<-x[4]
CzuloscWynik <- paste(cwhmisc::formatFix((Czulosc<-100*as.numeric(binconf(n22,n12+n22,method='w')))[1],be=0,af=2),'%',sep='')
CzuloscCI <-cwhmisc::formatFix(Czulosc[2:3],be=2,af=2)
SwoistoscWynik <- paste(cwhmisc::formatFix((Swoistosc<-100*as.numeric(binconf(n11,n11+n21,method='w')))[1],be=0,af=2),'%',sep='')
SwoistoscCI <-cwhmisc::formatFix(Swoistosc[2:3],be=0,af=2)
PrDodatniaWynik <- paste(cwhmisc::formatFix((PrDodatnia<-100*as.numeric(binconf(n22,n21+n22,method='w')))[1],be=0,af=2),'%',sep='')
PrDodatniaCI <-cwhmisc::formatFix(PrDodatnia[2:3],be=0,af=2)
PrUjemnaWynik <- paste(cwhmisc::formatFix((PrUjemna<-100*as.numeric(binconf(n11,n11+n12,method='w')))[1],be=0,af=2),'%',sep='')
PrUjemnaCI <-cwhmisc::formatFix(PrUjemna[2:3],be=0,af=2)
Wynik <- rbind(
 paste(c('Czu�o��:',CzuloscWynik,' (',CzuloscCI[1],'%',' -',CzuloscCI[2],'%',')'),sep='',collapse=''),
 paste(c('Swoisto��:', SwoistoscWynik,' (',SwoistoscCI[1],'%',' -',SwoistoscCI[2],'%',')'),sep='',collapse=''),
 paste(c('Predykcja dodatnia:', PrDodatniaWynik,' (',PrDodatniaCI[1],'%',' -',PrDodatniaCI[2],'%',')'),sep='',collapse=''),
 paste(c('Predykcja ujemna:', PrUjemnaWynik,' (',PrUjemnaCI[1],'%',' -',PrUjemnaCI[2],'%',')'),sep='',collapse=''))
Wynik
}

CzuloscSwoistoscNum<-function(x){
## x jest wektorem czterech liczb
 ## Funkcje standardowe
  #n11 n12 | n1
  #n21 n22 | n2
  n11<-x[1];n12<-x[2];n21<-x[3];n22<-x[4]
CzuloscWynik <-   n22/(n12+n22)
SwoistoscWynik <- n11/(n11+n21)
PrDodatniaWynik <-n22/(n21+n22)
PrUjemnaWynik <-  n11/(n11+n12)
Wynik <- round(100*c(n22/(n12+n22),n11/(n11+n21),n22/(n21+n22),n11/(n11+n12)),2)
Wynik<-c(Wynik,c(n11,n12,n21,n22))
WynikTab<- as.table(Wynik)
names(WynikTab) <- c('Czu�o��','Swoisto��','Predykcja dodatnia','Predykcja ujemna','n11','n12','n21','n22')
WynikTab
}

ZnakMniejszosci<-function(x){x1<-as.numeric(x);Wynik<-'=';if (x1<0.05) {Wynik<-'<'};Wynik}
FisherTest <- function(x){cwhmisc::formatFix(
   min(fisher.test(x, alt='l')$p.value,fisher.test(x, alt='g')$p.value),
   be=0,af=3)}

CzuloscSwoistoscTest222<-function(x){
 # x jest obiektem (macierz�) o wymiarach 2 x 2 x 2
 X1<-x[,,1]
 X2<-x[,,2]
Czulosc   <-matrix(c(X1[2,2],X1[1,2],X2[2,2],X2[1,2]),2,2)
Swoistosc <-matrix(c(X1[1,1],X1[2,1],X2[1,1],X2[2,1]),2,2)
PrDodatnia<-matrix(c(X1[2,2],X1[2,1],X2[2,2],X2[2,1]),2,2)
PrUjemna  <-matrix(c(X1[1,1],X1[1,2],X2[1,1],X2[1,2]),2,2)
#Czulosc   <-matrix(c(X1[2,2],X1[1,2]+X1[2,2],X2[2,2],X2[1,2]+X2[2,2]),2,2)
#Swoistosc <-matrix(c(X1[1,1],X1[1,1]+X1[2,1],X2[1,1],X2[1,1]+X2[2,1]),2,2)
#PrDodatnia<-matrix(c(X1[2,2],X1[2,1]+X1[2,2],X2[2,2],X2[2,1]+X2[2,2]),2,2)
#PrUjemna  <-matrix(c(X1[1,1],X1[1,1]+X1[1,2],X2[1,1],X2[1,1]+X2[1,2]),2,2)

Czulosc   <-FisherTest(Czulosc   )
Swoistosc <-FisherTest(Swoistosc )
PrDodatnia<-FisherTest(PrDodatnia)
PrUjemna  <-FisherTest(PrUjemna  )

Wynik<-Wydruk(c(
'Czu�o��:',           ' p',ZnakMniejszosci(Czulosc),Czulosc,'; ',
'Swoisto��:',         ' p',ZnakMniejszosci(Swoistosc) ,Swoistosc,';\n', 
'Predykcja dodatnia:',' p',ZnakMniejszosci(PrDodatnia),PrDodatnia,'; ',
'Predykcja ujemna:',  ' p',ZnakMniejszosci(PrUjemna)  ,PrUjemna,'; '))  
Wynik
}

CzuloscSwoistoscTest22<-function(x){
 # x jest wektorem o wymiarach 8 wsp�rz�dnych
 X1<-matrix(x[1:4],2,2)
 X2<-matrix(x[5:8],2,2)

Czulosc   <-matrix(c(X1[2,2],X1[1,2],X2[2,2],X2[1,2]),2,2)
Swoistosc <-matrix(c(X1[1,1],X1[2,1],X2[1,1],X2[2,1]),2,2)
PrDodatnia<-matrix(c(X1[2,2],X1[2,1],X2[2,2],X2[2,1]),2,2)
PrUjemna  <-matrix(c(X1[1,1],X1[1,2],X2[1,1],X2[1,2]),2,2)
Czulosc   <-FisherTest(Czulosc   )
Swoistosc <-FisherTest(Swoistosc )
PrDodatnia<-FisherTest(PrDodatnia)
PrUjemna  <-FisherTest(PrUjemna  )

Wynik<-Wydruk(c(
'Czu�o��:',           ' p',ZnakMniejszosci(Czulosc),Czulosc,'; ',
'Swoisto��:',         ' p',ZnakMniejszosci(Swoistosc) ,Swoistosc,';\n', 
'Predykcja dodatnia:',' p',ZnakMniejszosci(PrDodatnia),PrDodatnia,'; ',
'Predykcja ujemna:',  ' p',ZnakMniejszosci(PrUjemna)  ,PrUjemna,'; '))  
Wynik
}





stop('Zatrzymanie')
#----- DaneSurowe zapisanie
Student <- paste('Test54',"Adam","Nadoba", sep='')
NazwaPliku <- paste(Student,'.csv', sep='',collapse='')
write.table(DB, file=NazwaPliku, append=F, col.names=T, row.names=F, sep='; ')

#----- Pocz�tek problemu
TekstZadanie <- paste('\\bigskip \n\n {\\bf Zadanie', NumerZadania,'} 
Dane do zadania nale�y pobra� z oddzielnego pliku: \\cudz{',NazwaPliku,'}. 
Plik ten zawiera 
wyniki obserwacji frakcji LVEF i wyst�pienia \\cudz{incydentu} 
dla ',LiczbaPacjentow,' pacjent�w.
\n\n Celem w zadaniu jest podzia� pacjent�w na dwie grupy, mo�emy nazwa� je grup� o 
ni�szym i wy�szym stadium choroby. Wed�ug konsensusu (medical consensus), na 
og� warto�ci� uwa�an� za w�asciw� dla osoby zdrowej jest $\\m{LVEF} > 30\\%.$
W naszym przypadku s� to pacjenci chorzy i warto�� ta jako diagnostyczna do 
decyzji o preferencji do wszczepienia rozrusznika mo�e by� ni�sza.
\n\n Warto�� progow� (diagnostyczn�) znajdujemy w wyniku procedury sekwencyjnej:
\n\n Ka�da warto�� $\\m{LVEF} = n\\%$ dzieli pacjent�w na dwie grupy, jedn� 
gdy $\\m{LVEF} \\leq n\\%$ (bardziej chorzy) i drug� o 
gdy $\\m{LVEF} > n\\%.$
Dla ka�dego podzia�u znajdujemy warto�� swoisto�ci i ufno�ci.
Warto�� progowa (diagnostyczna), to jest taka warto�� dla kt�rej suma 
swoisto�ci i ufno�ci jest najwi�ksza. \n\n
W rozwi�zaniu zadania nale�y poda�
\\begin{enumerate}
\\item Wykres czu�o�ci(sensitivity) w zale�no�ci od swoisto�ci(specificity),
\\item Znale�� warto�� progow� (treshold),
\\item Dla znalezionej warto�ci progowej znale�� 
czu�o�� (sensitivity), swoisto��(specificity), 
predykcj� dodatni� (npv), predykcj� ujemn� (ppv),
\\item Znale�� 95\\% przedzia�y ufno�ci Wilsona dla 
czu�o�ci, swoisto�ci, predykcji dodatniej i predykcji ujemnej 
wyznaczonej przez wrto�� progow�.
\\end{enumerate}')

PredTabDruk <- print(xtable::xtable(Pred),floating=F, file='clipboard',include.rownames=F)
TabTabDruk <- print(xtable::xtable(Tab,digits=0),floating=F, file='clipboard',sanitize.text.function = function(x){x},include.rownames=F)
PredCITabDruk <- print(xtable::xtable(PredCI),floating=F, file='clipboard',include.colnames=F,include.rownames=F)
Odp = paste('\n\n \\bigskip ',PredTabDruk,'\n\n \\bigskip ',TabTabDruk,
 '\n\n \\bigskip ',PredCITabDruk,collapse='')
cat(TekstZadanie)

#----------------------------------------------------------
#----- Odpowied� do problemu
OdpProblem <- Odp
#----------------------------------------------------------
#----- Koniec problemu
