#-------------------UNIVEZITET U BIHACU-----------------------------------
#---------------Tehnicki fakultet II Ciklus------------------------------- 
#MSMO1102      Matematicko modeliranje i in?injerske metode
#--doc. dr. Bahrudin Hrnjica---

#Deskriptivna statistika- osnovne statisticke velicine u R

#Neka imamo neki skup podataka pri cemu imamo jedan ulazni parametar x, i jednu izlaznu varijablu y.

#Ucitavanje podataka
data1 <- c(11, 12, 14, 22, 27, 27, 31, 38, 42, 49, 54, 55, 56, 62, 64, 65, 70, 81, 83, 85, 92, 96);
#strukture podataka
str(data1)

#prikaz podataka
data1

#statistika
#aritmeticka sredina
a_sredina = mean(data1);

#medijan
a_medijan <- median(data1);
a_medijan


##Prvi put je potrebno pokrenuti ovu liniju koda
#install.packages("xlsx")
# load libraray
library(xlsx)     #excel manipulation



# read in the worksheet named mysheet
e_data <- read.xlsx("data/data_sample.xlsx", sheetName = "sample1") 


table(e_data$Pozicija)
summary(e_data$Pozicija)
str(e_data)
##
sd(e_data$Godiste)#standardna devijacija kolone Godiste (godište)

#broj uzoraka
length(e_data) #broj kolona
length(e_data$Zaposlenik) #broj kolona

hist(e_data$Plata)#histogram
boxplot(e_data$Plata)#kutijasti dijagram


##Kategorične kolone u tabeli

#broj muskaraca i žena
table(e_data$Pusac)

#procentualno broj mus i zen
table(e_data$Pusac)/length(e_data$Pusac)

#kontingentna tabela od dvije kolone
#pregled pušača muškaraca i zena
table(e_data$Spol, e_data$Pusac)

#numeričke kolone u tabeli
#aritmetička sredina
mean(e_data$Plata)

#aritmetička sredina sa 10% odbacivanjem najvećih i najmanjih vrijednosti
mean(e_data$Plata, trim = 0.10)

#izracunavanje mediana
median(e_data$Plata)

#izračunavanje varijance
var(e_data$Godiste)

#izračunavanje standardne devijacije
sd(e_data$Godiste)

#standardna devijacija kao korijen varijance
sqrt(var(e_data$Godiste))


#minimalna vrijednost kolone
min(e_data$Godiste)

#maksimalna vrijednost plate 
max(e_data$Plata)

#opseg varijable
range(e_data$Godiste)


#odredjivanje gornjeg, donjeg kvartila
quantile(e_data$Godiste, probs = 0.4)


#suma 
sum(e_data$Plata)

#mean kao suma/brojuzor
sum(e_data$Godiste)/length(e_data$Godiste)

#Pearsonova korelacija  Godiste i Plata
cor(e_data$Godiste,e_data$Plata)

#Spearmensova korelacija
cor(e_data$Godiste,e_data$Plata,method = "spearman")

#kovarijanca 
cov(e_data$Godiste, e_data$Plata)

#kovarijanca preko var naredbe
var(e_data$Godiste, e_data$Plata)


#većina gornjih vrijednosti se dobija sa summary
summary(e_data$Godiste)

#summary se može koristiti i za kategorične kolone
summary(e_data$Spol)

#summary je generalna komanda koja se primjenjuje na sve R objekte
summary(e_data)

#Grafika i dijagrami
#dijagram raspršenosti dvije kolone
plot(e_data$Godiste, e_data$Plata)
plot(e_data$Spol, e_data$Plata)

