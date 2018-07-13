#-------------------UNIVEZITET U BIHACU-----------------------------------
#---------------Tehnicki fakultet II Ciklus------------------------------- 
#MSMO1102      Matematicko modeliranje i inžinjerske metode
#Nastavnik:     --doc. dr. Bahrudin Hrnjica---

#Prediktivno odrzavanje - razvijanje prediktivnih modela koje daju odgovor na pitanje: Koja ili kada ce sljedeca masina 
#                          ili komponenta proizvodnje imati zastoj.

# https://jamessdixon.wordpress.com/2014/05/06/kaplan-meier-survival-analysis-using-f/



#Neka imamo skup podataka koji predstavlja zivotni vijek (en. lifetime) mašine odnosno koliko dugo (u sedmicama) mašina radi do sada.
# ulazni parametri predstavljaju indeks za pritisak, vlagu i temparaturu.
#Ucitavanje podataka
data1 <- read.csv("data/pm_data.csv", header = T, sep = ",", dec = ".", strip.white = TRUE, skip = 1, stringsAsFactors = FALSE);

#strukture podataka
str(data1)

#konverzija defekt varijable u binarnu
data1$defekt = data1$defekt == 1

#konverzija kolona u kategoricne tipove
data1$provider = as.factor(data1$provider);
data1$team = as.factor(data1$team);


#brzi pregled podataka koje obradjujemo
summary(data1)
#nakon pokretanja gornje komande vidimo da su neke masine radile i do 93 sedmice, gotovo dvije godine. S druge strane 
#mozemo vidjeti da od ukupno 1000 masina, samo 40% (397) masina radi bez zastoja.

#detaljnije se moze pogledati na grafikonima
#prikaz zivotnog vijeka mašina 
# TRUE - ispravne masine imaju preko 60 sedmica vijek.
boxplot(vijek ~ defekt, data = data1, 
    main = "Životni vijeka mašina", xlab = "Stanje mašine", ylab = "Vijek mašine (sedmica)")


#U ovoj analizi koristit cemo analizu prezivljevanja (survival regression)
library(survival)
# Selekcija zavisnih varijabli u regresijskom modelu prezivljevanja.
dependantvars = Surv(data1$vijek, data1$defekt)

# Izgradnja modela (koristenje gaussove distrubicije)
survival_regression_model = survreg(dependantvars ~ pritisak + vlaga + temparatura + team + provider, dist = "gaussian", data = data1)

summary(survival_regression_model)

#graficki prikaz rezultata koristenjem paketa GGally
library(GGally)
#Caplan Meiet Curve
cm_curve <- survfit(Surv(vijek, defekt) ~ 1, data = data1)
ggsurv(cm_curve)

# sa dijagrama mozemo vidjeti da svaka masina ima 50 sedmica rada,a da nakon 50 sedmica imamo procente za zastoj masine.
# sada se postavlja pitanje kako da odredimo koja ce sljedeca masina imati zastoj za naredni period nrp. dvije sedmice.
# Kada rijesimo ovaj problem, zadatak matematickog modela prediktivnog odrzavanja se sastoji u tome da svake dvije sedmice 
# pozivamo model i odredjujemo koje masine ce imati zastoj u narednim sdmicama

# Odrediti predvidjanje zastoja masine pri vjerojatnosti od p=0.5
# 
ezastoj = predict(survival_regression_model, newdata = data1, type = "quantile", p = .5)

# konstrukcija data.frame za predvidjanje  
predvidjanje = data.frame(ezastoj)
predvidjanje$vijek = data1$vijek
predvidjanje$defekt = data1$defekt

# Izracunavanje preostalogVijeka trajanja masine
# vijek se izracuna na nacin da se od izracunatog zastoja oduzme tekuci vijek trajanja
predvidjanje$preostaliVijek = predvidjanje$ezastoj - predvidjanje$vijek

# Sortiranje podataka prema viejku trajanja masina
predvidjanje = predvidjanje[order(predvidjanje$preostaliVijek),]

# Zadrzati samo one masine koje nisu prekinule s radom
prioritetnaAkcija = predvidjanje[predvidjanje$defekt == 0,]
prioritetnaAkcijaDT <- head(prioritetnaAkcija, n = 20)
print(prioritetnaAkcijaDT)




prioritetnaAkcija$class <- cut(prioritetnaAkcija$preostaliVijek, c(-10, 1, 4, 1000))
levels(prioritetnaAkcija$class) <- c('Urgent', 'Medium', 'good')
summary(prioritetnaAkcija)
# iz posljedenje komande vidimo da imamo 7 masina koje se stati u tekucoj sedmici i koje trebaju urgentni servis
# za 21 masinu imamo dvije sedmica do servisiranja 

