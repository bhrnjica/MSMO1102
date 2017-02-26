#-------------------UNIVEZITET U BIHACU-----------------------------------
#---------------Tehnicki fakultet II Ciklus------------------------------- 
#MSMO1102      Matematicko modeliranje i inžinjerske metode
#Nastavnik:     --doc. dr. Bahrudin Hrnjica---

#Prediktivno odrzavanje - razvijanje prediktivnih modela koje daju odgovor na pitanje: Koja ili kada ?e sljedeca masina 
#                          ili komponenta proizvodnje biti u defektu.

#Neka imamo skup podataka koji predstavlja zivotni vijek (en. lifetime) mašine odnosno koliko dugo (u sedmicama) mašina radi do sada.
# ulazni parametri predstavljaju indeks za pritisak, vlagu i temparaturu.
#Ucitavanje podataka
data1 <- read.csv("data/pm_data.csv", header = T, sep = ",", dec = ".", strip.white = TRUE, skip = 1, stringsAsFactors = FALSE);

#strukture podataka
str(data1)

#konverzija defekt varijable u binarnu
data1$defekt = data1$defekt == 1

#konverzija kolona u kategori?nne tipove
data1$provider = as.factor(data1$provider);
data1$team = as.factor(data1$team);

#prikaz zivotnog vijeka mašina 
# TRUE - pokvarene mašine imaju preko 60 sedmica vijek.
boxplot(vijek ~ defekt, data = data1, 
    #
    main = "Životni vijeka mašina", xlab = "Stanje mašine", ylab = "Vijek mašine (sedmica)")


formula = defekt ~ vijek + pritisak + vlaga + temparatura + team + provider;

model = glm(formula, data = data1, family = "gaussian")
summary(model)

