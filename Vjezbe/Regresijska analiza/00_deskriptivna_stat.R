#-------------------UNIVEZITET U BIHACU-----------------------------------
#---------------Tehnicki fakultet II Ciklus------------------------------- 
#MSMO1102      Matematicko modeliranje i inžinjerske metode
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


