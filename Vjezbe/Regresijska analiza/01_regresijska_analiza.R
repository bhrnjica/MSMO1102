#-------------------UNIVEZITET U BIHACU-----------------------------------
#---------------Tehnicki fakultet II Ciklus------------------------------- 
#MSMO1102      Matematicko modeliranje i inžinjerske metode
#--doc. dr. Bahrudin Hrnjica---

#Linearna jednostruka regresija - ukljucuje zavisnost jednog ulaznog parametra (prediktora, feture, ) 
#                                 i jedne izlazne varijable (labela).
#                                   y= f(x) = b0+b1x, gdje su b0, b1 - koeficijenti regresije.

#Neka imamo neki skup podataka pri cemu imamo jedan ulazni parametar x, i jednu izlaznu varijablu y.

#Ucitavanje podataka
data1 <- read.csv("data/podaci1.csv", header = F, col.names = c("x", "y"), sep = ",", dec = ".", strip.white = TRUE, stringsAsFactors = FALSE);

#strukture podataka
str(data1)

#prikaz podataka
data1

#statistika
xsr = mean(data1$x);
ysr = mean(data1$y);

#formiranje linearnog regresijskog modela u obliku y= b0 + b1x
#definisanje formule u R programskom jeziku
formula = y ~ x

#linearni regresijski model 

lrmodel = lm(formula, data1)

#prikaz koeficijenata regresijskog modela
lrmodel
#plot(lrmodel)
#korištenje modela za izracunavanje vrijednosti podataka za testiranje odnosnoe predvidjanje vrijednosti
new <- data.frame(x = c(23, 2))
new$y = predict(lrmodel, new)

plot(data1)
#add regression line
lines(new$x, new$y, col="red")
