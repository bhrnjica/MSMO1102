#-------------------UNIVEZITET U BIHAĆU-----------------------------------
#---------------Tehnički fakultet II Ciklus------------------------------- 
#MSMO1102      Matematičko modeliranje i inžinjerske metode
#--doc. dr. Bahrudin Hrnjica---

#Linearna višestruka regresija - uključuje zavisnost više ulaznih parametra (prediktora, feature, ) 
#                                 i jedne izlazne varijable (labela).
#                                   y= f(x1,x2,....) = b0+b1x1+b2x2+...., gdje su b0, b1 - koeficijenti regresije.

#Neka imamo neki skup podataka pri čemu imamo jedan ulazni parametar x, i jednu izlaznu varijablu y.

#Učitavanje podataka
data1 <- read.csv("data/podaci2.csv", header = T, col.names = c("x1","x2", "y"), sep = ",", dec = ".", strip.white = TRUE, stringsAsFactors = FALSE);

#strukture podataka
str(data1)

#prikaz podataka
head(data1)

#statistika
x1sr = mean(data1$x1);
x1sr;
x2sr = mean(data1$x2);
x2sr;
ysr = mean(data1$y);
ysr
#formiranje linearnog regresijskog modela u obliku y= b0 + b1x
#definisanje formule u R programskom jeziku
formula = y ~ x1+x2
print(formula)

#korelacijska matrica
c1 <- cor(data1)

#korelacijska matrica sa signifikantnim nivom
library("survival")
library("Formula")
library("ggplot2")
library("Hmisc")
c2 <- rcorr(as.matrix(data1))
c2
# prikaz korelacijskih koeficijenata
c2$r
# prikaz p-vrijednosti p testa
c2$P
#grafički prikaz korelacijske matrice (veæi krug i tamnija boja veća korelacija)
library(corrplot)
corrplot(c1, type = "upper", order = "hclust",  tl.col = "black", tl.srt = 45)

#linearni višestruki regresijski model 
lrmodel = lm(formula, data1)

#prikaz koeficijenata regresijskog modela
summary(lrmodel)

#korištenje modela za izraèunavanje vrijednosti podataka za testiranje odnosnoe predvidjanje vrijednosti
new <- data.frame(x1 = c(23, 22), x2 = c(3, 2))
new$y = predict(lrmodel, new)

#3D dijagrami
library(lattice)
cloud(formula, data = data1, pch = 16, # vrsta taèaka
               main="Grafièki prikaz y=f(x1,x2)")

library(scatterplot3d)
sp1 <- scatterplot3d(data1$x1, data1$x2, data1$y, pch = 16, highlight.3d = T)

#regresijska površina i podaci
sp1$plane3d(lrmodel, lty.box = "solid")

#3d Plot
library(rgl)
regg <- plot3d(data1$x1, data1$x2, data1$y, type="s", radius=0.2, col ="blue")

