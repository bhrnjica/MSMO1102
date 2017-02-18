#-------------------UNIVEZITET U BIHAÆU-----------------------------------
#---------------Tehnièki fakultet II Ciklus------------------------------- 
#MSMO1102      Matematièko modeliranje i inžinjerske metode
#--doc. dr. Bahrudin Hrnjica---

#Linearna po dijelovima regresija - predstavlja dvije ili više regresija na jednom data setu.
#                                   y= f(x1,x2,....) = b10+b11x1+b12x2+...., za x1,x2,> a
#                                   y= f(x1,x2,....)  = b20+b21x1+b22x2+...., za x1,x2,<= a 
#                                          gdje su b0, b1 - koeficijenti regresije.

#Neka imamo neki skup podataka pri èemu imamo jedan ulazni parametar x, i jednu izlaznu varijablu y.

#Uèitavanje podataka
data1 <- read.table("data/podace3.txt", header = T, dec = ".", strip.white = TRUE, stringsAsFactors = FALSE);

#strukture podataka
str(data1)

#prikaz podataka
data1

#statistika
x1sr = mean(data1$x);
ysr = mean(data1$y);

##grafièki prikaz i odreðivanje taèke prekida i prelaska jedna na drugu regresijski pravu
plot(data1)
#vidimo da za x=12 imamo prelaz, pa ce model po dijelovima imati sljedeci oblik
formula <- y ~ x * (x < 12) + x * (x >= 12)
#linearni regresijski model 

piecewise = lm(formula, data1)
plot(piecewise)

#prikaz koeficijenata regresijskog modela
summary(piecewise)

#korištenje modela za izraèunavanje vrijednosti podataka za testiranje odnosnoe predvidjanje vrijednosti
new <- data.frame(x = c(5,11,12, 20))
new$y = predict(piecewise, new)

#iscrtavanje 
#plot(data1$x, data1$y, xlim = c(12, 20), pch = 16)
lines(new$x, new$y, col = "red")
lines(new$x, new$y, col = "red", xlim = c(0, 12))
lines(new$x, new$y, col = "red", xlim = c(12, 25))
abline(v = 12, lty = 3)

