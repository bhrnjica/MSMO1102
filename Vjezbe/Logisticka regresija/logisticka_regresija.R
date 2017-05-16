#-------------------UNIVEZITET U BIHAĆU-----------------------------------
#---------------Tehnički fakultet II Ciklus------------------------------- 
#MSMO1102      Matematičko modeliranje i inžinjerske metode
#--doc. dr. Bahrudin Hrnjica---

#Logisticka regresija - 

#------QA Tim Izvještaj o pregledu proizvoda - -----

#   Nad svakim proizvodom izvršen je skup testova koji odredjuju da
#   li je proizvod dovojno kvalitetan za distibuciju i isporuku.
#   Manager QA tima na osnovu testova odredjuje kvalitetu proizvoda.
#   Zadatak Loogisticke regresije jeste da se izgradi Matematicki model koji ce iz testova proizvoda davati vjerojatnocu
#   ispravnosti proizvoda, odnosno da je proizvod kvalitetan i spreman za isporuku.

#neophodne biblioteke. Ukoliko se ne mogu učitati, to znači da je potrebno pakete peuzeti i intalirati.
library(ggplot2) #za iscrtavanje visokokvalitetnih grafikona

#   The caret package(short for _C_lassification _A_nd _RE_gression _T_raining) is a set of functions that 
#   attempt to streamline the process for creating predictive models. The package contains tools for :
#       -data splitting
#       -pre - processing
#       -feature selection
#       -model tuning using resampling
#       -variable importance estimation
#as well as other functionality.
library(caret)

library(optimx)


#Učitavanje podataka radimo sa klasicnom csv metodom, pri čemu smo učitali 100 testova proizvoda
data1 = read.csv("data/log_reg_data.txt", header = T, sep = "\t", dec = ".", skip=7, strip.white = TRUE, stringsAsFactors = FALSE);

#prikaz nekoliko prvih testova
head(data1)

#pregled strukture podataka 
str(data1)

#konverzija output var u bool 
data1$out=data1$out==1

#iscrtavanje dijagrama i koristenja ggplot paketa
#konstrukcija vektora sa vrijednostima "red" i "blue" koje će oznacavati vrijednosti out varijable
cols <- c("F" = "red", "T" = "blue")
#iscrtavanje podataka 
ggplot(logData,
aes(x = t1, y = t2, color = factor(out))) +
                    geom_point(size = 4, shape = 19, alpha = 0.6) +
                    scale_colour_manual(values = cols,
                   labels = c("Neispravan", "Ispravan"),
                   name = "QA Analiza")

# Definicija logit funkcije
sigmoid = function(z)
    {
        1 / (1 + exp(-z))
    }

#Za velike vrijednosti z dobijamo vrijednosti 1,
sigmoid(10 ^ 10) #resultat = 1
#dok za vrijednosti bliske 0 dobijamo vrijednost -1. 
sigmoid(-10 ^ 10) #resultat = 0
#sigmoid ima vrijenost 0.5 za z=0
sigmoid(0)#resultat = 0.5


#Crtanje grafa sigmoid funkcije
#1. definisimo x vrijednosti 
x <- seq(-10, 10, by = 0.1)

#pozovimo klasicnu plot funkciju
plot(x, y = sigmoid(x), type = "l", col = "red", xlab = "", ylab = "", main = "sigmoid funkcija (logistička kriva)")
#dodajmo liniju 
abline(v = 0, lty = 2, col = "gray70")
abline(h = 0.5, lty = 2, col = "gray70")

#definisanje formule za modela lr
formula = out ~ t1 + t2

#odredjivanje modela logističke regresije pomocu glm, uz binomial parametar
lr_model = glm(formula, data = data1, family = "binomial")

#pregled logistickog modela
summary(lr_model)
cor(lr_model.matrix(fit)[, -1])

#izracunavanje i crtanje granice razdvajanja
slope <- coef(lr_model)[2] / (-coef(lr_model)[3])
intercept <- coef(lr_model)[1] / (-coef(lr_model)[3])

#iscrtavanje granice odlučivanja
ggplot(data1, aes(x = t1, y = t2, color = factor(out))) +
geom_point(size = 4, shape = 19, alpha = 0.6) +
scale_colour_manual(values = cols, labels = c("Neispravan", "Ispravan"), name = "QA Analiza") +
# crtanje linije
geom_abline(intercept = intercept, slope = slope, col = "purple", show.legend = TRUE)

#određivanje performance modela preko ROC krive, te testiranje modela
library(ROCR)
#definisimo skup za testiranje
t1 = c(77.92409, 87.42057, 51.54772, 75.34438, 49.07256, 69.82457, 87.50879)
t2 = c(52.06099, 61.37929, 69.43286, 51.04775, 41.09210, 32.57720, 65.28611)
out = c(1, 1, 0, 1, 0, 0, 1)
test <- data.frame(t1, t2, out);

p <- predict(lr_model, newdata = test, type = "response")
performance(pred, measure = "auc")
plot(prf)

#povrsina ispod krive
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
