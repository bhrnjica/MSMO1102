#-------------------UNIVEZITET U BIHAĆU-----------------------------------
#---------------Tehnički fakultet II Ciklus------------------------------- 
#MSMO1102      Matematičko modeliranje i inžinjerske metode
#--doc. dr. Bahrudin Hrnjica---

#Logisticka regresija - 

#Neka imamo neki skup podataka pri čemu imamo nekoliko ulaznih parametara xi, i jednu izlaznu binarnu varijablu y.

#Učitavanje podataka (mtcars data)
data1 <- read.csv("data/log_reg_data.txt", header = T, sep = "\t", dec = ".", skip=6, strip.white = TRUE, stringsAsFactors = FALSE);

data1
str(data1)
#odabrati ulatzni parameta udaljenosti 
disp <- data1$disp;
#izlazna varijabla binarna koja označava da li se radi o automatskom ili ručnom mjenjaču
gear <- data1$am;

#iscrtavanje dijagrama i koristenja ggplot paketa
library(ggplot2)
library(data.table)
library(magrittr)
library(caret)
library(optimx)

cols <- c("0" = "red", "1" = "blue")

ggplot(data1, aes(x = t1, y = t2, color = factor(out))) +
                    geom_point(size = 4, shape = 19, alpha = 0.6) +
                    scale_colour_manual(values = cols, labels = c("Neispravan", "Ispravan"), name = "QA Analiza")

# Definicija logi funkcije
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
formula = out~t1+t2
#odredjivanje modela logističke regresije pomocu glm, uz binomial parametar
lr_model = glm(formula, data = data1, family = "binomial")


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
pr <- prediction(p, test$out)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#povrsina ispod krive
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc