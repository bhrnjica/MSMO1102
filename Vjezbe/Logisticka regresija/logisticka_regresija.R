#-------------------UNIVEZITET U BIHAĆU-----------------------------------
#---------------Tehnički fakultet II Ciklus------------------------------- 
#MSMO1102      Matematičko modeliranje i inžinjerske metode
#--doc. dr. Bahrudin Hrnjica---

#Logistièka regresija - 

#Neka imamo neki skup podataka pri čemu imamo nekoliko ulaznih parametara xi, i jednu izlaznu binarnu varijablu y.

#Učitavanje podataka (mtcars data)
data1 <- read.csv("data/mtcars.txt", header = T, sep = ",", dec = ".", skip=28, strip.white = TRUE, stringsAsFactors = FALSE);

data1
str(data1)
#odabrati ulatzni parameta udaljenosti 
disp <- data1$disp;
#izlazna varijabla binarna koja označava da li se radi o automatskom ili ručnom mjenjaču
gear <- data1$am;

#iscrtavanje dijagrama






# with(data, expression)
# example applying a t-test to a data frame mydata
data1$carb
with(data1, { data1$carb <- data1$carb + 2; print(data1$carb) ;})




require(graphics)
pairs(mtcars, main = "mtcars data")
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)
str(mtcars)