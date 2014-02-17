# TP sur les arbres de régression

#  imports
library(rpart)
library(randomForest)

source("Perfopm10.R")
source("Tabdeppm10.R")
source("Fig_obspm10.R")

Data<-read.table("http://lmi.insa-rouen.fr/~portier/Data/Data_HRI.txt",header=TRUE,sep=";")
summary(Data)

# Elimination des donnees
pm10data = na.omit(Data)
attach(pm10data)

# plot(pm10data)
# certaines données semblent être corrélées
# cor(pm10data)

# Echantillon d'apprentissage et de test
set.seed(111)
test.ratio = .25
npop = nrow(pm10data)
nvar = ncol(pm10data)
ntest = ceiling(npop*test.ratio)
testi = sample(1:npop, ntest)
appri = setdiff(1:npop, testi)
appr = pm10data[appri,]
test = pm10data[testi,]

# Construction de l’arbre maximal
modcart <- rpart(formula(pm10data),data = pm10data[appri,])
summary(modcart)
print(modcart)
plot(modcart, branch = 0.3, uniform = T)
text(modcart, digit = 4,col=2)
title("Modélisation des PM10")

#On peut constater que l'on obtiend un arbre de régréssion symétrique dans
#ce cas la mais ce ne sera bien évidament pas toujours le cas. La racine de
#l'arbre est la variable étant SO2 on trie notre échantillon à partir de la
#valeur de SO2 de 27,5.
#Une fois cette étape éffectuée, la partie de l'échantillon correspondant
#au sous arbre gauche sera elle meme scindée en deux sous partie suivant si
#la valeur de la variable SO2 est supérieur ou strictement inférieur à
#10,5.
#On répète le meme procédé pour les différents sous arbres avec d'autres
#variables tel que T.max ou encore HR.min.
#L'arbre de régression CART maximal est ainsi obtenu en ayant des feuilles
#comme par exemple notre première feuille qui correspond aux données de
#l'échantillon dont la valeur de la variable SO2 est strictement inférieur
#à 10,5, la valeur de T.max est inférieur à 22,55 ,
#la valeur moyenne de PM10 de ce nouvel échantillon (de cette feuille) est
#de 17,15.

# Peformances en estimation
pm10est= predict(modcart)
#plot(appr$PM10, pm10est)
Perfopm10(appr$PM10,pm10est)
TabDeppm10(appr$PM10,pm10est,30,50,30)
Titre = paste("Station HRI - Arbre maximal","Echantillon d'apprentissage", sep="\n")
Fig_obspm10(appr$PM10,pm10est,Titre,"Essai")

# et en prévision
pm10prev = predict(modcart, test)
plot(test$PM10, pm10prev)
Perfopm10(test$PM10,pm10prev)
TabDeppm10(test$PM10,pm10prev,30,50,30)
Titre = paste("Station HRI - Arbre maximal","Echantillon d'apprentissage", sep="\n")
Fig_obspm10(test$PM10,pm10prev,Titre,"Essai")

# Elagage
printcp(modcart)
plotcp(modcart)
modcartpr <- prune(modcart, cp = 0.023)
plot(modcartpr, branch = 0.3, uniform = T)
text(modcartpr, digit = 5)

# Forêts aléatoires
modrf <- randomForest(PM10 ~ NO + NO2 + SO2 + T.moy + VV.moy + PL.som + PA.moy + GTrouen + DV.dom, importance=TRUE)
impvar = c("NO","NO2","SO2","PL.som","T.moy","DV.dom","VV.moy","PA.moy","GTrouen")
op <- par(mfrow=c(3, 3))
for (i in seq_along(impvar)) {
  partialPlot(modrf, pm10data, impvar[i], xlab=impvar[i],main=paste("Effect de", impvar[i]),lwd=1.8,cex.lab=1.6,cex.main=1.6,,cex.axis=1.2)
}
par(op)

pm10est= predict(modrf)
#plot(appr$PM10, pm10est)
Perfopm10(appr$PM10,pm10est)
TabDeppm10(appr$PM10,pm10est,30,50,30)
Titre = paste("Station HRI - Arbre maximal","Echantillon d'apprentissage", sep="\n")
Fig_obspm10(appr$PM10,pm10est,Titre,"Essai")

pm10prev = predict(modrf, test)
plot(test$PM10, pm10prev)
Perfopm10(test$PM10,pm10prev)
TabDeppm10(test$PM10,pm10prev,30,50,30)
Titre = paste("Station HRI - Arbre maximal","Echantillon d'apprentissage", sep="\n")
Fig_obspm10(test$PM10,pm10prev,Titre,"Essai")



