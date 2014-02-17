# 
library(rpart)
library(randomForest)
#source("Perfopm10.R")
#source("Tabdeppm10.R")
#source("Fig_obspm10.R")
Data<-read.table("http://lmi.insa-rouen.fr/~portier/Data/Data_HRI.txt",header=TRUE,sep=";")
summary(Data)

# Elimination des donnees
pm10data = na.omit(Data)


# Echantillon d'apprentissage et de test
set.seed(111)
test.ratio = .25
npop = nrow(Data)
nvar = ncol(Data)
ntest = ceiling(npop*test.ratio)
testi = sample(1:npop, ntest)
appri = setdiff(1:npop, testi)
appr = Data[appri,]
test = Data[testi,]


# Construction de l’arbre maximal
modcart <- rpart(formula(pm10data),data = pm10data[appri,])
summary(modcart)
print(modcart)
plot(modcart, branch = 0.3, uniform = T)
text(modcart, digit = 4,col=2)
title("Modélisation des PM10")

#
