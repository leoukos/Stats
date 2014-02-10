### Regression logistique
# Survie de patients en Unité de soins intensif
# On veut expliquer STA

# Variables quantitatives :
# AGE, TAS, FC
# Variables qualitatives :
# ID, SEX, RAC, SER, CAN, IRC, INF, MCE, ATC, TYP, FRA, PO2, PH, PCO, BIC, CRE, CS

# Données
data = read.csv2("http://lmi.insa-rouen.fr/~portier/Data/dataTPUSI.csv", header=TRUE, sep=";")
attach(data)


# Analyse des données
names(data)
summary(data)
plot(data)
cor(data)

# Test d'indépendance du Khi deux
qualitatives = data.frame(ID, SEX, RAC, SER, CAN, IRC, INF, MCE, ATC, TYP, FRA, PO2, PH, PCO, BIC, CRE, CS)
for(i in length(qualitatives)){
	tab = table(qualitatives[,i], STA)
	res = chisq.test(tab)$p-value
	cat(res, "\n")
}
