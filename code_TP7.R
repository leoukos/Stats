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
for(i in 1:length(qualitatives)){
	tab = table(qualitatives[,i], STA)
	test = chisq.test(tab)$p.value
	if(test > 0.05){
		#cat(names(qualitatives)[i], " : ", test, "\t indépendant \n")
	} else {
		cat(names(qualitatives)[i], " : ", test, "\t non indépendant \n")
	}
}
# Garder :
# SER, IRC, INF, MCE, TYP, CRE, CS 


par(mfrow=c(1,3))
hist(AGE)
hist(TAS)
hist(FC)

quantitatives = data.frame( AGE, TAS, FC)
cor(quantitatives)

# Regression
starl <- glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial, trace=TRUE)
summary(starl)
anova(starl)

# Etude des résidus
res = residuals.glm(starl,type="pearson")
residuals.glm(starl, type="deviance")




