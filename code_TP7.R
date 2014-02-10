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
		cat(names(qualitatives)[i], " : ", test, "\t dépendant \n")
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

# Regression (test)
starl <- glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial, trace=TRUE)
summary(starl)
anova(starl)

# Selection de variables
summary(glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial))
summary(glm(STA ~ IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial))
summary(glm(STA ~ IRC + INF + MCE + TYP + CRE + CS + AGE + TAS , family = binomial))
summary(glm(STA ~ IRC + MCE + TYP + CRE + CS + AGE + TAS , family = binomial))
summary(glm(STA ~ IRC + MCE + TYP + CS + AGE + TAS , family = binomial))
summary(glm(STA ~ IRC + TYP + CS + AGE + TAS , family = binomial))
summary(glm(STA ~ TYP + CS + AGE + TAS , family = binomial))
summary(glm(STA ~ TYP + CS + AGE , family = binomial))


# Comparaison de modèles
starl_gros <- glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial)
starl_petit <- glm(STA ~ TYP + CS + AGE , family = binomial)

stat_de_test = starl_petit$deviance - starl_gros$deviance
zone = 15,507


# Etude des résidus
resp = residuals.glm(starl,type="pearson")
resd = residuals.glm(starl, type="deviance")

# CS comme une variable qualitative
CS <- as.factor(CS)
summary(glm(STA ~ AGE + CAN + IRC + INF + TAS + CS, family = binomial))


