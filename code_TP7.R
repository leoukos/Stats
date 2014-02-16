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


par(mfrow=c(2,2))
hist(AGE)
hist(TAS)
hist(FC)

par(mfrow=c(2,2))
plot(AGE, STA)
plot(TAS, STA)
plot(FC, STA)

quantitatives = data.frame( AGE, TAS, FC)
cor(quantitatives)

# Regression (test)
starl <- glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial, trace=TRUE)
summary(starl)
anova(starl) # Contribution associées au variables

# Selection de variables
summary(glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial))$deviance
summary(glm(STA ~ IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial))$deviance
summary(glm(STA ~ IRC + INF + MCE + TYP + CRE + CS + AGE + TAS , family = binomial))$deviance
summary(glm(STA ~ IRC + MCE + TYP + CRE + CS + AGE + TAS , family = binomial))$deviance
summary(glm(STA ~ IRC + MCE + TYP + CS + AGE + TAS , family = binomial))$deviance
summary(glm(STA ~ IRC + TYP + CS + AGE + TAS , family = binomial))$deviance
summary(glm(STA ~ TYP + CS + AGE + TAS , family = binomial))$deviance
summary(glm(STA ~ TYP + CS + AGE , family = binomial))$deviance


# Comparaison de modèles
starl_gros <- glm(STA ~ SER + IRC + INF + MCE + TYP + CRE + CS + AGE + TAS + FC , family = binomial)
starl_petit <- glm(STA ~ TYP + CS + AGE , family = binomial)

stat_de_test = starl_petit$deviance - starl_gros$deviance
zone = 15,507 # pris dans la table du khi deux


# Etude des résidus
resp = residuals.glm(starl,type="pearson")
resd = residuals.glm(starl, type="deviance")

par(mfrow=c(1,2))
plot(resp, main="Residus de Pearson",xlab="")
abline(h=quantile(resp, 0.20), col="red")
abline(h=quantile(resp, 0.80), col="red")
plot(resd, main="Residus de deviance", xlab="")
abline(h=quantile(resd, 0.20), col="red")
abline(h=quantile(resd, 0.80), col="red")

# CS comme une variable qualitative
CS <- as.factor(CS)
summary(glm(STA ~ AGE + CAN + IRC + INF + TAS + CS_2, family = binomial))


# Modele medecin
starl_medecin <- glm(STA ~ AGE + CAN + IRC + INF + TAS + CS, family = binomial)

summary(glm(STA ~ AGE + CAN + IRC + INF + TAS + CS, family = binomial))
summary(glm(STA ~ AGE + CAN + IRC + TAS + CS, family = binomial))
summary(glm(STA ~ AGE + IRC + TAS + CS, family = binomial))
summary(glm(STA ~ AGE + TAS + CS, family = binomial))



starl_stat <- glm(STA ~ TYP + CS + AGE , family = binomial)

# deviance plus petite ou pas ~ % variance expliquée  // AIC


# Numéro du patient dans le modèle
starl_petit <- glm(STA ~ TYP + CS + AGE + ID , family = binomial)





