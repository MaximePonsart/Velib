
##########
##
##  on cherche à prédire une variable quantitative en fonction d'un nombre important de variable quanti
##  -> problématique de régression
##  On va tester différents modèles :
##    - Regression linéaire multiple -> permet d'avoir une premiere estimation rapidement
##    - Ridge -> si les variables sont très corrélées entre elles, les effets se combinent mais toutes les variables sont utilisées dans le modèle
##    - Lasso -> les effets peu importants sont mis à 0 donc selection de variables mais si des variables sont corrélées entre elles il en prend une aléatoirement en revanche
##    - Forets Aléatoires
##
########

setwd("~/Bureau/SDU")

#import des donnees
don <- read.csv("donneesSFR.txt",header=T,sep=" ")
# on retire les lignes avec des valeurs à NA
don <- na.omit(don)


# On réalise une ACP pour voir les correlations et diminuer le nombre de variables
library(FactoMineR)

res.PCA <- PCA(don,quanti.sup=176)

barplot(res.PCA$eig[,2])
round(res.PCA$eig[1:8,],2)

## 5 dimensions expliquent 88% de l'inertue

dimdesc(res.PCA)

## Modeles de regression 

library(glmnet)
library(randomForest)

set.seed(123)
#decoupage en echantillon apprentissage/test
ind <- sample(1:nrow(don),floor(.8*nrow(don)))
donA <- don[ind,]
donT <- don[-ind,]

#regression lineaire
reg_lm <- lm(Y~.,data=donA)
prev_lm <- predict(reg_lm,donT)
err_lm <- (prev_lm-donT$Y)^2

#ridge
ridgecv <- cv.glmnet(x=as.matrix(donA[,-176]),y=donA[,176],family="gaussian",alpha=0,lambda=seq(0,10,length=100))
prev_ridge <- predict(ridgecv,as.matrix(donT[,-176]),s="lambda.min")
err_ridge <- (prev_ridge - donT$Y)^2

#lasso
lassocv <- cv.glmnet(x=as.matrix(donA[,-176]),y=donA[,176],family="gaussian",alpha=1,lambda=seq(0,10,length=100))
prev_lasso <- predict(lassocv,as.matrix(donT[,-176]),s="lambda.min")
err_lasso <- (prev_lasso - donT$Y)^2

#foret
foret <- randomForest(Y~.,data=donA)
prev_foret <- predict(foret,donT)
err_foret <- (prev_foret - donT$Y)^2

## On compare les erreurs

boxplot(err_lm,err_ridge,err_lasso,err_foret)

## Les forets aléatoires sortent vainqueurs

# Les forets aléatoires sont une collection d'arbres construits de manière aléatoires sur un échantillon légèrement différent à chaque fois
# et avec seulement une partie des variables explicatives choisies aléatoirement. 


## On a travaillé sur toutes les données mais on aurait pu faire une selection de variable au prealable
## L'idée derrière est de diminuer la variance introduite par le nombre important de variable sans introduire trop de biais en omettant des variables explicatrices
## On peut passer par un algo step (forward ou backward) pour comparer les AIC de chaque modele en ajoutant ou retirant des variables si la machine tient la charge
## On aurait pu aussi supprimer les individus aberrants 



