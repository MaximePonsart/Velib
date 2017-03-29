
# définir le Bureau ------------------------------------------------------------------

setwd("~/Bureau/Yasmina/Examens")


# Lire les données --------------------------------------------------------

#j'importe le fichier et je l'affecte à l'objet don
don <- read.table("donneesSFR.txt", header=T, dec=".")

summary(don)
sapply(don,class)
# les variables sont des variables quantitatives

#retirer les NA

don <- na.exclude(don)

# Methode -----------------------------------------------------------------

#Nous devons prédire une variable quantitative Y en fonction de 175 variables quantitatives.
#Nous allons donc faire une régression car il faut prédire "une valeur" et pas un label.


# Etape 1: ACP ------------------------------------------------------------

#Comme nous avons beaucoup de variables (difficile à visualiser avec pairs() ou avec un plot()),
#nous allons faire une ACP pour vérifier la corrélation entre les variables

library(FactoMineR)

#On utilise la fonction PCA() car les variables sont quantitatives
#Par défaut, la fonction va centrer-réduire les variables
#les unités de mesure étant différentes, nous devons centrer les variables car elles ont des variances très différentes
ACP_don <-PCA(don,graph =F)

#tableau un peu illisible mais donne une tendance des corrélations entre les variables
plot(ACP_don,choix="var")
ACP_don$var$cor# la pression a une corrélation négative avec la vitesse du vent


# Etape 2: régressio linéaire multiple ------------------------------------

#Je vais commencer par une régression linéaire multiple avec toutes les variables
mod <- lm(Y~.,data=don)
summary(mod)

#D'après le test de significativité, l'intercep(la constante) n'est pas égal à O (H0 est rejetée au profit de H1)

#Noms des variables avec une P value < 0.05
names(which(mod$coefficients<0.05))
length(names(which(mod$coefficients<0.05)))# 68 variables ont une p value < à 5%

# Mais la régression étant multiple et les variables explicatives non orthogonales, délicat d'utiliser les tests pour sélectionner les variables significatives
#Le test sur un coefficient revient à tester la significativité d'une variable alors que les autres variables sont dans le modèle
#Nous allons sélectionner les variables dans une autre étapes. Essayons de choisir une méthode de régression


# Regarder les résidus ----------------------------------------------------

#regardre les résidus studentisés: même variance
rs <- rstudent(mod)
plo(rs)
abline(h=c(-2,0,2),col=2)

# Choix de la méthode -----------------------------------------------------

library(glmnet)
library(ibr)
library(randomForest)
library(rpart)
library(mgcv)

#Nous allons comparer plusieurs méthodes de régression avec les paramètres de bases
which(colnames(don) == "Y")# le numéro de ligne pour la variable Y

iter=10
RES <- matrix(0,nrow=iter,ncol=6)
colnames(RES) <- c("MCO","RIDGE","LASSO","ELAS","ARBRE","Foret")

for(ii in 1:iter){
  set.seed(123+ii)#fixe la graine du générateur
  print(ii)
  ##Diviser les données en apprentissage et test: 90% en apprentissage et 10% en test (pas de règle)
  ind <- sample(1:nrow(don),floor(0.9*nrow(don)))
  donA <- don[ind,]
  donT <- don[-ind,]
  #modèle régression linéaire multiple (MCO)
  mod <- lm(Y~.,data = donA)
  pred <- predict(mod,donT)
  RES[ii,1] <- mean((pred-donT$Y)^2)
  #modèle ridge (pénalisation norme L2 des coefficients) avec validation croisée
  mod1 <- cv.glmnet(x=as.matrix(donA[,-176]),y=donA[,176],family="gaussian",alpha=0,lambda = seq(0,10,length=100))
  pred1 <- predict(mod1,newx=as.matrix(donT[,-176]),s="lambda.min")#je prends le plus petit lambda
  RES[ii,2] <- mean((pred1-donT$Y)^2)
  #modèle Lasso (pénalisation norme L1 des coefficients: ce modèle choisit les variables siginifactives mais gère pas bien les corrélations entre les variables) 
  mod3 <- cv.glmnet(x=as.matrix(donA[,-176]),y=donA[,176],family="gaussian",alpha=1,lambda = seq(0,10,length=100))
  pred3 <- predict(mod3,newx=as.matrix(donT[,-176]),s="lambda.min")#je prends le plus petit lambda
  RES[ii,3] <- mean((pred3-donT$Y)^2)
  #modèle Elasticnet (entre du Lasso et ridge)
  mod4 <- cv.glmnet(x=as.matrix(donA[,-176]),y=donA[,176],family="gaussian",alpha=0.5,lambda = seq(0,10,length=100))
  pred4 <- predict(mod4,newx=as.matrix(donT[,-176]),s="lambda.min")#je prends le plus petit lambda
  RES[ii,4] <- mean((pred4-donT$Y)^2)
  #modèle arbre
  mod5 <- rpart(Y~.,donA)
  pred5 <- predict(mod5,donT)
  RES[ii,5] <- mean((pred5-donT$Y)^2)
  #modèle forêt: avec els paramètres par défaut, n'est pas sensibles aux paramètres
  mod6 <- randomForest(Y~.,data=donA)
  pred6 <- predict(mod6,donT)
  RES[ii,6] <- mean((pred6-donT$Y)^2)
  
  }
boxplot(RES)#visualisation de l'erreur de prévision MSE

#L'erreur d'estimation par méthode
MSE <- apply(RES,2,abs)
#la valeur absolue de l'erreur
Abs <- abs(MSE)



# Etape 3: Choix des variables avec step() --------------------------------

#On va donc choisir les variables avec une procédure backward de la fonction step
mod_final <- step(mod,direction = "backward",data=don)# très lent en temps de calcul

#on va tester avec le package leaps
library(leaps)
choix <- regsubsets(y~.,data=don,nbest=1,nvmax=175,method="backward")
plot(choix)#on va regader le BIC: minimiser le BIC
resume_choix <- summary(choix)
var_expl <- names(don[,-176])
#choix des variables
best_bic <- order(resume_choix$bic)[1]#on choisit le meileur modèle
#les variables à selectionner
selection <- resume_choix$which[best_bic,-1]
#variable à garder
var_expl[selection]
var_conserver <- paste(var_expl[selection],"~",sep="",collapse="+")
formule <- paste(names(don[,176]),"~",var_conserver,sep"")
bstmod <- lm(formula(formule),data=don)



# Question 4: la foret aléatoire ------------------------------------------

#La forêt aléatoire est une collection de fôret qui 
#1)aggrège des arbres construits sur un échantillon bootstrap (tirage avec remise) et qui minimise la fonction de coût (CART régresseur)
#et qui choisit les "meilleurs variables" dans un ensemble composé de m variables choisies aléatoirement dans les d variables initiales
#Il s'agit d'une méthode d'aggrégation d'algorithmes prédictives par bagging
#L'avantage c'est la baisse de la corrélation entre les arbres que l'on a aggrégé car à chaque arbre, nous n'avons pas les mêmes données et les mêmes variables


