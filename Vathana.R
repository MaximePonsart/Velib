library(data.table)
library(dplyr)
library(FactoMineR)
library(caret)
library(ROCR)
library(rio)


# IMPORT : read.table

don1 <- read.table(file = "donneesSFR.txt",
                        header = TRUE,
                        sep = "") 

# Methodologie employé : répondre à un problème de régression
summary(don1)
names(don1)
str(don1$Y)


# on enlève les valeurs manquantes
is.na(don1)# valeurs manquantes
don1<-na.omit(don1)

#===========================================================================================
# Avant de commencer : 
# Nous avons plus de 50 echantillons
# Nous devons  prédire une production d'energie 

# Pour cette problématique nous allons tenter de répondre à une problématique de régression:
# Les étapes :
# 1) Choix de variables
# 2) Nous allons choisir les variables qui caractériseront le mieux la production d'une station
# 3) nous allons allons centrer-réduire car les variables ont des unités différentes


#============================================================================================

#Centrage et réduction du jeu de données

don1<-scale(don1, center=T, scale=T)

res.pca<-PCA(don1)

# Choix du nbre d'axes

barplot(res.pca$eig[,2],names=paste("Dim",1:nrow(res.pca$eig)))

# Analyse des 5 premières dimension
round(res.pca$eig[1:5,],2) # les 2 premiers axes expliquent 54% de l'inertie

res.pca$eig[1:5,]

dimdesc(res.pca$eig[1:5,])


# SELECTION DE VARIABLE
# J'ai un trou de mémoire, je ne sais plus comment récupérer les variables de mes 4 dimensions 
varset<-c()

don3<-don[,varset]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#PREPARATION ECHANTILLON TEST DE TEST ET APPRENTISSAGE

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)

#Variables colineaire entres elles?

#=========================================
#REGRESSION LOGISTIQUE
#========================================
set.seed(100)
Train <- createDataPartition(y = don1$Y, p = 0.6, list = FALSE)
training <- don1[  Train, ]
testing  <- don1[ -Train, ]

modele_logistique <- train(Y ~ ., 
                         data   = training, 
                         method = "glm", family = "binomial")

print(summary(modele_logistique))
print(summary(modele_logistique)$coeff[-1,4] < 0.05)
varImp(modele_logistique)



# on sauvegarde 
saveRDS(modele_binomial, file = paste("outputs", "logistic_model_total.RDS", sep="_"))
modele_binomial = readRDS(paste("outputs", "logistic_model_total.RDS", sep="_"))

# Calcul des prévisions avec la probabilité
glm.testing$prediction_modele <- 
  predict(modele_logistique, 
          newdata = glm.testing, 
          type = "prob")[ , 2]

roc.glm = roc(TARGET_B ~ prediction_modele , data = glm.testing)
print(roc.glm$auc)

# Evaluation du model

prediction_logistique <- predict(modele_logistique, newdata = glm.testing)



#FORET
set.seed(100)
mtry_fixe <- sqrt(ncol(don3) - 1)
rf.grid <- expand.grid(.mtry = mtry_fixe)

rf_model <- train(Y~ ., 
                  data = training, 
                  method = "rf", ntree = 100, 
                  tuneGrid = rf.grid,
                  trControl = ctrl) 


