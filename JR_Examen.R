#On importe les données
don<-read.csv2(file="donneesSFR.txt",header=T,dec=".",sep="",skipNul = T)


summary(don)
#Au préalable on peut regarder la liaison entre variables quanti avec matrice de corrélation et test
cor(x=don)
#On peut visualiser les corrélations pour faire du choix de variables
library(corrplot)
corrplot.mixed(cor(x=don[,1:5])) #entre les éléments d'un même point 
corrplot.mixed(cor(x=don[,6:10])) #entre les éléments d'un même point
corrplot.mixed(cor(x=don[,11:15])) #entre les éléments d'un même point
corrplot.mixed(cor(x=don[,c(seq(from =1, to=171, by=5))])) #entre la même variable mais de 2 points différents
#on peut également aller plus loin en ayant directement les corrélations avec la significativité
library(Hmisc)
rcorr(as.matrix(don[,1:5])) #exemple
#on peut faire une ACP pour récupérer les axes principaux en entrée de nos modèles, variables centrées-résuites par défau
library(FactoMineR)
res.pca = PCA(don, quali.sup = 1, graph = F)
barplot(res.pca$eig[,2],names=paste("Dim",1:nrow(res.pca$eig))) #on peut garder 5 axes qui représentent 87% de l'information
#on aurait égalemen pu utiliser le stepAIC pour maximiser la vraisemblance mais trop coûtant en temps de calcul avec autant de variables


#On lance une GLM qui sera notre niveau de la mer
library(caret)


#Découper en Apprentissage/Test
set.seed(100)
Train<-createDataPartition(y = don$Y,
                           p = .8,
                           list = F
)
training<-data.frame(don[Train,])
testing<-data.frame(don[-Train,])


#TESTER GLM
glm<-glm(Y~.,data=training,family=gaussian)
print(summary(glm)) #on peut aussi faire le choix de variables avec les p-values (en théorie 1 par 1, ici on choisit par manque de temps les p-value très faibles)
glm<-glm(Y~.,data=training,family=gaussian) #on garde tout pour plus de simplicité dans le temps imparti
prediction_glm<-predict(glm,newdata=testing[,-176])
res_glm<-cbind(testing,prediction_glm)
res_glm<-res_glm[-1712,]
res_glm<-res_glm[-1711,] 
err_glm <- mean((res_glm$prediction_glm - res_glm$Y)^2) / mean(res_glm$Y)^2
err_glm     #14,7% d'erreurs   



#TESTER RANDOM FOREST
set.seed(7)
mtry_fixe <- sqrt(ncol(don) - 1)
rf.grid <- expand.grid(.mtry = mtry_fixe)
ctrl<-trainControl(method="cv",number=10)
rf<-train(Y~.,data=training,method="rf",ntree=50,trControl=ctrl,tuneGrid=rf.grid)
prediction_rf<-predict(rf,newdata=testing[,-176])
res_rf<-cbind(testing[-1712,],prediction_rf)
err_rf <- mean((res_rf$prediction_rf - res_rf$Y)^2) / mean(res_rf$Y)^2
err_rf     #4,8% d'erreurs  - meilleur modele

#pour les variables importantes 
varImp(rf)


#SVM  trop long ici
set.seed(1000)
svm<-train(Y~.,data=training,method="svmLinear",trControl=ctrl,prob.model = TRUE)
prediction<-predict(svm,newdata=testing)


#GBM  - test, pas le temps d'ajuster les parametres
set.seed(1001)
grid.gbm <- expand.grid(
  interaction.depth = c(1, 3, 5, 9),   # Profondeur des arbres
  n.trees = 10,                        # Nombre d'arbres
  # Donc le nombre d'itération puisque les GBM réalisent es estimations en série
  shrinkage = c(0.1),                      # Taux d'apprentissage 
  n.minobsinnode = 50)   
gbm<-train(Y~.,data=training,method="gbm",trControl=ctrl,tuneGrid=grid.gbm)
prediction<-predict(gbm,newdata=testing[,-176])
res_gbm<-cbind(testing[-1712,],prediction)
err_gbm <- mean((res_gbm$prediction - res_gbm$Y)^2) / mean(res_gbm$Y)^2
err_gbm     #26,4% d'erreurs 


#RIDGE/LASSO ( pas trop eu le temps
#L'idée est de trouver le lamba (critere de pénalisation) optimum qui permette d'avoir le meilleur modèle.
# Jouer sur alpha=0 ou 1 nous permet de faire une lasso et/ou ridge selon qu'on est en norme L1 ou L2
# Tout dépend si nous voulons mettre des coefficients à 0 ou non
set.seed(1003)
library(glmnet)
ridge<-glmnet(x=as.matrix(don[,-176]),y=don[,176],family="gaussian",alpha=0)
matplot(t(ridge$beta),type="l")
ridgecv<-cv.glmnet(x=as.matrix(don[,-176]),y=don[,176],family="gaussian",alpha=0,lambda=seq(0,1,length=100))
plot(ridgecv$lambda,ridgecv$cvm)
prediction<-predict(ridgecv,as.matrix(testing[,-176]),s="lambda.min")




