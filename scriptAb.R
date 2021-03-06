library(caret)
library(glmnet)


don =  read.csv(file = "donneesSFR.txt",
                      sep = " ",
                      header = TRUE,
                      stringsAsFactors = FALSE
)
class(don)
don = na.omit(don)

#o vérifie qu'il n a pas de valeur manquante
na_count <-sapply(don, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
table(is.na(don$Y))

#il faut centrer et r�duire les donn�es 
#pour ne pas avoir d'impact des unit�s comme la pression qui a un niveau elev�
doncr<-scale(don)
doncr<-as.data.frame(doncr)
doncr$Y<-don$Y

#On d�coupe l'�chantillon pour �viter d'avoir un surapprentissage
ind <- sample(1:nrow(doncr),floor(.9*nrow(doncr)))
donA <- doncr[ind,]
donT <- doncr[-ind,]

#modele de base

#mod�le de regression
mco <- lm(Y~.,data=donA)
summary(mco)

mco <- lm(Y~.,data=donT)
prev <- predict(mco,donT)
errmco <- mean((prev - donT$Y)^2)

#autre methode avec le package CARET
# Contr�le de l'apprentissage : on opte pour une validation crois�e (cv) en 5 morceaux.
ctrl <- trainControl(method = "cv", number=5)
regression <- train(Y ~ ., 
                         data   = donA, 
                         method = "lm",trControl = ctrl)
reg.prev <- predict(regression, newdata = donT)
errreg <- (reg.prev - donT$Y)^2
mean(errreg)
varImp(regression)

#> mean(errreg)
#[1] 1640957


#les forets al�aoires avec Caret
mtry_fixe <- sqrt(ncol(don) - 1)
rf.grid <- expand.grid(.mtry = mtry_fixe)

rf_model <- train(Y ~ ., 
                  data = donA, 
                  method = "rf", ntree = 50, 
                  tuneGrid = rf.grid,
                  trControl = ctrl)
foret.prev <- predict(rf_model, newdata = donT)
varImp(rf_model)
errforet <- mean(foret.prev - donT$Y)^2
mean(errforet)

#> mean(errforet)
#[1] 413696.3
#je ne comprend pas ce n'est pas la bonne unit�, mais le taux d'erreur est moins important en la for�t que 
#pour la regression c'es le modele retenu.

 
###ridge
library(glmnet)
ridgecv <- cv.glmnet(x=as.matrix(doncr[,-176]),y=doncr[,176],
                     family="gaussian",alpha=0)
prev <- predict(ridgecv,as.matrix(donT[,-1]),s="lambda.min")
errridgecv <- mean((prev - donT$Y)^2)
#> errridgecv
#9.929324e+13

###lasso
ridgecv <- cv.glmnet(x=as.matrix(donA[,-1]),y=donA[,1],
                     family="gaussian",alpha=1)
prev <- predict(ridgecv,as.matrix(donT[,-1]),s="lambda.min")
errridlasso <- mean((prev - donT$Y)^2)

###elasticnet
ridgecv <- cv.glmnet(x=as.matrix(donA[,-1]),y=donA[,1],
                     family="gaussian",alpha=.5)
prev <- predict(ridgecv,as.matrix(donT[,-1]),s="lambda.min")
erreurelasticnet<-mean((prev - donT$Y)^2)


#choix des variables
     
mtry_fixe <- sqrt(ncol(don) - 1)
rf.grid <- expand.grid(.mtry = mtry_fixe)

rf_model <- train(Y ~ V_121 +P_437+ U_1508+ V_1508 +P_268 +W_131+U_131 +W_320+V_320+U_2162+V_2162 +P_2162  +W_1380 +P_1380+P_1338 +P_1379, 
                  data = donA, 
                  method = "rf", ntree = 50, 
                  tuneGrid = rf.grid,
                  trControl = ctrl)
foret.prev <- predict(rf_model, newdata = donT)
varImp(rf_model)
errforet <- mean(foret.prev - donT$Y)^2
mean(errforet)

#j'aurai du partir sur un choix de varibles d�s le d�part pour avoir un meilleur modele.