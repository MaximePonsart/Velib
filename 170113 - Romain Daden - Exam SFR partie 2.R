## Exam DS : 

library(data.table)
library(Hmisc)
library(FactoMineR)
library(leaps)
library(glmnet)
library(caret)
library(randomForest)

setwd("//telemaque/users/CEPE-S1-03/170113 - Certificat DS/Données")


# W : vitesse du vent (module)
# U : direction Nord / Sud
# V : est_Ouest
# P : pression
# T : température
# 
# U² + v² = W² (théorique)





## II) Data, Machine learning

    don <- fread("donneesSFR.txt") ## 8576 obs, 176 vars
    names(don) <- tolower(names(don))
    
    tmp <- data.table(var = names(don))
    tmp <- tmp[order(var)]
    setcolorder(don,tmp[,var])


## explo rapide  ______________
# sapply(don,class) ## uniquement du numeric

# summary(don)

# t(don[,lapply(.SD,function(x){sum(is.na(x))})]) ## une valeur manquante sur t_1339 et t_1420 et p_1378 --> on les vire
don <- na.omit(don)


# unique(substr(names(don),1,2)) ## les types de variables

## explication de la démarche : 

  ## problématique de régression : on tente de prédire une valeur par d'autres (logique supervisée)
  ## explo rapide avec une ACP, Y projetée en supplémentaire
  ## voir les corr entre variables, les individus à l'ouest du far ouest : noter ces individus pour pouvoir les virer ensuite
  ## reg simple + analyse des résidus vs X et Y pour trouver des intérêts à ajouter des transformations
  ## reg sélection de variables en mode step, AIC ou BIC
  ## reg elastic net, ridge et lasso pour comparo
  ## arbre et fôret

## ACP pour y voir plus clair    
    acp_don <- PCA(don, scale.unit = T, ncp = 50, quanti.sup = 176)
    plot(acp_don, axes = c(1,2), choix = "var",col.quanti.sup = "red") 
    ## sur les deux premiers axes (50% de l'info) : (Y correlée positivement avec les variables de vent, corrélée négativement avec les variables de pression mais non corrélée avec les variables de température    
    ## p_2162 : sort du lot vs les autres pressions
    ## w_131, v_942, w_437, w_268, w_1115, w_2162, u_131,w_320, v_121, v_437, v_320 : sortent du lot également vs les autres w v et u
    ## W, v et u corrélés
    plot(acp_don, axes = c(1,3), choix = "var",col.quanti.sup = "red")
    plot(acp_don, axes = c(1,4), choix = "var",col.quanti.sup = "red")


## on attaque la modélisation
# split apprentissage / test, pour pouvoir calculer les mse après coup et comparer les modèles. Dans l'idée, il faudra boucler sur différents splits apprentissage / validation pour ne pas souffrir d'un découpage non représentatif
    set.seed(465)
    tmp <- don[,sample(1:.N, .N*.8)]
    don_train <- don[tmp]
    don_test <- don[-tmp]

## on lance une première régression pour voir
    lm1_don <- lm(y~.,data = don_train)
    # summary(lm1_don)    
    lm1_aic <- AIC(lm1_don)
    lm1_bic <- BIC(lm1_don) ## AIC et BIC de folie ! On va tenter de voir si on peut obtenir mieux avec moins de variables

## une deuxième sans les variables de température pour voir (on verra plus tard que cela dégrade la qualité de prédiction)
    tmp_var_t <- names(don)[grep("t_",names(don))]
    tmp_var_lm2 <- setdiff(names(don),c(tmp_var_t,"y"))
    lm2_formule <- paste0("y~",paste(tmp_var_lm2,collapse = "+"))
    
    lm2_don <- lm(formula(lm2_formule),data = don_train)
    # summary(lm1_don)    
    lm2_aic <- AIC(lm2_don)
    lm2_bic <- BIC(lm2_don)

## comparaison des prédictions
    tmp <- copy(don_test)
    tmp[,':='(predict_lm1 = predict(lm1_don,tmp)
                   ,predict_lm2 = predict(lm2_don,tmp))]
    tmp[,':='(error_sq_lm1 = (y - predict_lm1)^2,
                   error_sq_lm2 = (y - predict_lm2)^2)]
    mse <- tmp[,.(lm1_mse = mean(error_sq_lm1),
                       lm2_mse = mean(error_sq_lm2))]
    mse_lm1 <- mse[,lm1_mse]
    mse_lm2 <- mse[,lm2_mse]
    ## --> sans les variables températures, ça marche quand même moins bien --> marche arrière

    
## avec un step : aurait été bien pratique, mais prend trop de temps compte tenu des délais --> on va plutôt utiliser les propriétés de la régression lasso pour voir quelles variables conserver
# tmp <- step(lm1_mod)
# lm3_don <- step(lm1_don, k = log(nrow(don_train))) ## pour avoir le BIC au lieu du AIC et être plus pénalisant sur le nb de variables à considérer


## on va plutôt tenter une regression lasso pour voir quels coefficients s'annulent en premiers grâce à la norme l1

  tmp_cv <- cv.glmnet(x=as.matrix(don_train[,-176,with = F]),y=as.matrix(don_train[,176,with = F]),
                       family="gaussian",alpha=1,lambda=seq(0,10,length=100))
  lasso <- predict(tmp_cv,as.matrix(don_test[,-176,with = F]),s="lambda.min")
  err_lasso <- (prev - as.matrix(don_test[,176,with = F]))^2
  mse_lasso = mean(err_lasso)
  
## on test également la forêt à ajouter au comparatif des modèles
  
  rf <- randomForest(y~., data = don_train,ntree = 100, importance = T)
  prev_rf <- predict(rf,don_test)
  err_rf <- (prev_rf - as.matrix(don_test[,176,with = F]))^2
  mse_rf = mean(err_rf)  
  
## comparaison des mse : 
  tmp <- data.table(modèle = c("rf","lasso","lm1","lm2"), mse = c(mse_rf,mse_lasso,mse_lm1,mse_lm2))
  tmp
  
## effectivement, sans aucun travail sur la sélection des variables, la forêt aléatoire génère l'erreur de prévision la plus faible 
  ## --> regardons l'importance qu'elle donne aux différentes variables grâce à son système de permutation une à une post apprentissage 
  # (NB : ici, les variables sont fortement corrélées. De fait, l'importance des variables estimée par la forêt risque 
  #   d'être erronée. En effet, la permutation d'une variable très importante détériorera la qualité de la prédiction, mais dans une
  #   moindre mesure si son information est déjà portée par d'autres variables similaires. Or, on a vu avec l'ACP que les variables était fortement correlées.
  
  tmp <- rf$importance #les variables ayant le plus fort %IncMSE sont plus plus importantes, celles qui ont le plus accru le MSE une fois permutées
