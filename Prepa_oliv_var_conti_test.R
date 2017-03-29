# Olivier CHATAL -24.03.2017

#### PREPARATION A L EXAMEN ####

rm(list = ls()) ; cat("\14") ; graphics.off() ; (.packages())
sessionInfo()

setwd("F:/R/DataScientist/jj_fiches")
# setwd("F:/R/DataScientist/J4-5 - Regressions_lin_ridge_lasso")
# setwd("F:/R/DataScientist/J10-Reg_logi_spline_foret_aleatoire/TD")
getwd()


## --------------------------------------------------- ##
##  PARAMETRAGE 
    fic <- "test.txt"  # nom du fichier de données

## --------------------------------------------------- ##
  
    
## ------------------------------------------------ 
## Données
## ------------------------------------------------
  
  ## Analyse du fichier de donnée
  ## ------------------------------
     # file.info(fic) # info sur le fichier
     # file.edit(fic) # print le contenu dans une page : ici le sep = "", dec="." 
      file.show(fic) # ouvre le fichier
  
  ## importation du fichier
  ## ------------------------------
    # read.table : 
     args(read.table) # defaut : header = FALSE, sep = "", dec="."
      data <- read.table(file = fic, header = T, sep ="" ) # row.names= => possible a declarer si besoin
      str(data)
      head(data, 2) 
      dim(data) ; names(data)
      
      ncol(data)
      colnames(data)[176]
      
  ## Traitement des données manquantes 
  ## ------------------------------        
      data.source <- data # sauvegarde au cas ou
      data <- na.omit(data.source)
      # 1 obs de moins
      head(data)
      str(data)
      

  ## Observation des données
  ## ------------------------------         
     # library(Hmisc) 
     # rcorr(as.matrix(data[,1:ncol(data)])) # attention au variables choisies
     # (.packages())
     # detach(package:Formula) ; detach(package:survival) ; detach(package:lattice) 
     #  

## ------------------------------------------------ 
## Echantillonnage
## ------------------------------------------------
 # decoupage en echantillon apprentissage/test

  ## Partitionnement du fichier (si cross-section)
  ## ------------------------------  
      # ??? => set.seed(380) # c'est pour retrouver tout le temps le meme echantillon?
      
   # => attention a la selection des variables : bien isolé Y !
      TE <- 2/3 # Taille de l'Echantillon : Split data into train (2/3) and test (1/3) sets
    # si cross-section (des fois que ce soit trié)
      train_rows <- sample(1:nrow(data) , floor(TE*nrow(data) ))
    # si time series (ou pas besoin de sample)
    # train_rows <- 1:floor(TE*nrow(data))
      
      data.train <- data[train_rows, ]  # echantillon d'apprentissage (training)
      data.test  <- data[-train_rows, ] # echantillon de test 
      
      # pour les var. explicatives : matrices # => attention a la selection dex X !!!
        x.train <- as.matrix(data.train[-ncol(data)])
        x.test  <- as.matrix(data.test[-ncol(data)])
      
       # Pour la variable a expliquer : matrices # => attention a la selection de Y !!!
         y.train <- as.matrix(data.train[ncol(data)])
         y.test  <- as.matrix(data.test[ncol(data)])
       
      # la version matricielle est surtout pour glm
      # pour randomForest, on peut garder en data frame
        
      # # pour les var. explicatives : matrices # => attention a la selection dex X !!!
      #   x.train <- as.matrix(data.train[,-58])
      #   x.test  <- as.matrix(data.test[,-58])
      #   
      # # Pour la variable a expliquer : matrices # => attention a la selection de Y !!!
      #   y.train <- as.matrix(data.train[,58])
      #   y.test  <- as.matrix(data.test[,58])
        
        

  ## ------------------------------------------------ 
  ## regression MCO avec selection des variables
  ## ------------------------------------------------
      # reg multiple sur toutes les variables
      lm.all <- lm(data=data.train, Y ~.) ; lm.all
      summary(lm.all)
      
      # au choix : attention changer le nom de Y !!!
         train <- cbind.data.frame(x.train, y.train)  # ici!
         test  <- cbind.data.frame(x.test,  y.test)   # ici!
          colnames(train)
          colnames(test)
      # # ou : nom de variables a changer !!!   
      #   # train
      #     Espvie.train <- data.train$Espvie
      #     train <- cbind(data.train[,-1],Espvie.train)
      #   # test
      #     Espvie.test <- data.test$Espvie
      #     test  <- cbind(data.test[,-1],Espvie.test)
         
    library(bestglm)
         
      selBIC <- bestglm(train,family=gaussian,IC="BIC",method="backward")
      selBIC$BestModel
      
      # valeurs prédite : Y chapeau  
        ?predict.lm
        yhat.lm.bic <- predict(selBIC$BestModel, newdata = data.test)
        # cbind(yhat.lm.bic,yhat.lm.bic2)
        str(yhat.lm.bic)
        
      # calcul du MSE   
        mse <- mean((data.test[ncol(data)] - yhat.lm.bic)^2); mse
        DF.bic <- as.data.frame(mse, row.names = "mseBIC")

        
  ## ------------------------------------------------   
  ## regression penalisee : ridge, lasso, elasticnet
  ## ------------------------------------------------
    # choix du parametre de regularisation (lamdba) par validation croisée
    #     => option de la fonction qui pourrait etre a redefinir : type.measure = "mse"
    #                                                              family = "gaussian"
    # --------------------
    library(glmnet)    
        
    # regressions  
      reg.ridge <- cv.glmnet(x.train,y.train,alpha = 0)   # performe une reg. ridge : Ridge in general is good at prediction, but is not very interpretable.
      reg.lasso <- cv.glmnet(x.train,y.train,alpha = 1)   # performe une reg. lasso : LASSO is good at picking up a small signal through lots of noise.
      reg.elast <- cv.glmnet(x.train,y.train,alpha = 0.5) # performe une reg. elasticnet pour tout 0<alpha<1
      # elasticnet => 0<alpha<1
      
    # valeurs prédite : Y chapeau  
      yhat.ridge <- predict(reg.ridge, s=reg.ridge$lambda.1se, newx=x.test)
      yhat.lasso <- predict(reg.lasso, s=reg.lasso$lambda.1se, newx=x.test)
      yhat.elast <- predict(reg.elast, s=reg.elast$lambda.1se, newx=x.test)
      
    # calcul du MSE  
      mse.ridge <- mean((y.test - yhat.ridge)^2); mse.ridge
      mse.lasso <- mean((y.test - yhat.lasso)^2); mse.lasso
      mse.elast <- mean((y.test - yhat.elast)^2); mse.elast
      
      coef(reg.lasso,s=reg.lasso$lambda.1se) # celui que l'on choisi!
      
    # --------------------
    # 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
      # autre solution : tout faire dans la boucle
      DF.pen <- data.frame(mse = 0)
      lst <- list()
      for (i in 0:10) {
        # regression pour 0<=alpha<=1
        assign(paste("reg.pen", i, sep=""), cv.glmnet(x.train, y.train, alpha=i/10 ) )
        
        # calcul des valeurs prédites : Y chapeau
        eval(parse(text=paste0("yhat",i,"<- predict(reg.pen",i,", s=reg.pen",i,"$lambda.1se, newx=x.test)") ))
        # calcul des MSE des regressions  
        eval(parse(text=paste0("mse",i, "<- mean((y.test - yhat",i,")^2)") ))
        # print des mse
        eval(parse(text=paste0("print(mse",i,")") )) 
        # enregistrement des MSE dans un DF  
        DF.pen[i+1,1]<-eval(parse(text=paste0("mse",i) ))
        lst <- c(lst,parse(text=sprintf("mse%s",i) ))
      }
      row.names(DF.pen)<-lst # stockage pour réutilisation futur
      # sauvegarde de secoure
      write.csv(DF.pen,file="DF.pen.csv")
      # nettoyage
      # rm(list=ls(pattern ="yhat"))
      # rm(list=ls(pattern ="mse"))
      # rm(list=ls(pattern ="reg.pen"))
      

  ## ------------------------------------------------
  ## machine learning : arbre, foret aleatoire, svm
  ## ------------------------------------------------
      
    ## Foret
    # ----------------------  
    library(randomForest)
      
      # calcul de la foret : # => Attention a l'ordre des matrices/DF des X et des Y (inverse de glmnet)
      
      # mod_RF <- randomForest(Y~., data=data.train, ntree=500) # comprend que l'on regresse Y sur le reste
      
        mod_RF <- randomForest(x=x.train, y=y.train, ntree=500) # si on met des matrices ou des data.frame
        mod_RF
      # mod_RF <- randomForest(x=data.train[,-58], y=data.train[,58], ntree=500)

      
      # valeurs prédite : Y chapeau  
        yhat.RF <- predict(mod_RF,newdata= data.test)
      
      # calcul du MSE  
        mse <- mean((y.test - yhat.RF)^2); mse
        DF.RF <- as.data.frame(mse,row.names="mseRF")

        
      
  ## ------------------------------------------------
  ## Selections du modèle a retenir
  ## ------------------------------------------------
      res <- rbind(DF.bic,DF.pen,DF.RF) ; res
        cat( "=========================================== \n",
               paste (" ==== >> ",row.names(res)[which.min(res$mse)],
                min(res), sep = " : " ),
             " \n",
             "========================================== \n")
        
  ## ------------------------------------------------ 
  ## regression logistic (si y = variable catégorielle)
  ## ------------------------------------------------    
     # => attention verifier que Y est bien un variable catégorielle
      data$Y <- as.factor(data$Y)
      
  # Logistic regression is another widely-used model when the response is categorical. 
  # If there are two possible outcomes, we use the binomial distribution, else we use the multinomial.
      
    # binomial distribution
      # ridge, lasso, elasticnet  
        fit = glmnet(x, y, family = "binomial")
        plot(fit, xvar = "dev", label = TRUE)
        predict(fit, newx = x[1:5,], type = "class", s = c(0.05, 0.01))
      
        # cross validation
        cvfit = cv.glmnet(x, y, family = "binomial", type.measure = "class")
        plot(cvfit)
        cvfit$lambda.min
        cvfit$lambda.1se
        coef(cvfit, s = "lambda.min")
        predict(cvfit, newx = x[1:10,], s = "lambda.min", type = "class")
      
 
    # multinomial distribution
      fit = glmnet(x, y, family = "multinomial", type.multinomial = "grouped")
      plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")
      
      # cross validation
      cvfit=cv.glmnet(x, y, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
      plot(cvfit)
      predict(cvfit, newx = x[1:10,], s = "lambda.min", type = "class")












