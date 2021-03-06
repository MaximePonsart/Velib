rep <- file.path("//telemaque","Usergroupes","CEPE-FORMATION","Salle 1")
setwd(rep)
# import donn�es :
don <- read.csv2(file = file.path(rep,"donneesSFR.txt"),sep=" ", dec=".")
y <- don$Y
type_var <- sapply(X = don , FUN = class)
table(type_var)

summary(don)

## Nous n'avons que des variables num�riques
## nous sommes dans un probl�me de r�gression
## nous pourrions dans un premier temps mieux comprendre les variables en regardant leur distribution, le fait qu'il y ait des valeurs manquanes,
## il parait difficile vu le nombre de variables et de points de la grille de regarder toutes les corr�lations, afin de voir si certains points sont tr�s corr�l�s (ou tr�s proches)

## dans 1 premier temps, on peut prendre toutes les variables et regarder ce que cela donne, 
## �ventuellement avec des m�thodes de s�lection de variables (up ou down ?)
## et regarder si il y a des points abberants

## on pourrait �ventuellement, si on a le temps, regarder d'autres m�thodes (for�ts par exemple)

# v�rification qu'il n'y a pas de val manquantes
val_manq <- is.na(don)

table(val_manq )
# 3 manquants
don2 <- na.omit(don)

# je voulais voir corr�lation par la matrice :
ma_cor <- round(cor(don2[,-176]),2)
# mais je n'arrive pas � la faire parler


donred <- as.data.frame(scale(don2)) # ne sert pas : j'ai voule tester car le graphique des r�sidus m'a fait peur
## echantillonage
set.seed(2001)
ind <- sample(1:nrow(don2), floor(0.8*nrow(don2)))
donA <- don2[ind,]
donT <- don2[-ind,]

## mod�le simple pour voir
## au d�but, j'avais fait tourner sur toute la base. 
# quand je refais sur �chantillon, je n'obtiens pas les m�mes variables

reg_simple <- lm(Y~. , data = donA)
summary(reg_simple)
# bon R2 mais :

residu_reg1 <- rstudent(reg_simple)
plot(residu_reg1)
abline(h=c(-2,2), lty = 2)
# trop de r�sidus : on a trop de variables ?
# en fait pas trop : 8576*0.05 = 428 point en dehors max

# step trop long aujourd'hui :
step(reg_simple)


# on fait s�lection de variable avec LEAPS
library(leaps)
reg_choix_var_1 <- regsubsets(Y~. , data = donA, nbest = 1 , nvmax = 15 , method = "forward")
plot(reg_choix_var_1 , scale="bic")
var_sel <- reg_choix_var_1$xnames
var_sel <- reg_choix_var_1$vorder
var_select <- as.data.frame(cbind(reg_choix_var_1$xnames,reg_choix_var_1$vorder))
var_select$V2 <- as.numeric(var_select$V2)
var_choix <- var_select[var_select$V2<= 15 ,]
var_choix$V1
# on refait modele dessus :
reg_choix <- lm(Y~W_131+W_320+V_320+U_1295+P_1381+T_1463+U_1336+P_1253+P_1506+W_1255+T_1505+V_1462 , data = donA)
summary(reg_choix)

residu_reg2 <- rstudent(reg_choix)
plot(residu_reg2)
abline(h=c(-2,2), lty = 2)


## on part sur s�lection par LASSO :

library(caret)

controle <- trainControl(method = "cv", number = 5)

# LASSO, car cela permet de s�lectionner les variables
modele_lasso <- train(Y~. , data = donA , method = "lasso" , trControl = controle)

modele_lasso
varImp(modele_lasso)
# probl�me : que des variables W (vitesse vent)
# en m�me temps, c'est le plus important pour faire tourner une �olienne
# on aurait pu faire que sur cela (?)

# test sur For�t al�atoire pour voir importance des variables
#nombre de variables dans arbre (th�orique)
mtry_theo <- sqrt(ncol(donA)-1)
mtry_theo

rf_grille <- expand.grid(mtry = c(13,15))
modele_rf <- train(Y~. , data = donA , method = "rf" , ntree = 100 , tuneGrid = rf_grille , trControl = controle)

modele_rf
varImp(modele_rf) #bug, cela me renvoie la fonction � debugger
modele_rf$modelInfo$varImp()

# explication for�t
# d'abord un arbre de d�cision :
# le but d'un arbre est de s�parer la population en petits groupes qui auront une valeur moyenne de la variable � expliquer la + diff�rente possible
# cela se fair par �tape : on cherhce la variable qui permet de s�parer la pop totale en 2 groupes A et B o� mean(Y) dans le groupe A est le + �loign� possible de mean(Y) dans le groupe B. Les variables explicatives peuvent �tre continues ou factorielles. si elles sont continuent, l'algo cherche la meilleure fa�on de d�couper la variable
# et on recommence ce processus sur les 2 sous groupes A et B
# quand l'algo n'arrive plus � trouver une variables qui permet de bien diff�rencier l'un de sous-groupes des d�coupages pr�c�dents (qui sont en fin de branche), on s'arr�te
# les groupes finaux sont des feuilles, auxquelles ont attribuera la valeur (mean(Y))
# le bu de la pr�diction est de trouver la feuille dans laquelle tombe un individu, pour donner � cet individu la valeur de la feuille

# probl�me d'un arbre : il d�pend fortement des param�tres choisis (�chantillon, profondeur de l'arbre, ..). Un arbre trop profond risue de surapprendre
# du coup, on ne fait pas 1 arbre, mais plein. Bien s�r, on veut de arbres diff�rents (sinon �a ne sert � rien), donc chaque arbre n'aura qu'un nombre fini de varaibles � tester. La s�lection de l'�chantillon de variables pour un arbre est al�atoire. de m�me, on peut mettre dans chaque arbre un �chantillon al�atoir d'individus
# la pr�vision sera alors la moyenne des valeurs obtenus dans chaque arbre, donc la moyenne des valeurs des feuilles dans lequel l'individu tombe.


# pas le temps de tester sur donT pour voir meilleur mod�le