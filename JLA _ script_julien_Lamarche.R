rep <- file.path("//telemaque","Usergroupes","CEPE-FORMATION","Salle 1")
setwd(rep)
# import données :
don <- read.csv2(file = file.path(rep,"donneesSFR.txt"),sep=" ", dec=".")
y <- don$Y
type_var <- sapply(X = don , FUN = class)
table(type_var)

summary(don)

## Nous n'avons que des variables numériques
## nous sommes dans un problème de régression
## nous pourrions dans un premier temps mieux comprendre les variables en regardant leur distribution, le fait qu'il y ait des valeurs manquanes,
## il parait difficile vu le nombre de variables et de points de la grille de regarder toutes les corrélations, afin de voir si certains points sont très corrélés (ou très proches)

## dans 1 premier temps, on peut prendre toutes les variables et regarder ce que cela donne, 
## éventuellement avec des méthodes de sélection de variables (up ou down ?)
## et regarder si il y a des points abberants

## on pourrait éventuellement, si on a le temps, regarder d'autres méthodes (forêts par exemple)

# vérification qu'il n'y a pas de val manquantes
val_manq <- is.na(don)

table(val_manq )
# 3 manquants
don2 <- na.omit(don)

# je voulais voir corrélation par la matrice :
ma_cor <- round(cor(don2[,-176]),2)
# mais je n'arrive pas à la faire parler


donred <- as.data.frame(scale(don2)) # ne sert pas : j'ai voule tester car le graphique des résidus m'a fait peur
## echantillonage
set.seed(2001)
ind <- sample(1:nrow(don2), floor(0.8*nrow(don2)))
donA <- don2[ind,]
donT <- don2[-ind,]

## modèle simple pour voir
## au début, j'avais fait tourner sur toute la base. 
# quand je refais sur échantillon, je n'obtiens pas les mêmes variables

reg_simple <- lm(Y~. , data = donA)
summary(reg_simple)
# bon R2 mais :

residu_reg1 <- rstudent(reg_simple)
plot(residu_reg1)
abline(h=c(-2,2), lty = 2)
# trop de résidus : on a trop de variables ?
# en fait pas trop : 8576*0.05 = 428 point en dehors max

# step trop long aujourd'hui :
step(reg_simple)


# on fait sélection de variable avec LEAPS
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


## on part sur sélection par LASSO :

library(caret)

controle <- trainControl(method = "cv", number = 5)

# LASSO, car cela permet de sélectionner les variables
modele_lasso <- train(Y~. , data = donA , method = "lasso" , trControl = controle)

modele_lasso
varImp(modele_lasso)
# problème : que des variables W (vitesse vent)
# en même temps, c'est le plus important pour faire tourner une éolienne
# on aurait pu faire que sur cela (?)

# test sur Forêt aléatoire pour voir importance des variables
#nombre de variables dans arbre (théorique)
mtry_theo <- sqrt(ncol(donA)-1)
mtry_theo

rf_grille <- expand.grid(mtry = c(13,15))
modele_rf <- train(Y~. , data = donA , method = "rf" , ntree = 100 , tuneGrid = rf_grille , trControl = controle)

modele_rf
varImp(modele_rf) #bug, cela me renvoie la fonction à debugger
modele_rf$modelInfo$varImp()

# explication forêt
# d'abord un arbre de décision :
# le but d'un arbre est de séparer la population en petits groupes qui auront une valeur moyenne de la variable à expliquer la + différente possible
# cela se fair par étape : on cherhce la variable qui permet de séparer la pop totale en 2 groupes A et B où mean(Y) dans le groupe A est le + éloigné possible de mean(Y) dans le groupe B. Les variables explicatives peuvent être continues ou factorielles. si elles sont continuent, l'algo cherche la meilleure façon de découper la variable
# et on recommence ce processus sur les 2 sous groupes A et B
# quand l'algo n'arrive plus à trouver une variables qui permet de bien différencier l'un de sous-groupes des découpages précédents (qui sont en fin de branche), on s'arrète
# les groupes finaux sont des feuilles, auxquelles ont attribuera la valeur (mean(Y))
# le bu de la prédiction est de trouver la feuille dans laquelle tombe un individu, pour donner à cet individu la valeur de la feuille

# problème d'un arbre : il dépend fortement des paramètres choisis (échantillon, profondeur de l'arbre, ..). Un arbre trop profond risue de surapprendre
# du coup, on ne fait pas 1 arbre, mais plein. Bien sûr, on veut de arbres différents (sinon ça ne sert à rien), donc chaque arbre n'aura qu'un nombre fini de varaibles à tester. La sélection de l'échantillon de variables pour un arbre est aléatoire. de même, on peut mettre dans chaque arbre un échantillon aléatoir d'individus
# la prévision sera alors la moyenne des valeurs obtenus dans chaque arbre, donc la moyenne des valeurs des feuilles dans lequel l'individu tombe.


# pas le temps de tester sur donT pour voir meilleur modèle