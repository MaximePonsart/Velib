# Christopher Burton-Page


# u:nord-sud
# w:vitesse du vent
# v:est-west 
# t:température
# p:pression
# y:prod d'un parc eolien
# prévoir la prod du parc en cas de nouvelles observations
p<-file.path("//","telemaque","users","CEPE-S1-01","Bureau","jeux de données","donneesSFR.txt")
don<-read.table(file.path("/","telemaque","users","CEPE-S1-01","Bureau","jeux de données","donneesSFR.txt") , header = TRUE , sep = " ")
# on peut modéliser simmplement chaque cellule météo avec la prod
# sur une table don_U
# on peut faire de la reduction de dimension avec une ACP pour voir quelle cellule joue le plus 
# ou quelle variable joue le plus sur la table don2

# exploration de la base
str(don)
length(don) # 176

# datamanagement : on a 5 variables x 35 cellules : on se ramène à 5 var + 1
cellules <- unique(  substr(colnames(don),3,nchar(colnames(don)) )  )
cellules<-cellules[-36]

don_P <- subset( don,select=grepl("P_",colnames(don)) )
colsP<-substr(colnames(don_P),3,nchar(colnames(don_P)) )
colnames(don_P) <- colsP
don_P <- cbind(don_P,don$Y)

don_T <- subset( don,select=grepl("T_",colnames(don)) )
colT<s-substr(colnames(don_T),3,nchar(colnames(don_T)) )
colnames(don_T) <- colsT
don_T <- cbind(don_T,don$Y)

don_U <- subset( don,select=grepl("U_",colnames(don)) )
colsU<-substr(colnames(don_U),3,nchar(colnames(don_U)) )
colnames(don_U) <- colsU
don_U <- cbind(don_U,don$Y)

don_V <- subset( don,select=grepl("V_",colnames(don)) )
colsV<-substr(colnames(don_V),3,nchar(colnames(don_V)) )
colnames(don_V) <- colsV
don_V <- cbind(don_V,don$Y)

don_W <- subset( don,select=grepl("W_",colnames(don)) )
colsW<-substr(colnames(don_W),3,nchar(colnames(don_W)) )
colnames(don_W) <- colsW
don_W <- cbind(don_W,don$Y)


library(tidyr)
don_P2 <- tidyr::gather(data=don_P,key=cellule,value=P)
don_T2 <- tidyr::gather(data=don_T,key=cellule,value=T)
don_U2 <- tidyr::gather(data=don_U,key=cellule,value=U)
don_V2 <- tidyr::gather(data=don_V,key=cellule,value=V)
don_W2 <- tidyr::gather(data=don_W,key=cellule,value=W)
Y<-rep(don$Y, times=35)
don2 <- cbind(don_P2,don_T2$T,don_U2$U,don_V2$V,don_W2$W,Y)

colnames(don2) <- c("cellule","P","T","U","V","W","Y")

rm(don_P2,don_T2,don_U2,don_V2,don_W2,colsP,colsT,colsU,colsV,colsW,x,Y,cellules,cols)

#on peut facilement améliorer ce code avec apply
str(don2)
summary(don2)
don2<-na.omit(don2)


# liaison des var explicatives avec Y

acp <- prcomp( as.matrix(na.omit(don2[,-1])), subset=don2$cellule , center=TRUE , scale=TRUE )
plot(acp)
summary(acp)
biplot(acp)

install.packages("FactoMineR")
library(FactomineR)
acp2<-PCA(don2, ncp=3 , quali.sup = cellule)

modl_lin <- lm( Y~. , don2 )
summary(modl_lin)

modl_lin <- lm( Y~. , don2 , subset = don2$cellule) # NOK

modl_lin <- lm( Y~. , don )
summary(modl_lin)
names(modl_lin)
#on récupère les var significatives
var_sign <- data.frame("coeff"=modl_lin$proba)
var_sign$var <- rownames(var_sign)
var_sign <- var_sign[var_sign$prob<0.05,]
var_sign <- var_sign[-1,]


library(dplyr)
var_sign <- arrange(var_sign, desc(abs(coeff)))




library(caret)
set.seed(123)
trainIndex <- createDataPartition(don, p = .8) #NOK
don_t<-don[trainIndex,]

#simple cross validation with 10 folds
fitControl<-trainControl(method = "cv",  number = 10)

#regression linéaire avec selection de variable stepwise
modl_lin2 <- train( Y~. , data=don , method = 'leapSeq' , nvmax = 15)

# regression pénalisée
modl_lin3 <- train( Y~. , data=don , method = 'penalized' , lambda1 = 0.1 , lambda2 = 0.2 )
#method = 'lasso'

#erreur de prévision
#MSE <- biais² + variance
prev<-predict(modl)

library(caret)
varImp