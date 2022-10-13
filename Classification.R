# 1 : Appliquer kmeans
# Lecture des données
Movie_Data<-read.csv2("Movie_Data.csv")
str(Movie_Data)

# centrage réduction des données

Movie_Data_cr<-scale(Movie_Data,center=T,scale=T)

# k-means avec les données centrées et réduites

groupes.kmeans <- kmeans(Movie_Data_cr,centers=3,nstart=4)
print(groupes.kmeans)
#Taux d’inertie est assez faible , mauvaise homogenisation de classification.

# 2 : Déterminer Nc le nombre de classes à retenir

#methode graphique de Taux d’inertie pour determiner le nombre de classe optimale a choisir
N=110
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(Movie_Data_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
max(inertie.expl) [1] 0.9495996  #<95%
N=111
inertie.expl <- rep(0,times=N)
for (k in 2:N){
clus <- kmeans(Movie_Data_cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
max(inertie.expl) [1] 0.9500323
#N=111 est le plus petit entier dont le taux d’inertie expliquée est supérieur à 0.95
#graphique
plot(1:N,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
# Déterminon Nc le nombre de classes
var(inertie.expl[21:N])*(N-21)*100/(var(inertie.expl)*(N-1)) [1] 5.499961
var(inertie.expl[22:N])*(N-22)*100/(var(inertie.expl)*(N-1)) [1] 5.101492
var(inertie.expl[23:N])*(N-23)*100/(var(inertie.expl)*(N-1)) [1] 4.718372
#Nc=22 est le nombre de classes à retenir

 # 3 : CAH
# Les données sont préalablement centrées et réduites sur Excel
Movie_Data_cr<-read.csv2("Movie_Data_cr.csv",row.names=NULL)
library(FactoMineR)
Res<-HCPC(Movie_Data_cr,nb.clust=-1)

# 3.1 : variables quantitatives les plus corrélées avec la variable classification
Res$desc.var
# 3.3 : Calculer les taux d’inertie
I<-Res$call
I
