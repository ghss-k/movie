#1 : ACP normée
Movie_Data<-read.csv2("Movie_Data.csv")
library(FactoMineR)
pca.data <- PCA(Movie_Data, scale.= TRUE)

# 2 : les variables sont centrer pour que G soit situé à l’origine. Les variables sont réduit pour rendre comparables les variables exprimées sur des échelles (unités) différentes

# 3 : l’indice KMO et les indices MSAI, Conclure

library(psych)
KMO(cor(Movie_Data))

# KMO= 0.92 La solution factorielle est excellente

# 4 : valeurs propres, pourcentage d’inertie de chaque valeur propre et le cumul des pourcentages d’inertie

>	pca.data$eig
# 5 : graphique des valeurs propres

plot(1:11, pca.data$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphi que des valeurs propres")

# 6 : dimension du sous espace = 4

var(pca.data$eig[2:11,1])*(11-2)/(var(pca.data$eig[,1])*(11-1)) 
var(pca.data$eig[3:11,1])*(11-3)/(var(pca.data$eig[,1])*(11-1)) [1] 



# Nuage des variables

# 7 : cos2 des variables sur le sous espace / Qualité de représentation

pca.data$var$cos2 
# visualiser les variables bien représentées, moyennement représentées et faiblement représentées pour chaque dimension
library(corrplot)
library(factoextra)
res <- get_pca_var(pca.data)
corrplot(res$cos2) 

# 9 : la contribution des variables dans chaque axe du sous espace

pca.data$var$contrib


# Plus la valeur de la contribution est importante, plus la variable contribue à la composante principale en question.

# 10 :	Appliquer la CAH au tableau des contributions des variables aux axes du sous espace, interpréter les résultats

cont <- pca.data$var$contrib
write.table(cont,"Cont_Var.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don<-read.csv2("Cont_var.csv",row.names=1)
Cat_var<-HCPC(Don)

# 11 : Tracer le nuage des variables projeté sur les 2 premiers axes

fviz_pca_var(pca.data)


# 12 : variables qui sont relativement bien corrélées (positivement et négativement) avec les axes du 1er plan factoriel



# 13 :	Calculer le cos2 des individus sur le sous espace
pca.data$ind$cos2

#14 :Distinguer les individus bien représentées, moyennement représentées et faiblement représentées sur le sous espace
# visualiser les individues bien représentées, moyennement représentées et faiblement représentées pour chaque dimension
fviz_pca_ind (pca.data, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
             )


# 15 :Calculer la contribution des individus dans chaque axe du sous espace
pca.data$ind$contrib

# 16 :	Appliquer la CAH au tableau des contributions des individus aux axes du sous espace, interpréter les résultats
cont <- pca.data$ind$contrib
write.table(cont,"Cont_Ind.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don<-read.csv2("Cont_ind.csv",row.names=1)
Cat_ind<-HCPC(Don) 
# 17 :	Tracer le nuage des individus projeté sur les 2 premiers axes
fviz_pca_ind(pca.data)
