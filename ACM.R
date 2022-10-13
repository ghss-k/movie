library(FactoMineR)
library(ggplot2)
install.packages("devtools")
library(factoextra)
library(psych)
library("readxl")
library("corrplot")
install.packages("FactoMineR")
library("FactoMineR")
library("arules")

########## ACM ###########

# read Movie_Data 
Movie_Data <- read_excel("C:\Users\dell\Downloads\Movie_Movie_Data.xlsx")
Movie_Data

# convert char var to num var  
Movie_Data$Marketing.expense <- as.numeric(as.character(Movie_Data$Marketing.expense)) 
Movie_Data$Production.expense <- as.numeric(as.character(Movie_Data$Production.expense)) 
Movie_Data$Multiplex.coverage <- as.numeric(as.character(Movie_Data$Multiplex.coverage))
Movie_Data$Budget <- as.numeric(as.character(Movie_Data$Budget)) 
Movie_Data$Movie_length <- as.numeric(as.character(Movie_Data$Movie_length)) 
Movie_Data$Lead_.Actor_Rating <- as.numeric(as.character(Movie_Data$Lead_.Actor_Rating))
Movie_Data$Lead_Actress_rating <- as.numeric(as.character(Movie_Data$Lead_Actress_rating))
Movie_Data$Producer_rating <- as.numeric(as.character(Movie_Data$Producer_rating)) 
Movie_Data$Critic_rating <- as.numeric(as.character(Movie_Data$Critic_rating))
Movie_Data$Trailer_views <- as.numeric(as.character(Movie_Data$Trailer_views))

# Transformer les variables quantitative au variables qualitative 
Movie_Data_tran <- discretizeDF(Movie_Data,default=list(method = "cluster",breaks = 2,labels=c("0","1")))


# Transformer num var to factor 
Movie_Data_tran$Marketing.expense <- as.factor(as.numeric(Movie_Data_tran$Marketing.expense))  
Movie_Data_tran$Production.expense <- as.factor(as.numeric(Movie_Data_tran$Production.expense))
Movie_Data_tran$Multiplex.coverage <- as.factor(as.numeric(Movie_Data_tran$Multiplex.coverage))
Movie_Data_tran$X4_number_of_convenience_stores <- as.factor(as.numeric(Movie_Data_tran$X4_number_of_convenience_stores)) 
Movie_Data_tran$Budget <- as.factor(as.numeric(Movie_Data_tran$Budget))
Movie_Data_tran$Movie_length <- as.factor(as.numeric(Movie_Data_tran$Movie_length)) 
Movie_Data_tran$Lead_.Actor_Rating <- as.factor(as.numeric(Movie_Data_tran$Lead_.Actor_Rating)) 
Movie_Data_tran$Lead_Actress_rating <- as.factor(as.numeric(Movie_Data_tran$Lead_Actress_rating)) 
Movie_Data_tran$Producer_rating <- as.factor(as.numeric(Movie_Data_tran$Producer_rating)) 
Movie_Data_tran$Trailer_views <- as.factor(as.numeric(Movie_Data_tran$Trailer_views))


# construit le tableau disjonctif 
k <- tab.disjonctif(Movie_Data_tran)
k

# frequence de chaque modalité 
apply(k,2,sum)

# calcul des effectifs de chaque modalité 
propmod=apply(k,2,sum)/(nrow(k))
propmod

# supprimer la variable Y_Trailer_views 
H <- subset(Movie_Data_tran,select=-Trailer_views)


# appliquer ACM 
res<-MCA(H,ncp=8,graph=FALSE,axes=c(2.3))
res

# calcul les valeurs propres 
res$eig


# graphique des valeurs propres 
plot(res$eig[,1],type="b",main="Scree plot") 


# dimension de sous espace
var(res$eig[2:10,1])*8/(var(res$eig[,1])*10)
var(res$eig[3:10,1])*7/(var(res$eig[,1])*10)
var(res$eig[4:10,1])*6/(var(res$eig[,1])*10)

# Calcul cos^2 des modalités 
res$var$cos2 


# Calcul la contribution des modalités 
res$var$contrib 


# tracer la contribution des modalités 
fviz_contrib(res,choice="var",axes=1:2,top=505) 
fviz_contrib(res,choice="var",axes=5:6,top=505) 


# Appliquer CAH au modalité 
HCPC(res$var$contrib) 


# Nuage des modalités 
plot(res,choix="var")

# cos^2 des individus 
res$ind$cos2

# contribution des induvidus 
res$ind$contrib 

# tracer le graphe des contributions des individus 
fviz_contrib(res,choice="var",axes=1:2,top=505) 
fviz_contrib(res,choice="ind",axes=1:2,top=505)

# Applique CAH au contribution des individus 
HCPC(res$ind$contrib) 

# Calcul des coefficient de corrélation des modalité 
res$var$eta2 

