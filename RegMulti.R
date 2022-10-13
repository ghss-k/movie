library("readxl")
library(tidyverse)

#importation des données
Movie_Data <- read_excel("C:\Users\dell\Desktop\Projets\Analyse de données\Movie_Data.csv")
Movie_Data

str(Movie_Data)

#application de la regression multiple
modele <- lm(Trailer_views~Marketing.expense+Production.expense+Multiplex.coverage+Budget+Movie_length+Lead_.Actor_Rating+Lead_Actress_rating+Director_rating+Producer_rating+Critic_rating,data=Movie_Data) 
summary(modele) 

#Test de fisher
fisher.test(Movie_Data) 
s<-summary(modele)
s$fstatistic

#Application de la modele du step
modele2 <- step(modele)
summary(modele2)
AIC(modele)
AIC(modele2)

#Test de normalité
ks.test(resid(modele2),pnorm)
shapiro.test(resid(modele2))

#test d'homoscedastisité
plot(predict(modele), resid(modele))
abline(h=0)

#Valeurs aberrantes : approximation du résidu standardisé < 2
sd=sqrt(deviance(modele)/df.residual(modele))
nbrow = 506
seqx=seq(1,nbrow,length=nbrow) #indices de ligne de chaque valeur
abr=abs(Movie_Data$Trailer_views-predict(modele))/sd
plot(seqx,abr, xlab="séquence", ylab="valeurs aberrantes")
abline(h=2, lty=2,col=2)

#régression par étage


#tableau de corrélation
mcor <- cor(Movie_Data)
mcor


#Exemple de calcule pour appliquer la procédure de remplir le tableau

#On commence par Budget qui a le plus grand coefficient de correlation parmi les variables explicatifs
corr <- -2.887e+05-7.455e+00*Movie_Data$Budget
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)
max(abs(cor(modele3$residuals,Movie_Data$Marketing.expense)),
    abs(cor(modele3$residuals,Movie_Data$Production.expense)),
    abs(cor(modele3$residuals,Movie_Data$Multiplex.coverage)),
    abs(cor(modele3$residuals,Movie_Data$Budget)),
    abs(cor(modele3$residuals,Movie_Data$Movie_length)),
    abs(cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)),
    abs(cor(modele3$residuals,Movie_Data$Lead_Actress_rating)),
    abs(cor(modele3$residuals,Movie_Data$Director_rating)),
    abs(cor(modele3$residuals,Movie_Data$Producer_rating)),
    abs(cor(modele3$residuals,Movie_Data$Critic_rating)))

cor(modele3$residuals,Movie_Data$Marketing.expense)
cor(modele3$residuals,Movie_Data$Production.expense)
cor(modele3$residuals,Movie_Data$Multiplex.coverage)
cor(modele3$residuals,Movie_Data$Budget)
cor(modele3$residuals,Movie_Data$Movie_length)
cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)
cor(modele3$residuals,Movie_Data$Lead_Actress_rating)
cor(modele3$residuals,Movie_Data$Director_rating)
cor(modele3$residuals,Movie_Data$Producer_rating)
cor(modele3$residuals,Movie_Data$Critic_rating)



#On vas ajouter Movie_length
corr <- -2.887e+05-7.455e+00*Movie_Data$Budget+8.051e+02*Movie_Data$Movie_length
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)
max(abs(cor(modele3$residuals,Movie_Data$Marketing.expense)),
    abs(cor(modele3$residuals,Movie_Data$Production.expense)),
    abs(cor(modele3$residuals,Movie_Data$Multiplex.coverage)),
    abs(cor(modele3$residuals,Movie_Data$Budget)),
    abs(cor(modele3$residuals,Movie_Data$Movie_length)),
    abs(cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)),
    abs(cor(modele3$residuals,Movie_Data$Lead_Actress_rating)),
    abs(cor(modele3$residuals,Movie_Data$Director_rating)),
    abs(cor(modele3$residuals,Movie_Data$Producer_rating)),
    abs(cor(modele3$residuals,Movie_Data$Critic_rating)))

cor(modele3$residuals,Movie_Data$Marketing.expense)
cor(modele3$residuals,Movie_Data$Production.expense)
cor(modele3$residuals,Movie_Data$Multiplex.coverage)
cor(modele3$residuals,Movie_Data$Budget)
cor(modele3$residuals,Movie_Data$Movie_length)
cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)
cor(modele3$residuals,Movie_Data$Lead_Actress_rating)
cor(modele3$residuals,Movie_Data$Director_rating)
cor(modele3$residuals,Movie_Data$Producer_rating)
cor(modele3$residuals,Movie_Data$Critic_rating)


#On vas ajouter Marketing.expense
corr <- -2.887e+05-7.455e+00*Movie_Data$Budget+6.842e+01*Movie_Data$Marketing.expense+8.051e+02*Movie_Data$Movie_length
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)
max(abs(cor(modele3$residuals,Movie_Data$Marketing.expense)),
    abs(cor(modele3$residuals,Movie_Data$Production.expense)),
    abs(cor(modele3$residuals,Movie_Data$Multiplex.coverage)),
    abs(cor(modele3$residuals,Movie_Data$Budget)),
    abs(cor(modele3$residuals,Movie_Data$Movie_length)),
    abs(cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)),
    abs(cor(modele3$residuals,Movie_Data$Lead_Actress_rating)),
    abs(cor(modele3$residuals,Movie_Data$Director_rating)),
    abs(cor(modele3$residuals,Movie_Data$Producer_rating)),
    abs(cor(modele3$residuals,Movie_Data$Critic_rating)))

cor(modele3$residuals,Movie_Data$Marketing.expense)
cor(modele3$residuals,Movie_Data$Production.expense)
cor(modele3$residuals,Movie_Data$Multiplex.coverage)
cor(modele3$residuals,Movie_Data$Budget)
cor(modele3$residuals,Movie_Data$Movie_length)
cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)
cor(modele3$residuals,Movie_Data$Lead_Actress_rating)
cor(modele3$residuals,Movie_Data$Director_rating)
cor(modele3$residuals,Movie_Data$Producer_rating)
cor(modele3$residuals,Movie_Data$Critic_rating)


#On vas ajouter Production.expense
corr <- -2.887e+05+8.051e+02*Movie_Data$Movie_length+5.658e+02 * Movie_Data$Production.expense+6.842e+01*Movie_Data$Marketing.expense-7.455e+00*Movie_Data$Budget
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)

max(abs(cor(modele3$residuals,Movie_Data$Marketing.expense)),
    abs(cor(modele3$residuals,Movie_Data$Production.expense)),
    abs(cor(modele3$residuals,Movie_Data$Multiplex.coverage)),
    abs(cor(modele3$residuals,Movie_Data$Budget)),
    abs(cor(modele3$residuals,Movie_Data$Movie_length)),
    abs(cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)),
    abs(cor(modele3$residuals,Movie_Data$Lead_Actress_rating)),
    abs(cor(modele3$residuals,Movie_Data$Director_rating)),
    abs(cor(modele3$residuals,Movie_Data$Producer_rating)),
    abs(cor(modele3$residuals,Movie_Data$Critic_rating)))

cor(modele3$residuals,Movie_Data$Marketing.expense)
cor(modele3$residuals,Movie_Data$Production.expense)
cor(modele3$residuals,Movie_Data$Multiplex.coverage)
cor(modele3$residuals,Movie_Data$Budget)
cor(modele3$residuals,Movie_Data$Movie_length)
cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)
cor(modele3$residuals,Movie_Data$Lead_Actress_rating)
cor(modele3$residuals,Movie_Data$Director_rating)
cor(modele3$residuals,Movie_Data$Producer_rating)
cor(modele3$residuals,Movie_Data$Critic_rating)

#On vas ajouter Multiplex.coverage
corr <- -2.887e+05+8.051e+02*Movie_Data$Movie_length+5.658e+02 * Movie_Data$Production.expense+6.842e+01*Movie_Data$Marketing.expense-7.455e+00*Movie_Data$Budget-6.839e+04*Movie_Data$Multiplex.coverage
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)

max(abs(cor(modele3$residuals,Movie_Data$Marketing.expense)),
    abs(cor(modele3$residuals,Movie_Data$Production.expense)),
    abs(cor(modele3$residuals,Movie_Data$Multiplex.coverage)),
    abs(cor(modele3$residuals,Movie_Data$Budget)),
    abs(cor(modele3$residuals,Movie_Data$Movie_length)),
    abs(cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)),
    abs(cor(modele3$residuals,Movie_Data$Lead_Actress_rating)),
    abs(cor(modele3$residuals,Movie_Data$Director_rating)),
    abs(cor(modele3$residuals,Movie_Data$Producer_rating)),
    abs(cor(modele3$residuals,Movie_Data$Critic_rating)))

cor(modele3$residuals,Movie_Data$Marketing.expense)
cor(modele3$residuals,Movie_Data$Production.expense)
cor(modele3$residuals,Movie_Data$Multiplex.coverage)
cor(modele3$residuals,Movie_Data$Budget)
cor(modele3$residuals,Movie_Data$Movie_length)
cor(modele3$residuals,Movie_Data$Lead_.Actor_Rating)
cor(modele3$residuals,Movie_Data$Lead_Actress_rating)
cor(modele3$residuals,Movie_Data$Director_rating)
cor(modele3$residuals,Movie_Data$Producer_rating)
cor(modele3$residuals,Movie_Data$Critic_rating)

#On va ajouter Director rating
corr <- -2.887e+05+8.051e+02*Movie_Data$Movie_length+5.658e+02 * Movie_Data$Production.expense+6.842e+01*Movie_Data$Marketing.expense-7.455e+00*Movie_Data$Budget-6.839e+04*Movie_Data$Multiplex.coverage-1.792e+04*Movie_Data$Director_rating
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)


#On s'arrete car l'équation de Régression se dégrade et on élimine Director rating
corr <- -2.887e+05+8.051e+02*Movie_Data$Movie_length+5.658e+02 * Movie_Data$Production.expense+6.842e+01*Movie_Data$Marketing.expense-7.455e+00*Movie_Data$Budget-6.839e+04*Movie_Data$Multiplex.coverage
modele3 <- lm(Trailer_views~ corr,data=Movie_Data)
summary(modele3)

AIC(modele3)

#Test de normalité
ks.test(resid(modele3),pnorm)
shapiro.test(resid(modele3))

