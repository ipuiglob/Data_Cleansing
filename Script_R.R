####################################################################################################
#Codi Practica 2 by Ivan Puig
#
#Atencio a les llibreries que cal carregar
####################################################################################################

library(mice)
library(psych)
library(ggplot)
library(ggplot2)
library(C50)


#Lectura del fitxer csv I c�rrega de les dades a un dataframe
Titanic <- read.csv(file="C:/Temp/train.csv",head=TRUE,sep=",")

#funcions per inspeccionar les dades
Titanic
Summary(Titanic)
levels(Titanic$Embarked) o > levels(Titanic$Sex)
sapply(Titanic, function(x) class(x))
sapply(titanic, function(x) sum(is.na(x)))

#Funci� que ens calcula la mitjana sense tenir en compte els valors nuls(NA)
mean(titanic_net$Age, na.rm=TRUE)

#identificaci� dels valors extrems
boxplot.stats(titanic$Age)$out

#representaci� d'un diagrama de caixes de la variable edat
boxplot(titanic$Age)

#Creaci� d'un subset del dataset principal
titanic_net=subset(titanic, select=c(Survived, Pclass, Sex, Age))

#Algunes funcions estad�stiques
Mean(titanic_net$Age, na.rm=TRUE)
sd(titanic_net$Age, na.rm=TRUE)
median(titanic_net$Age, na.rm=TRUE)
range(titanic_net$Age, na.rm=TRUE)
        
#Calcul de la normalitat
mostra <- rnorm(titanic_net$Age, mean(titanic_net$Age, na.rm=TRUE), sd(titanic_net$Age, na.rm=TRUE))

#Homogeneitat de la variancia
describeBy(titanic_net$Age, titanic_net$Pclass)
titanic_class <- subset(titanic_net, Pclass==1 | Pclass==2 | Pclass==3)
plot(Age ~ Pclass, data = titanic_class)
bartlett.test(titanic_class$Age ~ titanic_class$Pclass)
ggplot(titanic_class, aes(x=Pclass,y=Age,colour=Pclass)) + geom_boxplot() + geom_point() + theme_bw() + theme(legend.position = "none")
        
#Instrucci� que converteix de num�ric a factor un camp del dataset
titanic_net$Survived <- as.factor(titanic_net$Survived)
      
#Funci� que crea un arbre de classificaci�
arbre <- C5.0(Survived ~ Class + Sex + Age, data = titanic_net, control = C5.0Control(noGlobalPruning = TRUE))
        
#Visualitzaci� de les dades obtingudes de l'arbre de classificaci�
arbre
summary(arbre)
        
#Representaci� de les regles de l'arbre de classificaci�
arbre <- C5.0(Survived ~ Pclass + Sex + Age, data = titanic_net, control = C5.0Control(noGlobalPruning = TRUE),rules = TRUE)
        
summary(arbre)
        
#representaci� gr�fica de l'arbre de classificaci�
plot(arbre)
        