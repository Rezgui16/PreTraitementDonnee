#authors: soumia Rezgui 
#{r, fig.show='hide',  warning=TRUE, message=FALSE}


#importation des librairies necessaires au traitement des données
library(funModeling)
library(dplyr)

#se mettre dans le dossier ou on veut travailler 
setwd("C:/Users/Soumia/Desktop/Données")


#lecture du fichier des données contenant les RSV
#encoding utilisé pour enlever les carac speciaux et stringsAsFactor= FALSE pour ne pas avoir factor comme type

LR <- read.csv("c:/Users/Soumia/Desktop/Données/ListeReserves.csv",header = TRUE, sep =';',
               encoding = "UTF-8")

#View(LR)

#appreçu sur les données 
#le nom de la premiere colonnne est incorrecte
colnames(LR)[colnames(LR)=="X.U.FEFF.Id"] <- "Id" 
View(LR)


# Création d'une nouvelle base de donnees avec les colonnes qui nous interesse 
#on eneleve tout ce qui est commentaires et données non fiables
dfnew <- LR[, c(1:8, 10,12,14,15, 19:25)]

#voir la nouvelle base de données 
View(dfnew)


#permet d'avoir le nbr d'obs et le typage des vars
#275803 obs. ET 19 variables
str(dfnew)

#exploration des données manquantes 
summary(dfnew)

#enlever les accents des differentes colonnes
colnames(dfnew)[colnames(dfnew)=="Métier"] <- "Metier" 
colnames(dfnew)[colnames(dfnew)=="CatégorieMetier"] <- "CategorieMetier" 
colnames(dfnew)[colnames(dfnew)=="EntitéId"] <- "EntiteId" 
colnames(dfnew)[colnames(dfnew)=="Entité"] <- "Entite" 

#exporter la nouvelle base de données 
#write.csv(dfnew,file="C:/Users/Soumia/Desktop/Données/dfnewExport.csv",row.names = FALSE, quote = FALSE)

#renvoit, pour chaque variable, le nombre de valeurs égales à zéro, le nombre de valeurs manquantes,
#et le nombre de valeurs infinies (par exemple 1/0), ainsi que les pourcentages correspondant.
df_status(dfnew)

#determiner les variables categorielles
#levels(dfnew$ReserbeBloquante)

#modifier les factor en Date
#colonne dateCreation
#si je veux préciser le format
#dfnew$DateCreation<- as.Date(dfnew$DateCreation,format = "%d/%m/%Y")
dfnew$DateCreation<- as.Date(dfnew$DateCreation)

#colonne date de derniere modification 
dfnew$DateDerniereModification<- as.Date(dfnew$DateDerniereModification)

#COLONNE Date de levee MOE/MOA
dfnew$DateDeLeveeMOEMOA<- as.Date(dfnew$DateDeLeveeMOEMOA)

#Colonne Date de levee potentielle 
dfnew$DateLeveePotentielle<-as.Date(dfnew$DateLeveePotentielle)

#renommer des coclonnes ecriture trop longue
colnames(dfnew)[colnames(dfnew)=="DateDeLeveeCotraitantSousTraitant"] <- "SousTraitant"
colnames(dfnew)[colnames(dfnew)=="DateDeLeveeMandataire"] <- "Mandataire"

#probleme au niveau des conversion du typage des deux colonnes 
#dfnew$Mandataire<-as.Date(dfnew$Mandataire)
#dfnew$SousTraitant<- as.Date(dfnew$SousTraitant)


View(dfnew)
