#authors: soumia Rezgui 
#{r, fig.show='hide',  warning=TRUE, message=FALSE}


#importation des librairies necessaires au traitement des donn�es
library(funModeling)
library(dplyr)
library (plyr)
#se mettre dans le dossier ou on veut travailler 
setwd("C:/Users/Soumia/Desktop/Donn�es")


#lecture du fichier des donn�es contenant les RSV
#encoding utilis� pour enlever les carac speciaux et stringsAsFactor= FALSE pour ne pas avoir factor comme type

LR <- read.csv("c:/Users/Soumia/Desktop/Donn�es/ListeReserves.csv",header = TRUE, sep =';',
               encoding = "UTF-8")

#View(LR)

#appre�u sur les donn�es 
#le nom de la premiere colonnne est incorrecte
colnames(LR)[colnames(LR)=="X.U.FEFF.Id"] <- "Id" 
View(LR)


# Cr�ation d'une nouvelle base de donnees avec les colonnes qui nous interesse 
#on eneleve tout ce qui est commentaires et donn�es non fiables
dfnew <- LR[, c(1:8, 10,12,14,15, 19:25)]

#voir la nouvelle base de donn�es 
View(dfnew)


#permet d'avoir le nbr d'obs et le typage des vars
#275803 obs. ET 19 variables
str(dfnew)

#exploration des donn�es manquantes 
summary(dfnew)

#enlever les accents des differentes colonnes
colnames(dfnew)[colnames(dfnew)=="M�tier"] <- "Metier" 
colnames(dfnew)[colnames(dfnew)=="Cat�gorieMetier"] <- "CategorieMetier" 
colnames(dfnew)[colnames(dfnew)=="Entit�Id"] <- "EntiteId" 
colnames(dfnew)[colnames(dfnew)=="Entit�"] <- "Entite" 

#exporter la nouvelle base de donn�es 
#write.csv(dfnew,file="C:/Users/Soumia/Desktop/Donn�es/dfnewExport.csv",row.names = FALSE, quote = FALSE)

#renvoit, pour chaque variable, le nombre de valeurs �gales � z�ro, le nombre de valeurs manquantes,
#et le nombre de valeurs infinies (par exemple 1/0), ainsi que les pourcentages correspondant.
df_status(dfnew)

#determiner les variables categorielles
#levels(dfnew$ReserbeBloquante)

#modifier les factor en Date
#colonne dateCreation
#si je veux pr�ciser le format
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
#parce que �a commen�ait par NULL 
#dfnew$Mandataire<-as.Date(dfnew$Mandataire)
#dfnew$SousTraitant<- as.Date(dfnew$SousTraitant)

#modification des NULL 
dfnew$Mandataire[dfnew$Mandataire == "NULL"]<- NA
dfnew$SousTraitant[dfnew$SousTraitant == "NULL"]<- NA
dfnew$Mandataire<-as.Date(dfnew$Mandataire)
dfnew$SousTraitant<-as.Date(dfnew$SousTraitant)


View(dfnew)

#r�sum� des donn�es du Data set
summary(dfnew)


#afin de connaitre si y'a pas d'erreurs d'ecritures, �a nous ressort toutes les modalit�s 
levels(dfnew$CategorieReserve)
levels(dfnew$ModeleReserve)

#modification des valeurs de la colonne CategorieReserve
dfnew$CategorieReserve <- revalue (dfnew$CategorieReserve , c("EXECUTION"="EXEC"))

#c'est sur que �a marche mais je veux quelque chose de plus pratique
#CategorieReserve[CategorieReserve == "Autre"]<- autre

#2eme solution
#daycols = c("dfnew$CategorieReserve")
#for(icol in daycols){
#  if(icol =="Autre" | icol =="Autres" | icol =="autre"  ){
#    icol <- autre
#  }
#}

#remplacer quelques donn�es mal ecrites
#colonne modelreserve
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Autre","Autres","autre","autres")] <- "autre"
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Joints / joints silicone","joint sylicone a r�aliser","Joint","joint","Joint silicone")]<- "joint"

#colonne metier
dfnew$Metier[dfnew$Metier %in% c("PEINTURE 4B3","PEINTURE","Peinture","12-Peinture","Peintures","Peinture+Signal�tique","LOT 13 - PEINTURE",
                                 "PEINTURE RAVALEMENT NETTOYAGE","B1.12-PSG - PEINTURE  - SIGNALETIQUE","A11-PSG - PEINTURE  - SIGNAL�TIQUE")] <- "Peinture"

#j'ai un probleme �a me l'est remet en Na's 
#dfnew$Metier[dfnew$Metier %in% c("Plomberie","PLOMBERIE","PLOMBERIE LOGEMENT",
                                # "Plomberie - sanitaire","A13-PLB - PLOMBERIE")]

#dfnew$Metier[dfnew$Metier %in% c("Plomberie / CVC","13/14-Plomberies / CVC","Plomberie Sanitaire + CVC",
                                # "Plomberie/Sanitaire/CVC","CVC PLOMBERIE","Plomberie CVC")]<-"plomberie+cvc"



summary(dfnew$Metier)