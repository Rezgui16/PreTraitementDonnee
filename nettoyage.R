#authors: soumia Rezgui 
#{r, fig.show='hide',  warning=TRUE, message=FALSE}


#importation des librairies necessaires au traitement des données
library(funModeling)
library(dplyr)
library (plyr)
#se mettre dans le dossier ou on veut travailler 
setwd("C:/Users/Soumia/Desktop/Données")


#lecture du fichier des données contenant les RSV
#encoding utilisé pour enlever les carac speciaux et stringsAsFactor= FALSE pour ne pas avoir factor comme type

LR <- read.csv("c:/Users/Soumia/Desktop/Données/ListeReserves.csv",header = TRUE, sep =';',
               encoding = "UTF-8")

#View(LR)
summary(LR$CategorieReserve)
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
#parce que ça commençait par NULL 
#dfnew$Mandataire<-as.Date(dfnew$Mandataire)
#dfnew$SousTraitant<- as.Date(dfnew$SousTraitant)

#modification des NULL 
dfnew$Mandataire[dfnew$Mandataire == "NULL"]<- NA
dfnew$SousTraitant[dfnew$SousTraitant == "NULL"]<- NA
dfnew$Mandataire<-as.Date(dfnew$Mandataire)
dfnew$SousTraitant<-as.Date(dfnew$SousTraitant)


View(dfnew)

#résumé des données du Data set
summary(dfnew)


#afin de connaitre si y'a pas d'erreurs d'ecritures, ça nous ressort toutes les modalités 
levels(dfnew$CategorieReserve)
levels(dfnew$ModeleReserve)

#modification des valeurs de la colonne CategorieReserve
dfnew$CategorieReserve <- revalue (dfnew$CategorieReserve , c("EXECUTION"="EXEC"))

#c'est sur que ça marche mais je veux quelque chose de plus pratique
#CategorieReserve[CategorieReserve == "Autre"]<- autre

#2eme solution
#daycols = c("dfnew$CategorieReserve")
#for(icol in daycols){
#  if(icol =="Autre" | icol =="Autres" | icol =="autre"  ){
#    icol <- autre
#  }
#}

#remplacer quelques données mal ecrites
#colonne modelreserve
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Autre","Autres","autre","autres")] <- "autre"
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Joints / joints silicone","joint sylicone a réaliser","Joint","joint","Joint silicone")]<- "joint"

#colonne metier
dfnew$Metier[dfnew$Metier %in% c("PEINTURE 4B3","PEINTURE","Peinture","12-Peinture","Peintures","Peinture+Signalétique","LOT 13 - PEINTURE",
                                 "PEINTURE RAVALEMENT NETTOYAGE","B1.12-PSG - PEINTURE  - SIGNALETIQUE","A11-PSG - PEINTURE  - SIGNALÉTIQUE")] <- "Peinture"

#j'ai un probleme ça me l'est remet en Na's 
#dfnew$Metier[dfnew$Metier %in% c("Plomberie","PLOMBERIE","PLOMBERIE LOGEMENT",
                                # "Plomberie - sanitaire","A13-PLB - PLOMBERIE")]

#dfnew$Metier[dfnew$Metier %in% c("Plomberie / CVC","13/14-Plomberies / CVC","Plomberie Sanitaire + CVC",
                                # "Plomberie/Sanitaire/CVC","CVC PLOMBERIE","Plomberie CVC")]<-"plomberie+cvc"

#fonction pour les doublons 
#duplicated2 <- function(x){ 
#  if (sum(dup <- duplicated(x))==0) 
#    return(dup) 
#  if (class(x) %in% c("data.frame","matrix")) 
#    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
#  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
#}

#appel de la fonction
#doublons <- duplicated2(dfnew$Id)

dfnew2<-dfnew
View(dfnew)
doublonstest<-which(duplicated(dfnew$Id))
dfnew2$Id<- dfnew$Id[-doublonstest]

sum(duplicated(dfnew2$Id))
summary(dfnew2$Id) 

#creation de la nouvelle colonne en prenant en compte que la colonne leveeMoEMOA
#if(any(!is.na(dfnew$DateDeLeveeMOEMOA))) dfnew$LeveeReserve <- ifelse(!(is.na(dfnew$DateDeLeveeMOEMOA)), 1, 0)

#creation de la colonne cible en prenant en compte deux colonnes Mandataire et leveeMOEMOA
if(any(!is.na(dfnew$DateDeLeveeMOEMOA)) | any(!is.na(dfnew$Mandataire))) dfnew$LeveeReserve <- ifelse(!(is.na(dfnew$DateDeLeveeMOEMOA)) | !(is.na(dfnew$Mandataire)), 1, 0)
View(dfnew)

summary(dfnew$CategorieReserve)
#recodage de la variable categorieReserve en numeric
#EXEC<-1, GPA<-2, OPR<-3

#solution 1
#dfnew$CategorieReserve[dfnew$CategorieReserve =="EXEC"] <- 1
#dfnew$CategorieReserve[dfnew$CategorieReserve =="GPA"]  <- 2
#dfnew$CategorieReserve[dfnew$CategorieReserve =="OPR"]  <- 3 

#solution 2
#if(dfnew$CategorieReserve =="EXEC"){dfnew$CategorieReserve <- 0}else if(dfnew$CategorieReserve =="GPA"){dfnew$CategorieReserve<-1}else(dfnew$CategorieReserve =="OPR"){dfnew$CategorieReserve <-2}

#solution 3
dfnew$CategorieReserve <- revalue (dfnew$CategorieReserve , c("EXEC"= 1))
dfnew$CategorieReserve <- revalue (dfnew$CategorieReserve , c("GPA"= 2))
dfnew$CategorieReserve <- revalue (dfnew$CategorieReserve , c("OPR"= 3))

#on a des valeurs NULL dans la colonne Categorie Reserve, on les met dans la deuxieme categorie GPA
#dfnew$CategorieReserve[dfnew$CategorieReserve == "NULL"]<- 2
dfnew$CategorieReserve <- revalue (dfnew$CategorieReserve , c("NULL"= 2))

#conversion des reserves bloquante en type categoriel
dfnew$ReserbeBloquante<-as.factor(dfnew$ReserbeBloquante)
dfnew$LeveeReserve<-as.factor(dfnew$LeveeReserve)

#verifier les typages des variables
class(dfnew$LeveeReserve)
class(dfnew$ReserbeBloquante)
class(dfnew$CategorieReserve)


#write.csv(dfnew,file="C:/Users/Soumia/Desktop/Données/dfnewExport.csv",row.names = FALSE, quote = FALSE)
summary(dfnew$CategorieReserve)
summary(dfnew$ReserbeBloquante)
summary(dfnew$LeveeReserve)


