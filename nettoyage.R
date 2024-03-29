#authors: soumia Rezgui 
#{r, fig.show='hide',  warning=TRUE, message=FALSE}


#importation des librairies necessaires au traitement des données
library(funModeling)
library(dplyr)
library (plyr)
#se mettre dans le dossier ou on veut travailler 
setwd("C:/Users/Soumia/Desktop/Donn�es")


#lecture du fichier des données contenant les RSV
#encoding utilisé pour enlever les carac speciaux et stringsAsFactor= FALSE pour ne pas avoir factor comme type

LR <- read.csv("c:/Users/Soumia/Desktop/Donn�es/ListeReserves.csv",header = TRUE, sep =';',
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

#exploration des donn�ees manquantes 
summary(dfnew)

#enlever les accents des differentes colonnes
colnames(dfnew)[colnames(dfnew)=="M�tier"] <- "Metier" 
colnames(dfnew)[colnames(dfnew)=="Cat�gorieMetier"] <- "CategorieMetier" 
colnames(dfnew)[colnames(dfnew)=="Entit�Id"] <- "EntiteId" 
colnames(dfnew)[colnames(dfnew)=="Entit�"] <- "Entite"

#exporter la nouvelle base de donn�es 
#write.csv(dfnew,file="C:/Users/Soumia/Desktop/Données/dfnewExport.csv",row.names = FALSE, quote = FALSE)

#renvoit, pour chaque variable, le nombre de valeurs  , le nombre de valeurs manquantes,
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

dfnew$ModeleReserve <- as.character(dfnew$ModeleReserve)
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Autre","Autres","autre","autres")] <- "AUTRE"
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Joints / joints silicone","joint sylicone a r�aliser","Joint","joint","Joint silicone",
                                               "Joint �tanch�it�","Joint de finition","Calfeutrement / joint silicone","joints")]<- "JOINT"
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Nettoyage sur rail alu","Nettoyage")] <- "NETTOYAGE"
dfnew$ModeleReserve[dfnew$ModeleReserve %in% c("Entr�e d'air (Int)","Entr�e d'air (Int) ou grille d'air (ext")] <- "ENTREE D'AIR"
dfnew$ModeleReserve <- as.factor(dfnew$ModeleReserve)
summary(dfnew$ModeleReserve)
#colonne metier
#peinture
dfnew$Metier[dfnew$Metier %in% c("PEINTURE 4B3","PEINTURE","Peinture","12-Peinture","Peintures","Peinture+Signal�tique","LOT 13 - PEINTURE",
                                 "PEINTURE RAVALEMENT NETTOYAGE","B1.12-PSG - PEINTURE  - SIGNALETIQUE","A11-PSG - PEINTURE  - SIGNAL�TIQUE",
                                 "08 - PEINTURE","PEINTURE 4B1","PEINTURE / REVETEMENT","Peinture int�rieure","Lot 12 - Peinture"
                                 ,"PEINTRE","LOT 10 - PEINTURE / REV�TEMENTS MURALS","Peintures (A2-A3-A4 )")] <- "PEINTURE"

#plomberie
dfnew$Metier[dfnew$Metier %in% c("Plomberie","PLOMBERIE","PLOMBERIE LOGEMENT","Plomberie - sanitaire","A13-PLB - PLOMBERIE",
                                 "Plomberie sanitaire","13- Plomberies","LOT 16 - PLOMBERIE")] <- "PLOMBERIE"

#il faut au moment de la lecture du fichier faire STRINGFACTOR= FALSE chose qui ne m'arrange pas, �a me donne pas acc�e au differents levels
#je dois convertir en caractere puis remetter en factor � chaque changement 

dfnew$Metier <- as.character(dfnew$Metier)
dfnew$Metier[dfnew$Metier %in% c("CFO - CFA","ELECTRICITE (CDC)","CFA - CFO","ELECTRICITE (CFO-CFA)","Electricit� - CFO/CFA",
                                 "A14-CFO - COURANTS FORTS","ELEC_COURANT FAIBLE","electricite","Electricit� ",
                                 "16-Courant fort - Courant faible","ELEC_COURANT FORT","CFO/CFA","ELECTRICITE CFO_CFA",
                                 "Electricit� CFA/ CFO/SSI","COURANT FORT MISE EN SERVICE & ESSAIS","COURANT FORT AUTOCONTROLES",
                                 "B3-CFO - COURANTS FORTS","Courants Forts","ELECTRICITE CFO/CFA","Electricit� B. T.",
                                 "12 - CFO/CFA","Courants Faibles","Electricite CFO/CFA","Electricit�","CFO CFA","CFO","Electricit� courants forts",
                                 "Courant Fort","ELECTRICITE (CFA)","LOT - ELECTRICITE","ELECTRIQUE","ELEC","ELECTRICITE CFA")] <- "ELECTRICITE"

dfnew$Metier[dfnew$Metier %in% c("Plomberie / CVC","13/14-Plomberies / CVC","Plomberie Sanitaire + CVC",
                                 "Plomberie/Sanitaire/CVC","CVC PLOMBERIE","Plomberie CVC","PLOMBERIE / CVC",
                                 "B2.3-PFS - PLOMBERIE - FLUIDES SPECIAUX","Plomberie / Chauffage","PLOMBERIE & CHAUFFAGE",
                                 "PLOMBERIE CVC","Chauffage / Plomberie / Ventilation","PLOMBERIE CHAUFFAGE ECS VMC","CLIMATISATION VENTILATION PLOMBERIE",
                                 "Plomberie - CVC","CVC/PB")]<-"PLOMBERIE+CVC"

dfnew$Metier[dfnew$Metier %in% c("B1.1-GRO - GROS-OEUVRE - MACONNERIES","A1-GRO - GROS OEUVRE - MACONNERIES - RAVALEMENT",
                                 "14 Ma�onnerie","CCO - MACONNERIE","Ma�onnerie")] <- "MACONNERIE"

dfnew$Metier[dfnew$Metier %in% c("GROS OUVRE","Gros oeuvre","Gros-Ouvre","Gros Oeuvre","Gros ouvre","GROS OEUVRE",
                                 "GROS OEUVRE","GROS  OEUVRE","ML1 INSTALLATION DE CHANTIER - GROS OEUVRE","Gros Ouvre",
                                 "1-Gros Oeuvre")] <- "GROS OEUVRES"

dfnew$Metier[dfnew$Metier %in% c("Menuiserie int�rieure","MENUISERIES EXTERIEURES/OCCULTATIONS","Menuiserie int�rieure bois",
                                 "Menuiserie int�rieure (placard, meuble)","A9-MIN - MENUISERIES INT�RIEURES",
                                 "Menuiseries ext�rieures","A2-MFA - MENUISERIES EXTERIEURES - FACADES",
                                 "Menuiserie ext�rieure PVC","MENUISERIES EXTERIEURES_OCCULTATIONS",
                                 "Menuiserie ext�rieure+occultations","Menuiseries interieures",
                                 "Menuiseries Exterieures Bois","Menuiseries Int�rieures Bois",
                                 "B1.10-MIN - MENUISERIES INT�RIEURES","Menuiseries Int�rieures",
                                 "Menuiserie ext�rieure","5-Menuiserie int�rieure","MENUISERIES EXTERIEURES",
                                 "MENUISERIES INTERIEURES","MENUISERIE INTERIEURE","MENUISERIE BOIS",
                                 "Menuiseries Ext�rieures","Pose Menuiserie ext�rieure",
                                 "MENUISERIES EXTERIEURES ALU ","MENUISERIES INTERIEURE / EXTERIEURE","Menuiserie Interieur"
                                 ,"MENUISERIES INTERIEURES BOIS","Menuiserie Interieure","Menuiserie Ext. Alu et Mur rideau",
                                 "MENUISERIE EXT/ FOURNITURE PVC","MENUISERIE EXTERIEURE","LOT 5 - MENUISERIES EXTERIEURES / FACADES /STORES",
                                 "MENUISERIES EXTERIEURES_OCCULTATIONS 2","LOT 07 - MENUISERIES INTERIEURES",
                                 "B1.2-MFA - MENUISERIES EXTERIEURES - FACADES","Menuis. ext. PVC","METALLERIE SERRURERIE",
                                 "Menuiserie int�rieure (portes, plinthes)","04 - Menuiserie int�rieures")] <- "MENUISERIE"

dfnew$Metier[dfnew$Metier %in% c("CHAUFFAGE VENTILATION CLIMATISATION","B2.1-CVC - CHAUFFAGE VENTILATION CLIMATISATION DESENFUMAGE"
                                 ,"A12.1-CVC - CHAUFFAGE VENTILATION CLIMATISATION DESENFUMAGE","14-CVC","CVC - D","LOT 15 - CVC",
                                 "Chauffage, Ventilation, Climatisation et d�senfumage","Ventilation")] <- "CVC"

dfnew$Metier[dfnew$Metier %in% c("A4-SER - SERRURERIE","SERRURERIE_METALLERIE","ML2&3 ETANCHEITE - COUVERTURE - FACADE - SERRURERIE",
                                "SERRURERIE","B1.4-SMS - SERRURERIE - METALLERIE - OUVRAGES DE SURETE","Serrurerie",
                                "LOT 11 - MENUISERIE INT�RIEURES / SERRURERIE / M�TALLERIE")] <- "SERRURERIE"

dfnew$Metier[dfnew$Metier %in% c("Faux Plafonds - GRG","Cloisons modulaires/Faux plafond",
                                 "REVETEMENTS DURS",
                                 "Platrerie","Faux Plafond","FAUX PLAFOND","USdata$Cause <- factor(USdata$Cause)",
                                 "6-Faux Plafond Min�ral","Rev�tements de murs ","PLATRERIE PEINTURE FAUX PLAFOND",
                                 "B1.7-FPL - FAUX PLAFOND","6-Faux Plafonds","A6-FPL - FAUX PLAFOND","Faux plafonds",
                                 "Cloisons plaque de pl�tre","CLOISONS_DOUBLAGES","CLOISONS DOUBLAGES",
                                 "Cloisons Amovibles","4-Cloisons Amovible","Cloison","CLOISONS / DOUBLAGES",
                                 "4-Cloison Doublage","A5-CDL - CLOISONS - DOUBLAGES","Cloison Doublage","Rev�tement de sol et murs","4-Faux Plafond BA13",
                                 "Cloisons de laboratoires","Faux Plafonds","Cloisons+Doublages+Plafonds suspendus","LOT 11 - FAUX PLAFOND","Plafond",
                                 "PLATERIE -FAUX PLAFONDS","06 - Cloisons - Doublages - Faux-Plafond","CLOISON - PLATRERIE","Plafonds ","Cloisons pl�trerie")] <- "PLATERERIE"

dfnew$Metier[dfnew$Metier %in% c("Facades","REVETEMENTS DE FACADE","Rev�tements de fa�ades","REVETEMENTS DE FACADES"
                                 ,"REVETEMENTS DE FACADES","TRAITEMENT DE FACADE","Facade","FACADE","Fa�ades","FACADES","FACADE - BARDAGE",
                                 "Rev�tements de facade+isolation+bardages","3-Fa�ades respirantes","REVETEMENT_FA�ADE","03 - Menuiseries ext�rieures - Fa�ades")] <- "REVETEMENT FA�ADE"

dfnew$Metier[dfnew$Metier %in% c("Carrelage & Sol souple","Rev�tement de sols souples","CARRELAGE FAIENCE","A8-RSD - REV�TEMENTS DE SOL ET MURS DURS",
                                 "Sols Durs et Minces","Sols durs","SOLS FAIENCE PARQUET","Fa�ence","Rev�tement de sol","CARRELAGE",
                                 "SOL (hors carrelage)","Rev�tements de sols souples","SOLS DURS","SOLS SOUPLES","Carrelage",
                                 "8/9-Sols durs / souples","Sols souple","Sols Souples","Revetement de sols","Sol souple",
                                 "REVETEMENTS SOLS","CARRELAGE / SOLS SOUPLES","CARRELAGE_FAIENCE","A10-RSS - REV�TEMENTS DE SOLS SOUPLES",
                                 "B1.9-RSD - REVETEMENTS DE SOL ET MURS DURS","8-Sols Durs","REVETEMENTS DE SOL","LOT 7 - REV�TEMENTS SOLS",
                                 "Rev�tements de sols","B1.9A-RSD - RESINE DE SOL","SOLS COULES","Rev�tements de sols","Rev�tements de sols ",
                                 "Rev�tments de sols durs","Carrelage Faience","REV�TEMENT DE SOLS SOUPLES","Sols Souples - Sols durs","Sols PVC",
                                 "LOT 12 - REVETEMENT DE SOLS SOUPLES ET DURS","Rev�tement de sol souple")] <- "REVETEMENT SOL"

dfnew$Metier[dfnew$Metier %in% c("Nettoyage","NETTOYAGE")] <- "NETTOYAGE"
dfnew$Metier[dfnew$Metier %in% c("TUYAUTERIE","Tuyauterie","Tuyauteries CREO","Tuyauteur","Tuyauteries TEAM FLUIDES","Tuyauteries Doncieux")] <- "TUYAUTERIE"
dfnew$Metier[dfnew$Metier %in% c("DEMOLITION")] <- "DEMOLITION"
dfnew$Metier[dfnew$Metier %in% c("RENOVATION")] <- "RENOVATION"
dfnew$Metier[dfnew$Metier %in% c("Charpente m�tallique","CHARPENTE")] <- "CHARPENTE"
dfnew$Metier[dfnew$Metier %in% c("Etat des lieux")] <- "ETAT DES LIEUX"
dfnew$Metier[dfnew$Metier %in% c("Terrassement - GO")] <- "TERASSEMENT"
dfnew$Metier <- factor(dfnew$Metier)
summary(dfnew$Metier)


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

#eleminer les doublons de la base de données
#connaitre le nombre de doublons 
length(dfnew$Id)
length(duplicated(dfnew$Id))

dfnew<-dfnew[-which(duplicated(dfnew$Id)),]

#le compte est bon 
nrow(dfnew)
#exportation de la base de donn�ees 
write.csv(dfnew,file="C:/Users/Soumia/Desktop/Données/dfnewExport.csv",row.names = FALSE )
#test
#t <- read.csv("c:/Users/Soumia/Desktop/Données/dfnewExport.csv",header = TRUE, sep =',')

#verifier les modalités des variables categorielles
levels(dfnew$CategorieReserve)
levels(dfnew$ReserbeBloquante)
levels(dfnew$LeveeReserve)

#Mise en place de quelques visualisations 
#test de dependance
data <- c(10,11,13,14)
#Hyp testing pour la prob que notre hypothese est vrai
chisq.test(data)
#X-squared = 0.83333, df = 3, p-value = 0.8415
#valeur critique d'apres la table; p-value sup a 0.05 dc on accepte H0


#quelques calculs sur des variables categorielles 
#1
library(ggplot2)
library(ggmosaic)

ggplot(dfnew)+
  geom_mosaic(aes(x=product(LeveeReserve,ReserbeBloquante), fill=LeveeReserve))+
  ylab("leveeRSV")+
  xlab("Bloquante")+
  ggtitle("Répartition des données en fonction du leveeReserve de si elles sont bloquantes ou pas")

#sur celles non bloquantes on a plus de reserve levée que de non levée
#par contre pour le bloquantes on a plus de non levée que de levée

#2
ggplot(dfnew)+
  geom_mosaic(aes(x=product(LeveeReserve,CategorieReserve), fill=LeveeReserve), offset = 0.05)+
  ylab("LeveeReserve")+
  xlab("Categorie_RSV")+
  ggtitle("Répartition des données en fonction des Categories")+
  scale_fill_manual(values=c("#EE00EE", "#636363"))+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))

