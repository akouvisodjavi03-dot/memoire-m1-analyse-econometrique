

################ Mémoire M1 : Accès aux antipaludéens et aux moustiquaires imprégnés ###############


### Librairie 

library(haven)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(formattable)
library(kableExtra)
library(tidyr)
library(knitr)
library(openxlsx)
library(scales)
library(tidyverse)
library(vcd)
library(rcompanion)
library(reshape2)
library(MASS)
library(stargazer)



setwd("C:/Users/PC/Documents/MES COURS/COURS M1/Mémoire M1_sodjavi/data")



#_________________________________1 - Importation des bases de données_____________________________#

## Gabon
# Enfant 
GAB <- read_dta("GAKR71FL.DTA")
# Ménage
GABM <- read_dta("GAHR71FL.DTA")


## Madagascar 
# Enfant
MADA <- read_dta("MDKR81FL.DTA")
# Ménage
MADAM <- read_dta("MDHR81FL.DTA")


## Rwanda
# Enfant
RW <- read_dta("RWKR81FL.DTA")
# Ménage
RWM <- read_dta("RWHR81FL.DTA")


## Sénégal
# Enfant
SN <- read_dta("SNKR8SFL.DTA")
# Ménage
SNM <- read_dta("SNHR8SFL.DTA")



#====================================== Variables d'intérêt ===========================================

menage <- c('hv000', 'hv001','hv002', 'hv025','hv219','hv220', 'hv270a','hv228')

mere_enfant <- c('bidx' ,'b5','v000', 'v001', 'v002', 'v012','v106', 'v130','v157','v158','v159', 'v169a','v171a','v481','v467b','v467c','v467d','v467e','v467f','v743a','m14','v714')


#==================================== Consolidation des données =======================================

#_________________________Gabon

# mere_enfant
GAB_mev <- subset(GAB, select = mere_enfant)
names(GAB_mev)

# Selection des variables ménages 
GABMv  <- subset(GABM, select = menage)
names(GABMv)


# Voir les lignes complètement dupliquées
duplicated_rows <- GABMv[duplicated(GABMv), ]
# Afficher les doublons
print(nrow(duplicated_rows)) #0 doublons


# Rénommer des colonnes  de ménage  pour la fusion des deux dataframe
GABMv <- GABMv %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseGAB <- inner_join(GAB_mev , GABMv, by = join_by('v001', 'v002'))


#=========== Construction de l'indicateur d'accès à l'information de la mère ============#

varACM_G <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM_G <- subset(BaseGAB, select =varACM_G )
colSums(is.na(ACM_G))

# conversion des colonnes de ACM_G en facteur puis de ACM_G en data frame

ACM_G <- sapply(ACM_G, factor)
ACM_G <- data.frame(ACM_G)

# Transformation de variables de ACM_G en variables binaire

table(ACM_G$v157)

ACM_G <- ACM_G %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM_G$v158)
ACM_G <- ACM_G %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'),
          v159 = ifelse(v159 %in% c("1", "2"), '1', '0'),
          v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Réaliser l'ACM
res_ACM_G <- MCA(ACM_G, quanti.sup = NULL, quali.sup = NULL, graph = TRUE)
summary(res_ACM_G)
res_ACM_G$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord<- round(res_ACM_G$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseGAB$acces_infos_mere <- ind_coord

## Filtrer la variable hv228 pour ne retenir que les ménages ayant des enfants vivants

BaseGAB <- BaseGAB %>%filter(b5 == 1, hv228 %in% c(0, 1, 2, 3))

# Regroupement de modalités de hv228 (variable dépendante)

freq_table <- table(BaseGAB$hv228)
# Bar plot de la variable qualitative
barplot(freq_table,
        main = "Nombre d'enfant ayant dormi sous moustiquaire",
        xlab = "Modalités",
        ylab = "Fréquence",
        col = "lightblue",
        border = "darkblue")

BaseGAB <- BaseGAB %>%
  mutate(hv228 = as.numeric(as.character(hv228))) %>%  # convertir en numérique
  mutate(hv228 = case_when(
    hv228 == 0 ~ 0,
    hv228 %in% c(1, 2) ~ 1,
    hv228 == 3 ~ 2,
    TRUE ~ NA_real_
  ))

#Recodages et Regroupement de modalités des autres variables

# Indicateur d'autonomie de la femme : la Répondante travaille actuellement (0=Non , 1=Oui)

table(BaseGAB$v714); sum(is.na(BaseGAB$v714)) 
 
# Nombre de consultation prénatale : inférieur ou égale à 3 = 0 ou 98 Don't know= 0
table(BaseGAB$m14)
BaseGAB <- BaseGAB %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# lieu de résidence 
table(BaseGAB$hv025)
BaseGAB <- BaseGAB %>%
  mutate(hv025= ifelse(hv025 == 1, 1, 0))

# Sex du cheh du ménage
table(BaseGAB$hv219)
BaseGAB <- BaseGAB %>%
  mutate(hv219= ifelse(hv219 == 1, 1, 0))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Supérieur 
table(BaseGAB$v106)

# Problème pour Obtenir la permission pour  se faire soignée 1 = Gros problème , 2 = Pas un gros problème ( même codage pour les autres variables v467)
table(BaseGAB$v467b)

# Problème: Nécessité de prendre un moyen de transport (NA)

# Dernier mot en famille sur les Soins de santé pour les femmes vivants en couple
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes célibataires

table(BaseGAB$v743a); sum(is.na((BaseGAB$v743a)))    # 1955 NA

BaseGAB <- BaseGAB %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# construction de la variable identification(id)
# BaseGAB$Id <- paste(BaseGAB[['v000']], BaseGAB[['v001']], BaseGAB[['v002']], sep = "-")

# Exportion de la base en CSV
# write.csv(BaseGAB, 'BaseGAB.csv', row.names = FALSE)


#______________________________________Madagascar 

# mere_enfant
MADA_mev <- subset(MADA, select = mere_enfant)
names(MADA_mev)

# Selection des variables ménages 
MADAMv  <- subset(MADAM, select = menage)
names(MADAMv)


# Rénommer des colonnes  de ménage  pour la fusion des deux dataframe
MADAMv <- MADAMv %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseMADA <- inner_join(MADA_mev , MADAMv, by = join_by('v001', 'v002'))

#====== Construction de l'indicateur d'accès à l'information de la mère ============#

varACM_M <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM_M <- subset(BaseMADA, select =varACM_M )
colSums(is.na(ACM_M))

# conversion des colonnes de ACM_M en facteur puis de ACM_M en data frame

ACM_M <- sapply(ACM_M, factor)
ACM_M <- data.frame(ACM_M)

# Transformation de variables de ACM_M en variables binaire

table(ACM_M$v157)
ACM_M <- ACM_M %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM_M$v158)
ACM_M <- ACM_M %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'),
         v159 = ifelse(v159 %in% c("1", "2"), '1', '0'),
         v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Réaliser l'ACM
res_ACM_M <- MCA(ACM_M, quanti.sup = NULL, quali.sup = NULL, graph = FALSE)
summary(res_ACM_M)
res_ACM_M$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord_M<- round(res_ACM_M$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseMADA$acces_infos_mere <- ind_coord_M

## Filtrer la variable hv228 pour le retenir que les ménages ayant des enfants vivants

BaseMADA <- BaseMADA %>%filter(b5 == 1, hv228 %in% c(0, 1, 2, 3))

# Recodage de hv228 (variable dépendante)

BaseMADA <- BaseMADA %>%
  mutate(hv228 = as.numeric(as.character(hv228))) %>%  # convertir en numérique
  mutate(hv228 = case_when(
    hv228 == 0 ~ 0,
    hv228 %in% c(1, 2) ~ 1,
    hv228 == 3 ~ 2,
    TRUE ~ NA_real_
  ))

# Recodage des autres variables

# Indicateur d'autonomie de la femme : la Répondante travaille actuellement (0=Non , 1=Oui)

table(BaseMADA$v714); sum(is.na(BaseMADA$v714)) 

# Nombre de consultation prénatale : inférieur ou égale à 3 = 0 ou 98 Don't know= 0
table(BaseMADA$m14)
BaseMADA <- BaseMADA %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# lieu de résidence 
table(BaseMADA$hv025)
BaseMADA <- BaseMADA %>%
  mutate(hv025= ifelse(hv025 == 1, 1, 0))

# Sex du chef du ménage
table(BaseMADA$hv219)
BaseMADA <- BaseMADA %>%
  mutate(hv219= ifelse(hv219 == 1, 1, 0))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Supérieur 
table(BaseMADA$v106)

# Problème pour Obtenir la permission pour  se faire soignée 1 = Gros problème , 2 = Pas un gros problème ( même codage pour les autres variables v467)
table(BaseMADA$v467b)

# Problème: Nécessité de prendre un moyen de transport v467e (NA)

# Dernier mot en famille sur les Soins de santé pour les femmes vivants en couple
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes célibataires

table(BaseMADA$v743a); sum(is.na((BaseMADA$v743a)))    # 2216 NA

BaseMADA <- BaseMADA %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# construction de la variable identification(id)
BaseMADA$Id <- paste(BaseMADA[['v000']], BaseMADA[['v001']], BaseMADA[['v002']], sep = "-")

# Exportion de la base en CSV
write.csv(BaseMADA, 'BaseMADA.csv', row.names = FALSE)


#_________________________________________Rwanda

# mere_enfant
RW_mev <- subset(RW, select = mere_enfant)
names(RW_mev)

# Selection des variables ménages 
RWMv  <- subset(RWM, select = menage)
names(RWMv)


# Rénommer des colonnes  de ménage  pour la fusion des deux dataframe
RWMv <- RWMv %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseRW <- inner_join(RW_mev , RWMv, by = join_by('v001', 'v002'))

#====== Construction de l'indicateur d'accès à l'information de la mère ============#

varACM_R <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM_R <- subset(BaseRW, select =varACM_R )
colSums(is.na(ACM_R))

# conversion des colonnes de ACM_R en facteur puis de ACM_R en data frame

ACM_R <- sapply(ACM_R, factor)
ACM_R <- data.frame(ACM_R)

# Transformation de variables de ACM_R en variables binaire

table(ACM_R$v157)
ACM_R <- ACM_R %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM_R$v158)
ACM_R <- ACM_R %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'),
         v159 = ifelse(v159 %in% c("1", "2"), '1', '0'),
         v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Réaliser l'ACM
res_ACM_R <- MCA(ACM_R, quanti.sup = NULL, quali.sup = NULL, graph = FALSE)
summary(res_ACM_R)
res_ACM_R$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord_R<- round(res_ACM_R$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseRW$acces_infos_mere <- ind_coord_R

## Filtrer la variable hv228 pour le retenir que les ménages ayant des enfants vivants

BaseRW <- BaseRW %>%filter(b5 == 1, hv228 %in% c(0, 1, 2, 3))

# Recodage de hv228 (variable dépendante)

BaseRW <- BaseRW %>%
  mutate(hv228 = as.numeric(as.character(hv228))) %>%  # convertir en numérique
  mutate(hv228 = case_when(
    hv228 == 0 ~ 0,
    hv228 %in% c(1, 2) ~ 1,
    hv228 == 3 ~ 2,
    TRUE ~ NA_real_
  ))

# Recodage des autres variables

# Indicateur d'autonomie de la femme : la Répondante travaille actuellement (0=Non , 1=Oui)

table(BaseRW$v714); sum(is.na(BaseRW$v714)) 

# Nombre de consultation prénatale : inférieur ou égale à 3 = 0 ou 98 Don't know= 0
table(BaseRW$m14)
BaseRW <- BaseRW %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# lieu de résidence 
table(BaseRW$hv025)
BaseRW <- BaseRW %>%
  mutate(hv025= ifelse(hv025 == 1, 1, 0))

# Sex du chef du ménage
table(BaseRW$hv219)
BaseRW <- BaseRW %>%
  mutate(hv219= ifelse(hv219 == 1, 1, 0))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Supérieur 
table(BaseRW$v106)

# Problème pour Obtenir la permission pour  se faire soignée 1 = Gros problème , 2 = Pas un gros problème ( même codage pour les autres variables v467)
table(BaseRW$v467b)

# Problème: Nécessité de prendre un moyen de transport v467e (NA)

# Dernier mot en famille sur les Soins de santé pour les femmes vivants en couple
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes célibataires

table(BaseRW$v743a); sum(is.na((BaseRW$v743a)))    # 1243 NA

BaseRW <- BaseRW %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# construction de la variable identification(id)
BaseRW$Id <- paste(BaseRW[['v000']], BaseRW[['v001']], BaseRW[['v002']], sep = "-")

# Exportion de la base en CSV
write.csv(BaseRW, 'BaseRW.csv', row.names = FALSE)


#_____________________________________Sénégal


# mere_enfant
SN_mev <- subset(SN, select = mere_enfant)
names(SN_mev)

# Selection des variables ménages 
SNMv  <- subset(SNM, select = menage)
names(SNMv)


# Rénommer des colonnes  de ménage  pour la fusion des deux dataframe
SNMv <- SNMv %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseSN <- inner_join(SN_mev , SNMv, by = join_by('v001', 'v002'))

#====== Construction de l'indicateur d'accès à l'information de la mère ============#

varACM_S <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM_S <- subset(BaseSN, select =varACM_S )
colSums(is.na(ACM_S))

# conversion des colonnes de ACM_S en facteur puis de ACM_S en data frame

ACM_S <- sapply(ACM_S, factor)
ACM_S <- data.frame(ACM_S)

# Transformation de variables de ACM_S en variables binaire

table(ACM_S$v157)
ACM_S <- ACM_S %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM_S$v158)
ACM_S <- ACM_S %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'),
         v159 = ifelse(v159 %in% c("1", "2"), '1', '0'),
         v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Réaliser l'ACM
res_ACM_S <- MCA(ACM_S, quanti.sup = NULL, quali.sup = NULL, graph = FALSE)
summary(res_ACM_S)
res_ACM_S$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord_S<- round(res_ACM_S$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseSN$acces_infos_mere <- ind_coord_S

## Filtrer la variable hv228 pour le retenir que les ménages ayant des enfants vivants

BaseSN <- BaseSN %>%filter(b5 == 1, hv228 %in% c(0, 1, 2, 3))

# Recodage de hv228 (variable dépendante) 0 = aucun, 1= Certains, 2 = tous

BaseSN <- BaseSN %>%
  mutate(hv228 = as.numeric(as.character(hv228))) %>%  # convertir en numérique
  mutate(hv228 = case_when(
    hv228 == 0 ~ 0,
    hv228 %in% c(1, 2) ~ 1,
    hv228 == 3 ~ 2,
    TRUE ~ NA_real_
  ))

# Recodage des autres variables

# Indicateur d'autonomie de la femme : la Répondante travaille actuellement (0=Non , 1=Oui)

table(BaseSN$v714); sum(is.na(BaseSN$v714)) 

# Nombre de consultation prénatale : inférieur ou égale à 3 = 0 ou 98 Don't know= 0
table(BaseSN$m14)
BaseSN <- BaseSN %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# lieu de résidence 
table(BaseSN$hv025)
BaseSN <- BaseSN %>%
  mutate(hv025= ifelse(hv025 == 1, 1, 0))

# Sex du chef du ménage
table(BaseSN$hv219)
BaseSN <- BaseSN %>%
  mutate(hv219= ifelse(hv219 == 1, 1, 0))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Supérieur 
table(BaseSN$v106)

# Problème pour Obtenir la permission pour  se faire soignée 1 = Gros problème , 2 = Pas un gros problème ( même codage pour les autres variables v467)
table(BaseSN$v467b)

# Problème: Nécessité de prendre un moyen de transport v467e (NA)

# Dernier mot en famille sur les Soins de santé pour les femmes vivants en couple
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes célibataires

table(BaseSN$v743a); sum(is.na((BaseSN$v743a)))    # 510 NA

BaseSN <- BaseSN %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# construction de la variable identification(id)
BaseSN$Id <- paste(BaseSN[['v000']], BaseSN[['v001']], BaseSN[['v002']], sep = "-")

# Exportion de la base en CSV
write.csv(BaseSN, 'BaseSN.csv', row.names = FALSE)


#_______________________________ 2- Jointure des bases________________________________#

BaseGAB <- subset(BaseGAB, select = -c(v130,v467e))
BaseMADA <- subset(BaseMADA, select = -c(v130,v467e))
BaseRW <- subset(BaseRW, select = -c(v130,v467e))
BaseSN <- subset(BaseSN, select = -c(v130,v467e))

BaseC <- bind_rows(BaseGAB, BaseMADA, BaseRW, BaseSN)

BaseC <- BaseC %>% 
  rename('age_mere'='v012', 'educ_mere'='v106', 'Couverture_santé'='v481', 
         'pbm_permission'='v467b', 'pbm_arg_trait'='v467c', 'pbm_distance'='v467d',
         'pbm_accompagnant'='v467f', 'dernier_mot_soins'= 'v743a', 'nbr_visite_pren'='m14', 
         'travail_mere'='v714','residence'='hv025', 'sex_chef_m'='hv219', 
         'age_chef_m'='hv220', 'indice_richesse'='hv270a', 'enf_drt_moustq'='hv228','pays'= 'v000')
  

BaseC <- subset(BaseC, select = -c(v157,v158,v159,v169a,v171a))


#_________________________ Valeurs manquantes

# Initialiser un vecteur pour stocker les résultats
taux_na <- data.frame(
  variable = character(),
  taux_na_pct = numeric(),
  nb_na = integer(),
  stringsAsFactors = FALSE
)

# Boucle sur les colonnes
for (col in colnames(BaseC)) {
  nb_na <- sum(is.na(BaseC[[col]]))
  taux_pct <- 100 * nb_na / length(BaseC[[col]])
  
  taux_na <- rbind(taux_na, data.frame(
    variable = col,
    nb_na = nb_na,
    taux_na_pct = taux_pct,
    stringsAsFactors = FALSE
  ))
}

# Affichage trié par ordre décroissant de taux de NA
taux_na <- taux_na[order(-taux_na$taux_na_pct), ]

# Résultat
print(taux_na, row.names = FALSE)
taux_na = data.frame(taux_na)


# Surpression de variables filtre 
BaseC <- subset(BaseC, select = -c( b5, bidx, v001, v002, hv000))

# Exporter le dataframe en fichier CSV
write.csv(BaseC, "DataC.csv", row.names = FALSE)

# Importation de la base Finale
data<- read.csv("DataC.csv")

# Filtrage de la base pour ne garder que les individus de 20 ans et plus 
data <- data %>%
  filter(age_chef_m >= 20, age_chef_m <= 90)

data <- data %>%
  filter(age_mere >= 20)

# Renommer les modalités de la colonne pays

data <- data %>%
  mutate(pays = case_when(
    pays == "GA7" ~ "GABON",
    pays == "MD7" ~ "MADAGASCAR",
    pays == "RW7" ~ "RWANDA",
    pays == "SN8" ~ "SENEGAL",
    TRUE ~ pays  
  ))

# Vérification des NA non applicable

for(col in colnames(data)){
  if (sum(is.na(data[[col]])) > 0) {
    cat(sprintf("la colonne %s contient %4.1f%% de valeur manquante \n", col, (sum(is.na(data[[col]])) / length(data[[col]]) * 100)))
  } else {
    cat(sprintf("la colonne %s ne contient pas de valeur manquantes\n", col))
  }
}

# Supposons que ta colonne s'appelle "ma_colonne"
col_type <- class(data$dernier_mot_soins)

cat("Type de la colonne :", col_type, "\n")

# Remplacer les NA par 3 uniquement si la colonne est numérique
if (col_type %in% c("numeric", "integer")) {
  data$dernier_mot_soins[is.na(data$dernier_mot_soins)] <- 3
  cat("Les NA ont été remplacés par 3.\n")
} else {
  cat("La colonne n'est pas numérique. Aucun remplacement effectué.\n")
}

## Calcul de la prévalence d'accès au moustiquaire

# Effectif par pays

Eff <- data %>% 
  group_by(pays) %>%
  summarise(effectifs= n())

# Création d'une nouvelle variable pour le calcul de la prévalence

data$enf_drt_moustq_prev <- ifelse(data$enf_drt_moustq %in% c(1, 2), 1, 0)

# Définition de la fonction pour calculer la prévalence et les intervalles de confiance

calc_prevalence_ci <- function(x) {
  result <- prop.test(sum(x == 1), length(x))
  prevalence <- result$estimate
  conf_int <- result$conf.int
  # Formatage de l'intervalle de confiance en une seule colonne
  ci_string <- paste0(format(round(conf_int[1], 2), nsmall = 2), " - ", format(round(conf_int[2], 2), nsmall = 2))
  return(data.frame(Prevalence_acces = prevalence, CI = ci_string))
}

# calcul de la prévalence

prev_acces_moustq <- calc_prevalence_ci(data$enf_drt_moustq_prev)

# Tableau pour afficher les résultats 

prev_acces_moustq %>%
  mutate(Prevalence_acces = scales::percent(Prevalence_acces, accuracy = 0.1),
         CI = CI) %>%
  kbl(caption = "Prévalence de l'utilisation (accès) des moustiquaires par les enfants de moins de 5 ans ") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Prévalence Globale" = 1, "Intervalle de Confiance" = 1)) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "15em") 


# Calcul de la prévalence par pays avec intervalles de confiance
prevalence_acc_pays <- data %>%
  group_by(pays) %>%
  group_modify(~ calc_prevalence_ci(.x$enf_drt_moustq_prev)) %>%
  ungroup() 
 

# Tableau pour afficher les résultats 

prevalence_acc_pays %>%
  mutate(Prevalence_acces = scales::percent(Prevalence_acces, accuracy = 0.1),
         CI = CI) %>%
  rename(pays=pays)%>%
  kbl(caption = "Prévalence de l'utilisation (accès) des moustiquaires par les enfants de moins de 5 ans par pays ") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Prévalence par pays" = 1, "Intervalle de Confiance" = 1)) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "5em")%>%
  column_spec(3, width = "10em")


# Choix des variables explicatives qualitatives

# Liste des variables à analyser
variables <- c("age_mere"   ,         "educ_mere" ,         
               "Couverture_santé" ,   "pbm_permission"  ,    "pbm_arg_trait" ,     
                "pbm_distance"     ,   "pbm_accompagnant" ,   "dernier_mot_soins",  
                "nbr_visite_pren"  ,   "travail_mere"  ,      "residence" ,         
                "sex_chef_m"   ,       "age_chef_m"   ,       "indice_richesse", "pays")

# Fonction pour effectuer l'analyse d'une variable

# Fonction pour calculer la V de Cramér
cramers_v <- function(tab) {
  chi2 <- suppressWarnings(chisq.test(tab)$statistic)
  n <- sum(tab)
  k <- min(nrow(tab), ncol(tab))
  v <- sqrt(chi2 / (n * (k - 1)))
  return(as.numeric(v))
}

# Fonction pour analyser une variable
analyze_variable <- function(var_name) {
  tab <- table(data[[var_name]], data$enf_drt_moustq)
  test <- chisq.test(tab)
  v_cramer <- cramers_v(tab)
  
  list(
    table = tab,
    proportions = prop.table(tab, margin = 1),
    test = test,
    v_cramer = v_cramer
  )
}

# Application à toutes les variables
results <- map(setNames(variables, variables), analyze_variable)

# Création du tableau récapitulatif
summary_table <- map_dfr(results, ~{
  tibble(
    Chi_square_statistic = .x$test$statistic,
    DDL = .x$test$parameter,
    P_value = .x$test$p.value,
    Cramer_V = round(.x$v_cramer, 3),
    Significant = ifelse(.x$test$p.value < 0.05, "*", "")
  )
}, .id = "Variable") %>%
  mutate(Variable = c(
    "age de la mere", "education de la mere", "Couverture santé",
    "problème pour obtenir la permission", "problème pour argent traitement",
    "problème distance établissement de soin", "problème trouver l'accompagnant",
    "dernier mot décision soins", "nombre de visite prenatale", "travail de la mere",
    "lieu de residence", "sex chef menage", "age chef menage", "indice de richesse", "pays"
  ))

# Affichage du tableau avec kableExtra
summary_table %>%
  kbl(caption = "Résultats des tests de Chi² et V de Cramér pour les variables liées à l'accès aux moustiquaires",
      digits = 3) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(5, bold = TRUE, color = "white",
              background = ifelse(summary_table$Significant == "*", "green", "red")) %>%
  column_spec(4, bold = TRUE)


# test de corrélation 
cor_acces_infos_mere <- cor.test(data$acces_infos_mere, data$enf_drt_moustq)


# Enfant dormant sous moustiquaire et âge de la mère

# Boxplot

ggplot(data, aes(x = factor(enf_drt_moustq), y = age_mere, fill = factor(enf_drt_moustq))) +
  geom_boxplot() +
  labs(
    title = "Évolution de la variable dépendante en fonction de l'âge de la mère",
    x = "Accès à la moustiquaire (0 = Non, 1 = Oui, 2 = NSP)",
    y = "Âge de la mère"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "Non accès", "1" = "Accès", "2" = "Ne sait pas")) +
  guides(fill = "none")  # Pour supprimer la légende

   
  ## Barplot
  
  ggplot(data, aes(x =age_mere , fill = enf_drt_moustq)) +
    geom_bar(position = "dodge") +  # "dodge" = barres côte à côte
    labs(
      title = "Évolution de la variable dépendante en fonction de l'age de la mère",
      x = "age_mere",
      y = "enf_drt_moustq",
      fill = "Effectif"
    ) +
    theme_minimal()
  
  
## Enfant dormant sous moustiquaire et accès de la mère à l'information 

  ggplot(data, aes(x  = factor(enf_drt_moustq)  , y = acces_infos_mere, fill = factor(enf_drt_moustq))) +
    geom_boxplot()      # boxplot
  labs(
    title = "Évolution de la variable dépendante en fonction del'accès à l'information de la mère",
    x = "enf_drt_moustq (x)",
    y = "acces_infos_mere (y)"
  ) +
    theme_minimal() +
    scale_x_discrete(labels = c("Non accès", "Accès")) +
    guides(fill = none)

## Enfant dormant sous moustiquaire et age du chef de ménage 
  
  ggplot(data, aes(x  = factor(enf_drt_moustq)  , y = age_chef_m, fill = factor(enf_drt_moustq))) +
    geom_boxplot()      # boxplot
  labs(
    title = "Évolution de la variable dépendante en fonction de l'âge du chef de ménage mère",
    x = "enf_drt_moustq (x)",
    y = "age_chef_m (y)"
  ) +
    theme_minimal() +
    scale_x_discrete(labels = c("Non accès", "Accès")) +
    guides(fill = none)

  
## Régression logistique
  
  ## Conversion des variables catégorielles en facteurs
  
  data <- data %>% 
    mutate(
      enf_drt_moustq = as.numeric(enf_drt_moustq),
      pays = as.factor(pays),
      educ_mere = as.factor(educ_mere),
      Couverture_santé = as.factor(Couverture_santé),
      pbm_permission = as.factor(pbm_permission),
      pbm_arg_trait = as.factor( pbm_arg_trait ),
      pbm_distance = as.factor(pbm_distance),
      pbm_accompagnant = as.factor(pbm_accompagnant),
      dernier_mot_soins = as.factor(dernier_mot_soins),
      nbr_visite_pren = as.factor(nbr_visite_pren),
      travail_mere = as.factor(travail_mere),
      residence = as.factor(residence),
      sex_chef_m = as.factor(sex_chef_m),
      indice_richesse = as.factor(indice_richesse),
      acces_infos_mere = as.numeric(acces_infos_mere),
      Id = as.factor(Id)
    )

  ## test d'association des variables 
  
# Sous-ensemble du dataframe
data_quali <- data[, 1:15]

# Facultatif : transformer toutes les variables en facteurs si nécessaire
data_quali <- data.frame(lapply(data_quali, as.factor))

# Fonction pour calculer le V de Cramer entre toutes les paires
cramer_matrix <- function(df) {
  vars <- names(df)
  n <- length(vars)
  result <- matrix(NA, n, n, dimnames = list(vars, vars))
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        result[i, j] <- 1
      } else {
        tbl <- table(df[[i]], df[[j]])
        result[i, j] <- assocstats(tbl)$cramer
      }
    }
  }
  return(result)
}

# Calcul de la matrice
v_cramer_mat <- cramer_matrix(data_quali)

# Mise en forme pour ggplot2
v_cramer_df <- melt(v_cramer_mat, na.rm = TRUE)
colnames(v_cramer_df) <- c("Var1", "Var2", "V")

# Création de la heatmap
ggplot(v_cramer_df, aes(x = Var1, y = Var2, fill = V)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "orange", high = "red",
                       midpoint = 0.5, limits = c(0, 1),
                       name = "V de Cramer") +
  geom_text(aes(label = round(V, 2)), size = 3) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Matrice du V de Cramer entre variables catégorielles")


## ==============Régression logistique multinomiale * sans traitement de Na==============


  ## Modèle 1 : niveau d'accès fonction des déterminants socio-démographique et économique 
  ## enf_drt_moustq = pays + age_chef_m + age_mere + educ_mere + pbm_arg_trait + pbm_distance + residence +  indice_richesse + sex_chef_m

## recodage de la variable enf_drt_moustq en facteur

data$enf_drt_moustq <- factor(data$enf_drt_moustq,
                              ordered = TRUE,
                              levels = c("0", "1", "2"))



# Regression logit globale
modele1_logit_global <- polr(enf_drt_moustq ~
                        pays + age_chef_m + age_mere + educ_mere 
                      + pbm_arg_trait + pbm_distance + residence 
                      + indice_richesse + sex_chef_m+ Couverture_santé + 
                        pbm_permission + pbm_accompagnant 
                      + dernier_mot_soins + nbr_visite_pren + travail_mere 
                      + acces_infos_mere, data = data, method = "logistic")



# Régression logit multinomiale

modele1_logit <- polr(enf_drt_moustq ~
                            pays + age_chef_m + age_mere + educ_mere 
                          + pbm_arg_trait + pbm_distance + residence 
                          + indice_richesse + sex_chef_m, data = data, method = "logistic")


modele1_logit_2 <- polr(enf_drt_moustq ~
                          pays + age_chef_m + I(age_chef_m^2) +
                          age_mere + I(age_mere^2) + educ_mere +
                          pbm_arg_trait + pbm_distance + residence +
                          indice_richesse + sex_chef_m,
                        data = data,
                        method = "logistic")
# Résumé des coefficients
summary(modele1_logit)

ctable <- coef(summary(modele1_logit))
p_values <- 2 * (1 - pnorm(abs(ctable[, "t value"])))
ctable <- cbind(ctable, "p value" = p_values)
ctable

stargazer(modele1_logit, type="text")


  
 ## Modèle 2 : niveau d'accès fonction des déterminants liés aux soins et autonomisation de la femme
## enf_drt_moustq = Couverture_santé + pbm_permission + pbm_accompagnant + dernier_mot_soins + nbr_visite_pren + travail_mere + acces_infos_mere


modele2_logit <- polr(enf_drt_moustq ~
                        Couverture_santé + pbm_permission + pbm_accompagnant 
                      + dernier_mot_soins + nbr_visite_pren + travail_mere 
                      + acces_infos_mere, data = data, method = "logistic")

# Résumé des coefficients
summary(modele2_logit)

ctable <- coef(summary(modele2_logit))
p_values <- 2 * (1 - pnorm(abs(ctable[, "t value"])))
ctable <- cbind(ctable, "p value" = p_values)
ctable

stargazer(modele2_logit, type="text")


# Exporter au format HTML
stargazer(modele2_logit, type = "html", out = "modele2_logit.html")



