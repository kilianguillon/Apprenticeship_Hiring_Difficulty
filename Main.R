'Library'

library("haven")
library("dplyr")
library("tidyr")
library("fixest")

#Data, downloading of the processed dataset

data <- read.csv("C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/data_final_2018_2020.csv")

#Construction of variables for the empirical study

#Turnover by firm by year
#Apprentices are excluded from the turnover to not have interference during the regression
data <- data %>%
  mutate(effectif_total = nb_apprentis + nb_CDI + nb_courts,
         turnover = replace_na((nb_entrées - nb_apprentis_debut + nb_sorties - nb_apprentis_fin)/(effectif_total-nb_apprentis),0))

#Share of no-CDI contracts
#Apprentices are excluded from the share to not have interference during the regression
data <- data %>%
  mutate(prop_nonpermanents = nb_courts / (nb_courts+nb_CDI))

#Dummy for the reform (2019)
data <- data %>%
  mutate(reform = ifelse(AN >= 2019, 1, 0))

#Recoding the region variable
get_region <- function(REGT){
  case_when(
    REGT ==11 ~ "Ile-de-France",
    REGT ==24 ~ "Centre-Val de Loire",
    REGT ==27 ~ "Bourgogne-Franche-Comté",
    REGT ==28 ~ "Normandie",
    REGT ==32 ~ "Hauts-de-France",
    REGT ==44 ~ "Grand Est",
    REGT ==52 ~ "Pays de la Loire",
    REGT ==53 ~ "Bretagne",
    REGT ==75 ~ "Nouvelle-Aquitaine",
    REGT ==76 ~ "Occitanie",
    REGT ==84 ~ "Auvergne-Rhône-Alpes",
    REGT ==93 ~ "Provence-Alpes-Côte d'Azur",
    REGT ==94 ~ "Corse",
    REGT ==01 ~ "Guadeloupe",
    REGT ==02 ~ "Martinique",
    REGT ==03 ~ "Guyane",
    REGT ==04 ~ "La Réunion",
    REGT ==06 ~ "Mayotte",
    TRUE ~ NA_character_
  )
}

data <- data%>%
  mutate(region = get_region(region))

#Recoding and aggregating the sector of activity variable (NAF rev 2 to NACE 17)
get_sectu <- function(sectu){
  case_when(
    sectu >= 01 & sectu <= 03 ~ "Agriculture, sylviculture et pêche",
    sectu >= 05 & sectu <= 09 ~ "Industries extractives",
    sectu >= 10 & sectu <= 12 ~ "Fabrication de denrées alimentaires, de boissons et de produit à base de tabac",
    sectu >= 13 & sectu <= 18 ~ "Fabrication d'autres produits industriels",
    sectu ==19 ~ "Cokéfaction et raffinage",
    sectu >= 20 & sectu <= 25 ~ "Fabrication d'autres produits industriels",
    sectu >= 26 & sectu <= 28 ~ "Fabrication d'équipements électriques",
    sectu >= 29 & sectu <= 30 ~ "Fabrication de matériels de transport",
    sectu >= 31 & sectu <= 33 ~ "Fabrication d'autres produits industriels",
    sectu >= 35 & sectu <= 39 ~ "Industries extractives",
    sectu >= 41 & sectu <= 43 ~ "Construction",
    sectu >= 45 & sectu <= 47 ~ "Commerce; réparation d'automobiles",
    sectu >= 49 & sectu <= 53 ~ "Transports et entreposage",
    sectu >= 55 & sectu <= 56 ~ "Hébergements et restauration",
    sectu >= 58 & sectu <= 63 ~ "Information et communication",
    sectu >= 64 & sectu <= 66 ~ "Activités financières et d'assurance",
    sectu ==68 ~ "Activités immobilières",
    sectu >= 69 & sectu <= 82 ~ "Activités scientifiques et techniques ; services administratifs et de soutien",
    sectu >= 84 & sectu <= 88 ~ "Administration publique, enseignement ; santé humaine et action sociale",
    sectu >= 90 & sectu <= 98 ~ "Autres activités de services",
    TRUE ~ NA_character_
  )
}

data <- data%>%
  mutate(secteur = get_sectu(secteur))

#Index using the region and the sector to assess the difficulty of hiring, using France Travail BMO data

df_regions <- tribble(
  ~region, ~taux_difficulte,
  "Corse", 55.7,
  "Martinique", 46.5,
  "Bretagne", 55.0,
  "Nouvelle-Aquitaine", 51.7,
  "Bourgogne-Franche-Comté", 51.1,
  "Auvergne-Rhône-Alpes", 54.4,
  "Pays de la Loire", 56.2,
  "Mayotte", 42.6,
  "Normandie", 49.9,
  "Provence-Alpes-Côte d'Azur",44.5,
  "Hauts-de-France", 49.0,
  "La Réunion", 32.6,
  "Occitanie", 45.0,
  "Ile-de-France", 47.3,
  "Guyane", 49.4,
  "Grand Est", 54.1,
  "Centre-Val de Loire", 58.2,
  "Guadeloupe", 43.4
)

df_secteurs <- tribble(
  ~secteur, ~taux_difficulte,
 "Agriculture, sylviculture et pêche", 43.5,
 "Industries extractives", 49.2,
 "Fabrication de denrées alimentaires, de boissons et de produit à base de tabac", 53.4,
 "Fabrication d'autres produits industriels", 62.4,
 "Cokéfaction et raffinage", 40.2,
 "Fabrication d'équipements électriques", 52.7,
 "Fabrication de matériels de transport", 46.1,
 "Construction", 68.4,
 "Commerce; réparation d'automobiles", 71.3,
 "Transports et entreposage", 62.2,
 "Hébergements et restauration", 53.5,
 "Information et communication", 51.4,
 "Activités financières et d'assurance", 37.4, 
 "Activités immobilières", 52.6,
 "Activités scientifiques et techniques ; services administratifs et de soutien", 53.8,
 "Administration publique, enseignement ; santé humaine et action sociale", 40.0,
 "Autres activités de services", 35.2)
  
#Score between 0 and 100
df_regions <- df_regions%>%
  mutate(score_region = taux_difficulte)

df_secteurs <- df_secteurs%>%
  mutate(score_secteur = taux_difficulte)

#Create an index of the mean of the two scores 
data <- data%>%
  left_join(df_regions %>% select(region, score_region), by="region")
  

data <-data %>%
  left_join(df_secteurs %>% select(secteur, score_secteur), by="secteur")%>%
  mutate(indice_difficulte = scale((score_region + score_secteur) /2))


#Creation of the target variables : the number of new apprentices (absolute),
#the percentage of the new apprentices in the total hiring of the firm(relative)
#0 when there is no hiring in this year
data <- data %>%
  mutate(prop_apprentis_debut = replace_na(100*nb_apprentis_debut / nb_entrées, 0))

#Same, but for the permanent contract hirings (CDI)
data <- data %>%
  mutate(prop_CDI_debut = replace_na(100*nb_CDI_debut / nb_entrées, 0))

#Same, but for the unpermanent contract hirings (CDD)
data <- data %>%
  mutate(prop_CDD_debut = replace_na(100*nb_CDD_debut / nb_entrées, 0))

#Put brut_moy at the log, it is now the log of the mean of the gross wage
data$brut_moy <- ifelse(data$brut_moy > 1, log(data$brut_moy),0)





"""Econometrics"""

#I test a lots of combinations of differents variables :
#Target variables : nb of new apprentices, share of new apprentices
#Main covariates (hiring difficulties) : turnover, share of unpermanent contracts, index of hiring
#difficulties, index of regional and sectorial hiring difficulties
#With or without the dummy of the reform, with or without year fixed effect
#But same framework for the work : linear model with firm fixed effects

#Share of new apprentices 
#Turnover
#Reform Dummy
modele_prop_apprentis <- feols(prop_apprentis_debut ~ reform + turnover + reform*turnover+ brut_moy + 
                              age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_prop_apprentis)

#Share of new apprentices 
#Share of unpermanent contracts
#Reform Dummy
modele_prop_apprentis <- feols(prop_apprentis_debut ~ reform + prop_nonpermanents + reform*prop_nonpermanents+ brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_prop_apprentis)

#Share of new apprentices
#Index of hiring difficulties
#Reform Dummy
modele_prop_apprentis <- feols(prop_apprentis_debut ~ reform + indice_difficulte + reform*indice_difficulte+ brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_prop_apprentis)

#Share of new apprentices
#Index of hiring difficulties (sector and region separated)
#Reform Dummy
modele_prop_apprentis <- feols(prop_apprentis_debut ~ reform + score_region +score_secteur + reform*score_region+ brut_moy +
                                  reform*score_secteur+age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_prop_apprentis)







#Number of new apprentices
#Turnover
#Reform Dummy
modele_nb_apprentis <- feols(nb_apprentis_debut ~ reform + turnover + reform*turnover+ brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_nb_apprentis)

#Number of new apprentices
#Share of unpermanent contracts
#Reform Dummy
modele_nb_apprentis <- feols(nb_apprentis_debut ~ reform + prop_nonpermanents + reform*prop_nonpermanents+ brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_nb_apprentis)

#Number of new apprentices
#Index of hiring difficulties
#Reform Dummy
modele_nb_apprentis <- feols(nb_apprentis_debut ~ reform + indice_difficulte + reform*indice_difficulte+ brut_moy +
                               age_moy_nonapprentis  + effectif_total | SIRET,
                             data = data)
summary(modele_nb_apprentis)

#Number of new apprentices
#Index of hiring difficulties (region and sector separated)
#Reform Dummy
modele_nb_apprentis <- feols(nb_apprentis_debut ~ reform + score_region +score_secteur + reform*score_region+ brut_moy +
                                 reform*score_secteur+age_moy_nonapprentis  + effectif_total | SIRET,
                               data = data)
summary(modele_nb_apprentis)






#Share of new apprentices
#Turnover
#Year fixed effect
modele_prop_apprentis <- feols(prop_apprentis_debut ~ turnover + turnover*reform + brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET+AN,
                               data = data)
summary(modele_prop_apprentis)

#Share of new apprentices
#Share of unpermanent contracts
#Year fixed effect
modele_prop_apprentis <- feols(prop_apprentis_debut ~ prop_nonpermanents + prop_nonpermanents*reform + brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET+AN,
                               data = data)
summary(modele_prop_apprentis)

#Number of new apprentices
#Turnover
#Year fixed effect
modele_nb_apprentis <- feols(nb_apprentis_debut ~  turnover + turnover*reform + brut_moy +
                               age_moy_nonapprentis  + effectif_total| SIRET,
                             data = data)
summary(modele_nb_apprentis)

#Share of new apprentices
#Index of hiring difficulties
#Year fixed effect
modele_prop_apprentis <- feols(prop_apprentis_debut ~ indice_difficulte + indice_difficulte*reform + brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET+AN,
                               data = data)
summary(modele_prop_apprentis)

#Number of new apprentices
##Index of hiring difficulties
#Year fixed effect
modele_nb_apprentis <- feols(nb_apprentis_debut ~  indice_difficulte + indice_difficulte*reform+  brut_moy +
                               age_moy_nonapprentis  + effectif_total| SIRET+AN,
                             data = data)
summary(modele_nb_apprentis)

#Share of new apprentices
#Index of hiring difficulties (region and sector separated)
#Year fixed effect
modele_prop_apprentis <- feols(prop_apprentis_debut ~ score_region + score_secteur+ score_region*reform
                               + score_secteur*reform + brut_moy +
                                 age_moy_nonapprentis  + effectif_total | SIRET+AN,
                               data = data)
summary(modele_prop_apprentis)

#Number of new apprentices
#Index of hiring difficulties (region and sector separated)
#Year fixed effect
modele_nb_apprentis <- feols(nb_apprentis_debut ~  score_region + score_secteur + score_region*reform
                             + score_secteur*reform  + brut_moy +
                               age_moy_nonapprentis  + effectif_total| SIRET,
                             data = data)
summary(modele_nb_apprentis)


#Econometrics with CDD et CDI (only two models)
#CDI
modele_prop_CDI <- feols(prop_CDI_debut ~ nb_apprentis_debut + nb_apprentis_debut*reform + brut_moy +
                         age_moy_nonapprentis  + effectif_total + indice_difficulte + indice_difficulte*reform | SIRET+AN,
                       data = data)
summary(modele_prop_CDI)

#CDD
modele_prop_CDD <- feols(prop_CDD_debut ~ nb_apprentis_debut + nb_apprentis_debut*reform + brut_moy +
                           age_moy_nonapprentis  + effectif_total + indice_difficulte + indice_difficulte*reform| SIRET+AN,
                         data = data)
summary(modele_prop_CDD)




#Descriptive Statistics

#Correlation matrix between turnover, indice_diff, score_region and score_secteur, prop_nonpermanents
corr_data <- data%>%
  select(turnover, indice_difficulte, score_region, score_secteur, prop_nonpermanents)

cor_matrix <- cor(corr_data, use = "complete.obs")
print(round(cor_matrix,2))


#Tables describing the evolution of apprenticeship
data%>%
  group_by(AN)%>%
  summarise(nb_deb_apprentis = sum(nb_apprentis_debut),
            nb_fin_apprentis = sum(nb_apprentis_fin),
            nb_apprentis = sum(nb_apprentis))%>%
  arrange(AN)

#Distribution of the indice_difficulte among firms
plot(density(data$indice_difficulte, na.rm =TRUE, adjust = 2.5),
  main = "Density of Hiring Difficulty Index",
  xlab = "Hiring Difficulty Index",
  ylab = "Density",
  col = "darkgreen",
  lwd = 3)


