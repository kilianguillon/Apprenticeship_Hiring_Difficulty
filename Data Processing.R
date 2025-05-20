'Library'

library("haven")
library("dplyr")
library("tidyr")

"Data : I take the vintage 2018, 2019 and 2020, because I want to cover the reform in 2019 and I don't
use the system of vintage : because if I use the 2019 vintage to have data on 2018, there is a risk of incompletude 
because there will be data on 2018, only if the worker is still working in 2019"

#Variables necessary for the study
variables_to_keep <- c("SIRET", "REGT", "CONTRAT_TRAVAIL", "AGE", "A88", "DATDEB",
                       "DATFIN", "S_BRUT")

data2018_1 <- read_sas("C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/postp_2018_1.sas7bdat",
                 col_select = variables_to_keep)%>%
  na.omit()
data2018_2 <- read_sas("C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/postp_2018_2.sas7bdat",
                     col_select = variables_to_keep)%>%
  na.omit()
data2019_1 <- read_sas("C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/postp_2019_1.sas7bdat",
                 col_select = variables_to_keep)%>%
  na.omit()
data2019_2 <- read_sas("C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/postp_2019_2.sas7bdat",
                       col_select = variables_to_keep)%>%
  na.omit()
data2020 <- read_sas("C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/postp_2019_2020.sas7bdat",
                 col_select = variables_to_keep)%>%
  na.omit()


#To prepare the appending of the datasets, I create a variable AN ("year")
data2018_1 <- data2018_1%>%mutate(AN = 2018)
data2018_2 <- data2018_2%>%mutate(AN = 2018)
data2019_1 <- data2019_1%>%mutate(AN = 2019)
data2019_2 <- data2019_2%>%mutate(AN = 2019)
data2020 <- data2020%>%mutate(AN = 2020)

#In 2018, the variable AGE is not numeric
data2018_1 <- data2018_1%>%mutate(AGE = as.numeric(AGE))
data2018_2 <- data2018_2%>%mutate(AGE = as.numeric(AGE))

#Appending of the datasets
data <- bind_rows(data2018_1,data2018_2, data2019_1, data2019_2, data2020)%>%
  na.omit()

#My goal is to do regression with fixed firm effects, so I only keep the firms that 
#are appearing two different years
data <- data %>%
  group_by(SIRET)%>%
  filter(n_distinct(AN)>=2)%>%
  ungroup()



#Goal : create a firm panel data from 2018 to 2019 
#Variables that I create for the future analysis : number of aapprentices, CDI, short contracts,
#region of the firm, sector of activity, average gross wage, 
#number of new contracts, of end of contracts

data_final <- data %>%
  group_by(SIRET,AN)%>%
  summarise(
    nb_apprentis = sum(CONTRAT_TRAVAIL %in% c ("04","05"), na.rm = TRUE),
    nb_CDI = sum(CONTRAT_TRAVAIL == "01", na.rm = TRUE),
    nb_courts = sum(CONTRAT_TRAVAIL %in% c ("02","03","27","28","88","90","93","95","96"), na.rm = TRUE),
    region = names(sort(table(REGT),decreasing = TRUE))[1],
    secteur = names(sort(table(A88),decreasing = TRUE))[1],
    nb_apprentis_debut = sum(CONTRAT_TRAVAIL %in% c ("04","05") & DATDEB > 1, na.rm = TRUE),
    nb_apprentis_fin = sum(CONTRAT_TRAVAIL %in% c ("04","05") & DATFIN < 360, na.rm = TRUE),
    nb_apprentis_complet = sum(CONTRAT_TRAVAIL %in% c ("04","05") & DATFIN == 360 & DATDEB == 1, na.rm = TRUE),
    brut_moy = mean(S_BRUT, na.rm = TRUE),
    nb_CDI_debut = sum(CONTRAT_TRAVAIL == "01" & DATDEB > 1, na.rm = TRUE),
    nb_CDD_debut = sum(CONTRAT_TRAVAIL == "02" & DATDEB > 1, na.rm = TRUE),
    nb_entrées = sum(DATDEB > 1, na.rm = TRUE),
    nb_sorties = sum(DATFIN < 360, na.rm = TRUE)
    )
  

#Average age for non-apprentices
data_age <- data %>%
  filter(!CONTRAT_TRAVAIL %in% c("04","05"))%>%
  group_by(SIRET, AN)%>%
  summarise(age_moy_nonapprentis = mean(AGE, na.rm = TRUE))

#Merge the average age data with the main data
data_final <- left_join(data_final, data_age,  by = c("SIRET","AN"))

#Upload the final dataset
write.csv(data_final, "C:/Users/Public/Documents/Kilian/Data/DADS/Tous_Salariés/data_final_2018_2020.csv")
