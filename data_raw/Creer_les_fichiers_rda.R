# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

#library(tidyverse)

#####################Data à intégrer dans systdata.rda


######Parametres de base
CovParms<-read_delim("data_raw/CovParms.csv", delim=",")

ListeCor <- CovParms %>%
            filter(CovParmEffet %in% c(1,3,4)) %>% # ne pas selectionner la correlation
            rename(Veg_Pot=Veg_pot) %>%
            group_by(Veg_Pot) %>%
            summarise(Cor = sum(ParameterEstimate)/2)


Para<-read_delim("data_raw/Para.csv", delim=",") %>%
      mutate(Effect = str_to_lower(Effect))

Effet.acc<- Para %>%
             filter(SubModuleID==2) %>%
              mutate(Effect = str_to_lower(Effect)) %>% # il y a des doublons d'effets a cause des minuscules/majuscule pour un meme nom
              select(Effect) %>%
              unique() %>%
              arrange(Effect)

Effet.mort <- Para %>%
              filter(SubModuleID==1) %>%
              mutate(Effect = str_to_lower(Effect)) %>% # il y a des doublons d'effets a cause des minuscules/majuscule pour un meme nom
              select(Effect) %>%
              unique() %>%
              arrange(Effect)

Effet.rec <- Para %>%
             filter(SubModuleID %in% c(3,4,5)) %>%
             mutate(Effect = str_to_lower(Effect)) %>% # il y a des doublons d'effets a cause des minuscules/majuscule pour un meme nom
             filter(!Effect %in% c("dispersion","scale"))%>%
             select(Effect) %>%
             unique() %>%
             arrange(Effect)

ListeVp<-read_delim("data_raw/ListeVP.csv", delim=";")

Sp<-read_delim("data_raw/Sp.csv", delim=";")

SpGroups<-read_delim("data_raw/SpGroups.csv", delim=",")

SpInd<-read_delim("data_raw/Spind.csv", delim=";")

ListeSpVp<-merge(SpGroups,Sp, by="SpeciesGroupID") %>%
           merge(ListeVp, by="VegPotID")

ParaBiomasse<-read_delim("data_raw/ParaBiomasse.csv", delim=";")

PropEPX<-read_delim("data_raw/PropEPX.csv", delim=";")

#######MortQUE
CovParmMortQUE<-read_delim("data_raw/MortQUE/CovParmMortQUE.csv", delim=";")
EssGrMortQUE<-read_delim("data_raw/MortQUE/EssGrMortQUE.csv", delim=";")
ParaMortQUE<-read_delim("data_raw/MortQUE/ParaMortQUE.csv", delim=";")


usethis::use_data(CovParms, ListeCor, Para, Effet.acc, Effet.mort, Effet.rec,
                  ListeVp,ListeSpVp, Sp, SpGroups, SpInd, ListeSpVp, ParaBiomasse, CovParmMortQUE,
                  EssGrMortQUE, ParaMortQUE,
                  internal=TRUE, overwrite = TRUE)


######################DonneesAccessibles
#######BRT
CO2<-read_delim("data_raw/CO2.csv", delim=";")
EssGrBRT<-read_delim("data_raw/BRT/EssGr_BRT.csv", delim=";")
mod_bop_BRT<-readRDS("data_raw/BRT/mod_bop_BRT.rds")
mod_epb_BRT<-readRDS("data_raw/BRT/mod_epb_BRT.rds")
mod_epn_BRT<-readRDS("data_raw/BRT/mod_epn_BRT.rds")
mod_peu_BRT<-readRDS("data_raw/BRT/mod_peu_BRT.rds")
mod_pig_BRT<-readRDS("data_raw/BRT/mod_pig_BRT.rds")
mod_sab_BRT<-readRDS("data_raw/BRT/mod_sab_BRT.rds")

#########GAM
EssGrGAM<-read_delim("data_raw/GAM/EssGr_GAM.csv", delim=";")
mod_bop_GAM<-readRDS("data_raw/GAM/mod_bop_GAM.rds")
mod_epb_GAM<-readRDS("data_raw/GAM/mod_epb_GAM.rds")
mod_epn_GAM<-readRDS("data_raw/GAM/mod_epn_GAM.rds")
mod_peu_GAM<-readRDS("data_raw/GAM/mod_peu_GAM.rds")
mod_pig_GAM<-readRDS("data_raw/GAM/mod_pig_GAM.rds")
mod_sab_GAM<-readRDS("data_raw/GAM/mod_sab_GAM.rds")

#########Exemples

ClimAn_Test<-read_delim("data_raw/ClimAn_test.csv", delim=";")
ClimMois_Test<-read_delim("data_raw/ClimMois_test.csv", delim=";")
Intrant_Test<-read_delim("data_raw/Intrant_test.csv", delim=";")
ClimAn_Exemple<-read_delim("data_raw/ClimAn_Exemple.csv", delim=",")
ClimMois_Exemple<-read_delim("data_raw/ClimMois_Exemple.csv", delim=",")
Donnees_Exemple<-read_delim("data_raw/ClimMois_Exemple.csv", delim=",")


usethis::use_data(ClimAn_Test, ClimMois_Test, Intrant_Test,
                  ClimAn_Exemple,ClimMois_Exemple,Donnees_Exemple,
                  CO2, EssGrBRT, mod_epb_BRT, mod_epn_BRT, mod_peu_BRT,
                  mod_pig_BRT, mod_sab_BRT, mod_bop_BRT,
                  PropEPX, EssGrGAM, mod_bop_GAM, mod_epb_GAM,
                  mod_epn_GAM, mod_peu_GAM, mod_pig_GAM, mod_sab_GAM,
                  internal=FALSE, overwrite = TRUE)####On garde les modèles GAM dans data sinon syst data trop gros




