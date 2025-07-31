

# Fichier pour les tests du simulateur Artemis Module d’accroissement Original et Module de mortalité Original

saveRDS(Result, "tests/testthat/fixtures/expect_test_for_Artemis_AccModif_ORI_MortModif_ORI.rds")

# Fichier pour les tests du simulateur Artemis Module d’accroissement BRT et Module de mortalité Original

saveRDS(Result, "tests/testthat/fixtures/expect_test_for_Artemis_AccModif_BRT_MortModif_ORI.rds")

# Fichier pour les tests du simulateur Artemis Module d’accroissement BRT et Module de mortalité Original

saveRDS(Result, "tests/testthat/fixtures/expect_test_for_Artemis_AccModif_GAM_MortModif_QUE.rds")



#  Fichier des arbres sur lequel on applique la fonction de d'Accroissement
#  PredAcc <- Accrois %>%
#  mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
#  PTot=PTot,RegionOuest=RegionOuest,sum_st_ha=sum_st_ha, Drainage=Drainage, Veg_Pot=Veg_Pot)
saveRDS(PredAcc, "tests/testthat/fixtures/PredAcc.rds")


# Data frame résultant de la fonction d'accroissement.
saveRDS(expect_test_for_Artemis_accroissement, "tests/testthat/fixtures/expect_test_for_Artemis_accroissement.rds")

# Data frame résultant de la fonction d'accroissement.
saveRDS(expect_test_for_Artemis_MortaliteQUE, "tests/testthat/fixtures/expect_test_for_Artemis_MortaliteQUE.rds")


# Fichier des arbres sur lequel on applique la fonction de de Mortalité
#  PredMort <- Mort %>%
#  mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
#  PTot=PTot,RegionOuest=RegionOuest,sum_st_ha=sum_st_ha, Drainage=Drainage,
#  Veg_Pot=Veg_Pot, TMoy=TMoy)
saveRDS(PredMort, "tests/testthat/fixtures/PredMort.rds")

saveRDS(expect_test_for_Artemis_Mortalite , "tests/testthat/fixtures/expect_test_for_Artemis_Mortalite.rds")


saveRDS(placcette_ArtemisClim , "tests/testthat/fixtures/placcette_ArtemisClim.rds")

saveRDS(result , "tests/testthat/fixtures/expect_result_for_ArtemisClimat.rds")


# fichier des arbres de la placette pour appliquer le module d'accroissement
saveRDS(plac_Accrois , "tests/testthat/fixtures/plac_Accrois.rds")

# fichier des arbres de la placette pour appliquer le module d'accroissement BRT
saveRDS(plac_AccroisBRT , "tests/testthat/fixtures/plac_AccroisBRT.rds")

# fichier des arbres de la placette pour appliquer le module d'mortalite QUE
saveRDS(plac_Mortalite_QUE , "tests/testthat/fixtures/plac_Mortalite_QUE.rds")

# fichier des arbres de la placette pour appliquer le module d'accroissement BRT
saveRDS(plac_AccroisGAM , "tests/testthat/fixtures/plac_AccroisGAM.rds")


# Résultat de la fonction Ess_groupe
saveRDS(GroupeTest, "tests/testthat/fixtures/expect_result_for_Ess_groupe.rds")


# Résultat de la fonction Climat
saveRDS(Climat_result_0_ORI, "tests/testthat/fixtures/expect_result_for_Climat_0_ORI.rds")
saveRDS(Climat_result_1_ORI, "tests/testthat/fixtures/expect_result_for_Climat_1_ORI.rds")
saveRDS(Climat_result_0_GAM, "tests/testthat/fixtures/expect_result_for_Climat_0_GAM.rds")
saveRDS(Climat_result_1_GAM, "tests/testthat/fixtures/expect_result_for_Climat_1_GAM.rds")
saveRDS(Climat_result_0_BRT, "tests/testthat/fixtures/expect_result_for_Climat_0_BRT.rds")
saveRDS(Climat_result_1_BRT, "tests/testthat/fixtures/expect_result_for_Climat_1_BRT.rds")


# Résultat de prépare Data

saveRDS(Data_0_ORI_ORI[[1]], "tests/testthat/fixtures/expect_result_for_PrepareData_0_ORI_ORI_Data.rds")
saveRDS(Data_0_ORI_ORI[[3]], "tests/testthat/fixtures/expect_result_for_ClimatMois_0_ORI_ORI_Data.rds")
saveRDS(Data_0_ORI_ORI[[4]], "tests/testthat/fixtures/expect_result_for_ClimatAn_0_ORI_ORI_Data.rds")

# Résultats sortie Arbre placette

saveRDS(ResultArbre, "tests/testthat/fixtures/expect_result_for_Sortie_Arbre_0_ORI_ORI.rds")
saveRDS(ResultPlacette, "tests/testthat/fixtures/expect_result_for_Sortie_Placette_0_ORI_ORI.rds")

# Résultats sortie billonage
saveRDS(ResultBillon, "tests/testthat/fixtures/expect_result_for_Sortie_Billonnage_0_ORI_ORI.rds")
saveRDS(ResultBillon2015, "tests/testthat/fixtures/expect_result_for_Sortie_Billonnage2015_0_ORI_ORI.rds")
