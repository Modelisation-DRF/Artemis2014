

# Fichier pour les tests du simulateur Artemis

saveRDS(expect_test_for_Artemis_AccModif_ORI_MortModif_ORI, "tests/testthat/fixtures/expect_test_for_Artemis_AccModif_ORI_MortModif_ORI.rds")


#  Fichier des arbres sur lequel on applique la fonction de d'Accroissement
#  PredAcc <- Accrois %>%
#  mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
#  PTot=PTot,RegionOuest=RegionOuest,sum_st_ha=sum_st_ha, Drainage=Drainage, Veg_Pot=Veg_Pot)
saveRDS(PredAcc, "tests/testthat/fixtures/PredAcc.rds")


# Data frame résultant de la fonction d'accroissement.
saveRDS(expect_test_for_Artemis_accroissement, "tests/testthat/fixtures/expect_test_for_Artemis_accroissement.rds")


# Fichier des arbres sur lequel on applique la fonction de de Mortalité
#  PredMort <- Mort %>%
#  mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
#  PTot=PTot,RegionOuest=RegionOuest,sum_st_ha=sum_st_ha, Drainage=Drainage,
#  Veg_Pot=Veg_Pot, TMoy=TMoy)
saveRDS(PredMort, "tests/testthat/fixtures/PredMort.rds")

saveRDS(expect_test_for_Artemis_Mortalite , "tests/testthat/fixtures/expect_test_for_Artemis_Mortalite.rds")


saveRDS(placcette_ArtemisClim , "tests/testthat/fixtures/placcette_ArtemisClim.rds")

saveRDS(expect_result_for_ArtemisClimat , "tests/testthat/fixtures/expect_result_for_ArtemisClimat.rds")
