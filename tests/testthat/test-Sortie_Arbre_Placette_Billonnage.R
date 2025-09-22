test_that("Test la sortie Arbre et la sortie placette pour la simulation avec EvolClim=0 et AccModif=ORI et MortModif=ORI", {

  Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3,Clim = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
            arrange(PlacetteID,origTreeID,Annee) %>%
            select(-Cl_Drai, -Residuel)

  ResultArbre<-SortieArbre(Result)

  expect_test_for_ResultArbre <- readRDS(test_path("fixtures", "expect_result_for_Sortie_Arbre_0_ORI_ORI.rds"))

  expect_equal(ResultArbre, expect_test_for_ResultArbre)

  ResultPlacette<-SortiePlacette(Result)

  expect_test_for_ResultPlacette <- readRDS(test_path("fixtures", "expect_result_for_Sortie_Placette_0_ORI_ORI.rds"))

  expect_equal(ResultPlacette, expect_test_for_ResultPlacette)
})


test_that("Test les sorties billonage pour la simulation avec EvolClim=0 et AccModif=ORI et MortModif=ORI", {

  IntrantsBillons<-Intrant_Test
  IntrantsBillons$DHPcm<-IntrantsBillons$DHPcm+10#####ajoute 10 cm pour avoir des abres de grandes dimension

  Result <- simulateurArtemis(Data_ori = IntrantsBillons ,Horizon = 3,Clim = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
    arrange(PlacetteID,origTreeID,Annee) %>%
    select(-Cl_Drai, -Residuel)

  ResultBillon<-SortieBillonage(Result, Type="DHP")

  expect_test_for_ResultBillon <- readRDS(test_path("fixtures", "expect_result_for_Sortie_Billonnage_0_ORI_ORI.rds"))

  expect_equal(ResultBillon, expect_test_for_ResultBillon)

  ResultBillon2015<-SortieBillonage(Result, Type="DHP2015")

  expect_test_for_ResultBillon2015 <- readRDS(test_path("fixtures", "expect_result_for_Sortie_Billonnage2015_0_ORI_ORI.rds"))

  expect_equal(ResultBillon2015, expect_test_for_ResultBillon2015)
})
