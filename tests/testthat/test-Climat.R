test_that("Test la mise en forme des données climatiques pour la simulation avec EvolClim=0 et AccModif=ORI", {

 Climat_result_0_ORI<-ClimatBiosim(Placettes=Donnees_Exemple$PlacetteID, Annee=2020, t=10, RCP="RCP45",
                          ClimMois=ClimMois_Exemple, ClimAn=ClimAn_Exemple, EvolClim=0, AccModif="ORI")

   expect_test_for_Climat_0_ORI <- readRDS(test_path("fixtures", "expect_result_for_Climat_0_ORI.rds"))

  expect_equal(Climat_result_0_ORI, expect_test_for_Climat_0_ORI  )
})

test_that("Test la mise en forme des données climatiques pour la simulation avec EvolClim=1 et AccModif=ORI", {

  Climat_result_1_ORI<-ClimatBiosim(Placettes=Donnees_Exemple$PlacetteID, Annee=2020, t=10, RCP="RCP45",
                                    ClimMois=ClimMois_Exemple, ClimAn=ClimAn_Exemple, EvolClim=1, AccModif="ORI")

  expect_test_for_Climat_1_ORI <- readRDS(test_path("fixtures", "expect_result_for_Climat_1_ORI.rds"))

  expect_equal(Climat_result_1_ORI, expect_test_for_Climat_1_ORI  )
})

test_that("Test la mise en forme des données climatiques pour la simulation avec EvolClim=0 et AccModif=GAM", {

  Climat_result_0_GAM<-ClimatBiosim(Placettes=Donnees_Exemple$PlacetteID, Annee=2020, t=10, RCP="RCP45",
                                    ClimMois=ClimMois_Exemple, ClimAn=ClimAn_Exemple, EvolClim=0, AccModif="GAM")

  expect_test_for_Climat_0_GAM <- readRDS(test_path("fixtures", "expect_result_for_Climat_0_GAM.rds"))

  expect_equal(Climat_result_0_GAM, expect_test_for_Climat_0_GAM)
})

test_that("Test la mise en forme des données climatiques pour la simulation avec EvolClim=1 et AccModif=GAM", {

  Climat_result_1_GAM<-ClimatBiosim(Placettes=Donnees_Exemple$PlacetteID, Annee=2020, t=10, RCP="RCP45",
                                    ClimMois=ClimMois_Exemple, ClimAn=ClimAn_Exemple, EvolClim=1, AccModif="GAM")

  expect_test_for_Climat_1_GAM <- readRDS(test_path("fixtures", "expect_result_for_Climat_1_GAM.rds"))

  expect_equal(Climat_result_1_GAM, expect_test_for_Climat_1_GAM)
})

test_that("Test la mise en forme des données climatiques pour la simulation avec EvolClim=0 et AccModif=BRT", {

  Climat_result_0_BRT<-ClimatBiosim(Placettes=Donnees_Exemple$PlacetteID, Annee=2020, t=10, RCP="RCP45",
                                    ClimMois=ClimMois_Exemple, ClimAn=ClimAn_Exemple, EvolClim=0, AccModif="BRT")

  expect_test_for_Climat_0_BRT <- readRDS(test_path("fixtures", "expect_result_for_Climat_0_BRT.rds"))

  expect_equal(Climat_result_0_BRT, expect_test_for_Climat_0_BRT)
})

test_that("Test la mise en forme des données climatiques pour la simulation avec EvolClim=1 et AccModif=BRT", {

  Climat_result_1_BRT<-ClimatBiosim(Placettes=Donnees_Exemple$PlacetteID, Annee=2020, t=10, RCP="RCP45",
                                    ClimMois=ClimMois_Exemple, ClimAn=ClimAn_Exemple, EvolClim=1, AccModif="BRT")

  expect_test_for_Climat_1_BRT <- readRDS(test_path("fixtures", "expect_result_for_Climat_1_BRT.rds"))

  expect_equal(Climat_result_1_BRT, expect_test_for_Climat_1_BRT)
})
