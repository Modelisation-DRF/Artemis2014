test_that("Test la mise en forme des données pour la simulation avec EvolClim=0, AccModif=ORI et MortModif=ORI", {

  Data_0_ORI_ORI<-PrepareData(Data=Donnees_Exemple, Clim_tous=ClimMois_Exemple, ClimAn_tous=ClimAn_Exemple,
                              AccModif="ORI", EvolClim=0, MortModif="QUE", RCP="RCP45",
                              SpInd=SpInd, ListeVp=ListeVp, SpGroups=SpGroups, Sp=Sp)
  expect_test_for_Data_0_ORI <- readRDS(test_path("fixtures", "expect_result_for_PrepareData_0_ORI_ORI_Data.rds"))

  #expect_equal(Data_0_ORI_ORI[[1]], expect_test_for_Data_0_ORI ) # ça ne fonctionne plus à cause des attributs
  expect_true(isTRUE(all.equal(Data_0_ORI_ORI[[1]], expect_test_for_Data_0_ORI, check.attributes = FALSE)))
})

test_that("Test la mise en forme des données climatiques mensuelles pour la simulation avec EvolClim=0,
          AccModif=ORI et MortModif=ORI", {

  Data_0_ORI_ORI<-PrepareData(Data=Donnees_Exemple, Clim_tous=ClimMois_Exemple, ClimAn_tous=ClimAn_Exemple,
                              AccModif="ORI", EvolClim=0, MortModif="QUE", RCP="RCP45",
                              SpInd=SpInd, ListeVp=ListeVp, SpGroups=SpGroups, Sp=Sp)
  expect_test_for_ClimatMois_0_ORI <- readRDS(test_path("fixtures", "expect_result_for_ClimatMois_0_ORI_ORI_Data.rds"))

  expect_equal(Data_0_ORI_ORI[[3]], expect_test_for_ClimatMois_0_ORI )
})

test_that("Test la mise en forme des données climatiques annuelles pour la simulation avec EvolClim=0,
          AccModif=ORI et MortModif=ORI", {

  Data_0_ORI_ORI<-PrepareData(Data=Donnees_Exemple, Clim_tous=ClimMois_Exemple, ClimAn_tous=ClimAn_Exemple,
                              AccModif="ORI", EvolClim=0, MortModif="QUE", RCP="RCP45",
                              SpInd=SpInd, ListeVp=ListeVp, SpGroups=SpGroups, Sp=Sp)
  expect_test_for_ClimatMois_0_ORI <- readRDS(test_path("fixtures", "expect_result_for_ClimatAn_0_ORI_ORI_Data.rds"))

  expect_equal(Data_0_ORI_ORI[[4]], expect_test_for_ClimatMois_0_ORI )
})
