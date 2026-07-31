test_that("Test la sortie Biomasse pour la simulation avec EvolClim=0 et AccModif=ORI et MortModif=ORI", {

  Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3,ClimTot = NULL,
                              AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
            mutate(Annee=Annee-(as.numeric(format(Sys.Date(), "%Y"))-2025))%>%
            arrange(PlacetteID,origTreeID,Annee)

  ResultBiomasse<-SortieBiomasse(Result)

  expect_test_for_ResultBiomasse <- readRDS(test_path("fixtures", "expect_result_for_Sortie_Biomasse_0_ORI_ORI.rds"))

  expect_equal(ResultBiomasse, expect_test_for_ResultBiomasse)


})


