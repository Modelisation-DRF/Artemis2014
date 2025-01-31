

test_that("La fonction simulateurArtemis() avec les Changements climatiques, Paramètres de recrutement ajustés, Coupe partielle réalisée depuis moins de 10 ans,
          Évolution du climat, Module d’accroissement Original et Module de mortalité Original et sans Données climatiques ", {

  set.seed(NULL)
  set.seed(3)


  Result <- simulateurArtemis(Data_ori = Intrant_Ref ,Horizon = 3,Clim = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45')

  expect_test_for_Artemis_AccModif_ORI_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_ORI_MortModif_ORI.rds"))

  set.seed(NULL)

  expect_equal(Result, expect_test_for_Artemis_AccModif_ORI_MortModif_ORI)

})

