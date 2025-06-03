
test_that("La fonctionne ArtemisClimat fonctionne comme attendu avec les parametres de base", {

  set.seed(NULL)
  set.seed(3)

  Para <- Para %>% mutate(Effect = str_to_lower(Effect))
  Data <- readRDS(test_path("fixtures", "placcette_ArtemisClim.rds"))
  AnneeDep <- 2025
  Horizon <- 3
  FacHa <- 25
  Tendance <- 0
  Residuel <- 0
  Clim <- NULL
  ClimAn<- NULL
  EvolClim <- 0
  AccModif <- "ORI"
  MortModif <- "ORI"
  RCP <- "RCP45"
  Models <- NULL

  result <- ArtemisClimat(Para=Para,  Data=Data,
                AnneeDep=AnneeDep, Horizon=Horizon, FacHa=FacHa, Tendance=Tendance, Residuel=Residuel, Clim=Clim, ClimAn =ClimAn,
                EvolClim =EvolClim, AccModif=AccModif, MortModif= MortModif, RCP=RCP, Models = Models)

  result<-result %>% select(-Cl_Drai)#Enlevé drainage car rajouté après
  set.seed(NULL)
  expect_result_for_ArtemisClimat <- readRDS(test_path("fixtures", "expect_result_for_ArtemisClimat.rds"))

  expect_equal(result, expect_result_for_ArtemisClimat, tolerance = 1e-4)

})
