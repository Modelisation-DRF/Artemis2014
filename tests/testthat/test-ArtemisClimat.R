
test_that("La fonction ArtemisClimat fonctionne comme attendu avec les parametres de base", {

  set.seed(NULL)
  set.seed(3)

  Para <- Para %>% mutate(Effect = str_to_lower(Effect))
  Data <- readRDS(test_path("fixtures", "placcette_ArtemisClim.rds"))
  Data$Depot<-"Tv"
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

  set.seed(NULL)
  expect_result_for_ArtemisClimat <- readRDS(test_path("fixtures", "expect_result_for_ArtemisClimat.rds")) %>%
    mutate(Residuel=0) # on a ajouté la colonne Residuel au outpuut de cette fonction, et dans ce test, toutes les valeurs doivent être à 0

  expect_equal(result, expect_result_for_ArtemisClimat, tolerance = 1e-4)

})


test_that("La fonction ArtemisClimat fonctionne comme attendu avec les parametres de base et MCH=1", {


  Para <- Para %>% mutate(Effect = str_to_lower(Effect))
  Data <- readRDS(test_path("fixtures", "placcette_ArtemisClim.rds")) # une placette de 16 arbres, 16 essences différentes, dhp=15
  Data$Depot<-"Tv"
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

  # set.seed(NULL)
  # set.seed(3)
  # vérifier la valeur du HEG quand MCH=0
  # result0 <- ArtemisClimat(Para=Para,  Data=Data,
  #                         AnneeDep=AnneeDep, Horizon=Horizon, FacHa=FacHa, Tendance=Tendance, Residuel=Residuel, Clim=Clim, ClimAn =ClimAn,
  #                         EvolClim =EvolClim, AccModif=AccModif, MortModif= MortModif, RCP=RCP, Models = Models, MCH=0)
  # set.seed(NULL)

  set.seed(NULL)
  set.seed(3)
  result1 <- ArtemisClimat(Para=Para,  Data=Data,
                           AnneeDep=AnneeDep, Horizon=Horizon, FacHa=FacHa, Tendance=Tendance, Residuel=Residuel, Clim=Clim, ClimAn =ClimAn,
                           EvolClim =EvolClim, AccModif=AccModif, MortModif= MortModif, RCP=RCP, Models = Models, MCH=1)
  set.seed(NULL)

  # le HEG, l'arbre 10, doit avoir Nombre = 1 - 0.104004375 à l'année 2035
  # à l'année 2025, c'est le point de départ, ça ne change pas
  # et le HEG numero 28 est une recrue à l'année 2035, donc son nombre ne change pas
  attendu <- readRDS(test_path("fixtures", "expect_result_for_ArtemisClimat.rds")) %>%
    mutate(Residuel=0) %>% # on a ajouté la colonne Residuel au output de cette fonction, et dans ce test, toutes les valeurs doivent être à 0
    filter(Annee==2035)
  attendu <- attendu %>% mutate(Nombre = ifelse(origTreeID==10 & Annee==2035, 1 - 0.104004375, Nombre))
  result1 <- result1 %>% filter(Annee==2035)
  expect_equal(result1, attendu, tolerance = 1e-4)

})
