
test_that("Tester que la fonction de mortaliteQUES retourne la valeur attendue. Quand MortModif = 'QUE'", {


  set.seed(NULL)
  set.seed(3)

  Models<-ChargeModeles("ORI","QUE")

  Mort <- readRDS(test_path("fixtures", "plac_Mortalite_QUE.rds"))

  ClimatQUE <- data.frame(PlacetteID = "PMJ12",FFP = 100.0758,PTotPeriode = 839.4394,TMoyPeriode = 3.027273,Tmax_yr = 8.927273,CMI = -99.7788,Aridity = 58.4606,DD = 1565.533,
                   Max_ST = 20.90061,Min_WT = -17.56465,MSP = 429.4273,PAS = 140.1242,PUtil = 88.4697,TmaxUtil = 23.18889,TSummer = 14.51303,Snow_cat = "medium",stringsAsFactors = FALSE
  )
  DrainageCl <- "MesSub"
  PenteCl <- "B"
  Texture <- "FinMoy"
  Coupe<-0
  Coupe0<-0
  sum_st_ha <- 7.0686
  t <- 10

  PredMort<-mortQUE(Mort, ClimatQUE, Models, DrainageCl, PenteCl, Texture, Coupe, Coupe0, sum_st_ha, t)

  set.seed(NULL)

  expect_test_for_Artemis_MortaliteQUE <- readRDS(test_path("fixtures", "expect_test_for_Artemis_MortaliteQUE.rds"))

  expect_equal(PredMort, expect_test_for_Artemis_MortaliteQUE)

})
