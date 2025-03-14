
test_that("Tester que la fonction d'accroissementGAM retourne la valeur attendue. Quand AccModif = 'GAM'",{

  set.seed(NULL)
  set.seed(3)

  Models<-ChargeModeles("GAM","ORI")

  ClimatGAM <- data.frame( PlacetteID = "PMJ12",FFP = 100.0758,PTotPeriode = 839.4394,TMoyPeriode = 3.027273,Tmax_yr = 8.927273,CMI = -99.7788,Aridity = 58.40606,
                           DD = 1565.533,Max_ST = 20.90061,Min_WT = -17.56465,MSP = 429.4273,PAS = 140.1242,PUtile = 88.4697,TmaxUtil = 23.18889,TSummer = 14.51303,
                           Snow_cat = "medium", stringsAsFactors = FALSE # Empêcher la conversion automatique en facteur
                           )
  sum_st_ha <-  7.0686
  t <- 10

  Accrois <- readRDS(test_path("fixtures", "plac_AccroisGAM.rds"))

  ResultAccroisGAM<-AccroissementGAM(Accrois, ClimatGAM, Models, t)

  set.seed(NULL)

  expectForAccGAM <- data.frame(
    origTreeID = 1:16,
    pred_acc = c(1.4970937, 1.4970937, 1.4970937, 1.4970937, 2.4706859, 1.4970937, 1.4970937, 1.4970937,
                 1.4970937, 1.4970937, 1.4943989, 0.8820483, 1.4970937, 2.2040869, 2.2040869, 2.2040869),
    stringsAsFactors = FALSE # Empêcher la conversion automatique en facteur
  )

  expectForAccGAM <- as.data.frame(expectForAccGAM)%>% mutate(pred_acc = round(pred_acc, 5))

  ResultAccroisGAM <- as.data.frame(expectForAccGAM)%>% mutate(pred_acc = round(pred_acc, 5))

  expect_true(all.equal(ResultAccroisGAM, expectForAccGAM, tolerance = 1e-6))

})
