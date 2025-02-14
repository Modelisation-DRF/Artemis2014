
test_that("multiplication works", {

  set.seed(NULL)
  set.seed(3)

  Models<-ChargeModeles("BRT","ORI")

  ClimatBRT <- data.frame(PlacetteID = "PMJ12",FFP = 100.0758,PTotPeriode = 839.4394,TMoyPeriode = 3.027273,Tmax_yr = 8.927273,CMI = -99.7788,Aridity = 58.40606,
                          DD = 1565.533,Max_ST = 20.90061,Min_WT = -17.56465,MSP = 429.4273,PAS = 140.1242,PUtil = 88.4697,TmaxUtil = 23.18889,TSummer = 14.51303,
                          Snow_cat = "medium")
  sum_st_ha <-  7.0686
  t <- 10

  Accrois <- readRDS(test_path("fixtures", "plac_AccroisBRT.rds"))

  ResultAccroisBrt <- AccroissementBRT(Accrois, ClimatBRT, Models, sum_st_ha, t)

  ResultAccroisBrt <- as.data.frame(ResultAccroisBrt)%>% mutate(pred_acc = round(pred_acc, 5))

  set.seed(NULL)


  expectForAccBRT <- data.frame(
    origTreeID = 1:16,
    pred_acc = c(1.316761, 1.316761, 1.316761, 1.316761, 4.642885, 1.316761, 1.316761, 1.316761,
                 1.316761, 1.316761, 2.496479, 1.777854, 1.316761, 2.493227, 2.493227, 2.493227))

  expectForAccBRT <- as.data.frame(expectForAccBRT)%>% mutate(pred_acc = round(pred_acc, 5))

  expect_equal(ResultAccroisBrt, expectForAccBRT)
})
