

test_that("Tester que la fonction d'accroissement retourne la valeur attendue. Quand AccModif = 'ORI'", {

  PredAcc <- readRDS(test_path("fixtures", "PredAcc.rds"))
  Cor <- 0.10628207185

    set.seed(NULL)
    set.seed(3)

    accroissement <- PredAcc %>%
    group_by(origTreeID) %>%
    nest() %>%
    mutate(pred_acc = map(data,accrois)) %>%
    unnest(pred_acc) %>%
    mutate(pred_acc=ifelse(pred_acc<0,0,(exp(pred_acc+Cor)-1))) %>%
    select(-data)
    set.seed(NULL)

    expect_test_for_Artemis_accroissement <- readRDS(test_path("fixtures", "expect_test_for_Artemis_accroissement.rds"))

    expect_equal(accroissement, expect_test_for_Artemis_accroissement)
})
