
test_that("Tester que la fonction de mortalité retourne la valeur attendue. Quand MortModif = 'ORI'", {

  PredMort <- readRDS(test_path("fixtures", "PredMort.rds"))

  set.seed(NULL)
  set.seed(3)

  mortalite <- PredMort %>%
    group_by(origTreeID) %>%
    nest() %>%
    mutate(pred_mort = map(data,mort)) %>%
    unnest(pred_mort) %>%
    select(-data)

  set.seed(NULL)

  expect_test_for_Artemis_Mortalite <- readRDS(test_path("fixtures", "expect_test_for_Artemis_Mortalite.rds"))

  expect_equal(mortalite, expect_test_for_Artemis_Mortalite)
})
