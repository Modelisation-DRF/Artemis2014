
test_that("Tester que la fonction de mortalité retourne la valeur attendue avec une placette de plusieurs arbres. Quand MortModif = 'ORI'", {

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


test_that("Tester que la fonction de mortalité retourne la valeur attendue avec un data d'une ligne, sans MCH", {

  PredMort <- readRDS(test_path("fixtures", "PredMort.rds"))
  PredMort1 <- PredMort[PredMort$Espece=='HEG',] # ligne 10

  obtenu <- mort(PredMort1)

  attendu <- readRDS(test_path("fixtures", "expect_test_for_Artemis_Mortalite.rds"))
  attendu1 <- attendu[10,]

  expect_equal(obtenu, attendu1$pred_mort)

  })

test_that("Tester que la fonction de mortalité retourne la valeur attendue avec un data d'une ligne HEG, avec MCH", {

  PredMort <- readRDS(test_path("fixtures", "PredMort.rds"))
  PredMort1 <- PredMort[PredMort$Espece=='HEG',] # ligne 10

  obtenu <- mort(PredMort1, MCH=1)

  attendu <- -5.4430 + 0.0621*PredMort1$DHPcm + log(PredMort1$t)
  attendu <- (1-exp(-exp(attendu)))

  expect_equal(obtenu, 0.104004375)

})

test_that("Tester que la fonction de mortalité retourne la valeur attendue avec un data d'une ligne d'une recrue, avec MCH", {

  PredMort <- readRDS(test_path("fixtures", "PredMort.rds"))
  PredMort1 <- PredMort[PredMort$Espece=='HEG',] # ligne 10
  PredMort1$GrEspece <- 'AUT'
  PredMort1$Espece <- NA  # les recrues d'un groupe d'especes on Espece à NA

  expect_no_error(mort(PredMort1, MCH=1))

})


test_that("Tester que la fonction de mortalité retourne la valeur attendue avec une placette de plusieurs arbres. Quand MortModif = 'ORI' MCH=1", {

  PredMort <- readRDS(test_path("fixtures", "PredMort.rds"))

  mortalite <- PredMort %>%
    group_by(origTreeID) %>%
    nest() %>%
    mutate(pred_mort = map(data,mort, MCH=1)) %>%
    unnest(pred_mort) %>%
    select(-data)


  attendu <- readRDS(test_path("fixtures", "expect_test_for_Artemis_Mortalite.rds"))
  # le HEG est à la ligne 10 et doit avoir la valeur 0.1040044
  attendu[10,2] <- 0.104004375

  expect_equal(mortalite, attendu)
})
