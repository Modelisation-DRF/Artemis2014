test_that("SortieSybille fonctionne sans erreur", {
  # Data
  test_data <- data.frame(
    Veg_Pot = c("MS2", "RS2"),
    PlacetteID = c(1, 1),
    DHPcm = c(15, 25),
    Altitude = c(450, 450),
    hauteur_pred = c(12, 18),
    origTreeID = c(1, 2),
    Espece = c("SAB", "EPN"),
    Cl_Drai = c("2", "3"),
    Etat = c("vivant", "vivant"),
    Nombre = c(1, 1),
    sdom_bio = c("4E", "4O"),
    Annee = c(2020, 2020)
  )

  expect_error(
    SortieSybille(
      test_data,
      nom_grade1 = "pate",
      long_grade1 = 8,
      diam_grade1 = 4
    ),
    NA  # NA veut dire aucune erreur
  )
})
