

verifier_colonnes_ClimAn <- function(data) {

  data<- renommer_les_colonnes_climat_annuel(data)

  types_attendus <- list(
    Annee = "integer",
    rcp = "character",
    Aridity = "numeric",
    CMI = "numeric",
    DD = "numeric",
    FFP = "numeric",
    PTot = "numeric",
    Tmax_yr = "numeric",
    TMoy = "numeric"
  )


  erreurs <- list()


  for (col in names(types_attendus)) {
    if (col %in% names(data)) {
      type_actuel <- class(data[[col]])
      type_attendu <- types_attendus[[col]]
      if (type_actuel != type_attendu) {
        erreurs[[col]] <- paste(col, "type incorrect :", "Attendu :", type_attendu, "mais obtenu :", type_actuel)
      }
    } else {
      erreurs[[col]] <- paste(col, "est manquant dans le data frame")
    }
  }
 return(erreurs)
}



verifier_colonnes_Clim <- function(data) {

  data<- renommer_les_colonnes_climat_mensuel(data)

  specs <- list(
    Annee = list(type = "integer", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Mois = list(type = "integer", obligatoire = TRUE, valeurs_possibles = 1:12, validation = "valeurs dans la plage possible"),
    rcp = list(type = "character", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Ptot = list(type = "numeric", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Tmin = list(type = "numeric", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Tmax = list(type = "numeric", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL)
  )

  erreurs <- list()


  for (col in names(specs)) {

    if (specs[[col]]$obligatoire && !(col %in% names(data))) {
      erreurs[[col]] <- paste(col, "est manquant dans le data frame")
      next
    }

    if (col %in% names(data)) {
      type_actuel <- class(data[[col]])
      type_attendu <- specs[[col]]$type
      if (type_actuel != type_attendu) {
        erreurs[[col]] <- paste(col, "type incorrect :", "Attendu :", type_attendu, "mais obtenu :", type_actuel)
      }


      if (!is.null(specs[[col]]$valeurs_possibles) && any(!data[[col]] %in% specs[[col]]$valeurs_possibles)) {
        erreurs[[col]] <- paste(col, "contient des valeurs en dehors de la plage autorisée :", paste(specs[[col]]$valeurs_possibles, collapse = ", "))
      }
    }
  }

 return(erreurs)
}


