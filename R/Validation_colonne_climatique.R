#' Vérifier les noms de colonnes des variables climatiques
#'
#'
#' @param data Un dataframe représentant les données climatiques annuelles.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @export
#'
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
      erreurs[[col]] <- paste(col, "est manquant dans les donn\u00E9es")
    }
  }
 return(erreurs)
}


#' Vérifier les noms de colonnes des variables climatiques
#'
#'
#' @param data Un dataframe représentant les données climatiques mensuelles.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @export
#'
verifier_colonnes_Clim <- function(data) {

  data<- renommer_les_colonnes_climat_mensuel(data)

  specs <- list(
    Annee = list(type = "integer", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Mois = list(type = "integer", obligatoire = TRUE, valeurs_possibles = 1:12, validation = "valeurs dans la plage possible"),
    rcp = list(type = "character", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    PTot = list(type = "numeric", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Tmin = list(type = "numeric", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL),
    Tmax = list(type = "numeric", obligatoire = TRUE, valeurs_possibles = NULL, validation = NULL)
  )

  erreurs <- list()


  for (col in names(specs)) {

    if (specs[[col]]$obligatoire && !(col %in% names(data))) {
      erreurs[[col]] <- paste(col, "est manquant dans les donn\u00E9es")
      next
    }

    if (col %in% names(data)) {
      type_actuel <- class(data[[col]])
      type_attendu <- specs[[col]]$type
      if (type_actuel != type_attendu) {
        erreurs[[col]] <- paste(col, "type incorrect :", "Attendu :", type_attendu, "mais obtenu :", type_actuel)
      }


      if (!is.null(specs[[col]]$valeurs_possibles) && any(!data[[col]] %in% specs[[col]]$valeurs_possibles)) {
        erreurs[[col]] <- paste(col, "contient des valeurs en dehors de la plage autoris\u00E9e :", paste(specs[[col]]$valeurs_possibles, collapse = ", "))
      }
    }
  }


 return(erreurs)
}



#' Valider la colonne Mois. Il doit avoir 12 mois par année présentes.
#'
#'
#' @param data Un dataframe représentant les données climatiques mensuelles.
#'
#' @return une liste des années qui n'ont pas 12 mois
#'
#' @export
#'
valider_Mois <- function(data, scenario_rcp) {
    data <- renommer_les_colonnes_climat_mensuel(data)

    erreurs <- list()

    validation <- data %>% filter(rcp == scenario_rcp) %>%
      group_by(Annee,PlacetteID ) %>%
      summarise(
        nb_mois = n_distinct(Mois),
        .groups = "drop"
      )  %>%
      filter(nb_mois != 12)

    if(nrow(validation) > 0){
      for (i in seq_len(nrow(validation))){
        message <- paste(
          "Nombre de mois invalide :",
          validation$nb_mois[i],
          "mois pour Annee =", validation$Annee[i],
          ", PlacetteID =", validation$PlacetteID[i]
        )

        erreurs[[paste0("Annee_", validation$Annee[i],
                        "_Placette", validation$PlacetteID[i]
                        )]] <- message
      }
    }

    return (erreurs)
  }



#' Validation des données climatiques annuelles
#'
#'
#' @param data Un dataframe représentant les données
#' @param data_annuel Un dataframe représentant les données climatiques annuelles
#' @param scenario_rcp scenario rcp
#'
#' @return une liste des incohérences entre les données et les données climatiques
#'
#' @export
#'
validation_annuel <- function(data, data_annuel, scenario_rcp) {
  data <- renommer_les_colonnes(data)
  data_annuel <- renommer_les_colonnes_climat_annuel(data_annuel)

  erreurs <- list()

  # Filtrer selon le scénario
  data_annuel  <- data_annuel  %>% filter(rcp == scenario_rcp)

  # Validation données présentes
  if (nrow(data_annuel) == 0){
    erreurs[["data_annuel_vide"]] <- paste( "Aucune donnée climatique annuelle pour le scénario ", scenario_rcp )
  }

  else{
    # Validation année vs placetteId
    nb_ann <- n_distinct(data_annuel$Annee)

    resultat_annuel <- data_annuel %>%
      filter(PlacetteID %in% data$PlacetteID) %>%
      distinct(PlacetteID, Annee) %>%
      count(PlacetteID) %>%
      pull(n) %>%
      all(. == nb_ann)


    if (!resultat_annuel) {
      erreurs[["annee_manquante_annuel"]] <- paste( "Il manque des placettes dans le fichier annuel pour certaines années" )
    }
  }

  return(erreurs)
}

#' Validation des données climatiques mensuelles
#'
#'
#' @param data Un dataframe représentant les données
#' @param data_mensuel Un dataframe représentant les données climatiques mensuelles
#' @param scenario_rcp scenario rcp
#'
#' @return une liste des incohérences entre les données et les données climatiques
#'
#' @export
#'
validation_mensuel <- function(data, data_mensuel, scenario_rcp) {
  data <- renommer_les_colonnes(data)
  data_mensuel <- renommer_les_colonnes_climat_mensuel(data_mensuel)

  erreurs <- list()

  # Filtrer selon le scénario
  data_mensuel  <- data_mensuel  %>% filter(rcp == scenario_rcp)

  # Validation données présentes
  if (nrow(data_mensuel) == 0){
    erreurs[["data_mensuel_vide"]] <- paste( "Aucune donnée climatique mensuelle pour le scénario ", scenario_rcp )
  }

  else{
    # Validation année vs placetteId

    nb_ann <- n_distinct(data_mensuel$Annee)

    resultat_mensuel <- data_mensuel %>%
      filter(PlacetteID %in% data$PlacetteID) %>%
      distinct(PlacetteID, Annee) %>%
      count(PlacetteID) %>%
      pull(n) %>%
      all(. == nb_ann)


    if (!resultat_mensuel) {
      erreurs[["annee_manquante_mensuel"]] <- paste( "Il manque des placettes dans le fichier mensuel pour certaines années" )
    }
  }
  return(erreurs)
}

#' Comparer le nombre d'annee présent dans les données climatique annuelles et mensuelles selon le scénario
#'
#'
#' @param data Un dataframe représentant les données
#' @param data_annuel Un dataframe représentant les données climatiques annuelles
#' @param data_mensuel Un dataframe représentant les données climatiques mensuelles
#' @param scenario_rcp scenario rcp
#'
#' @return une liste des incohérences annuelles-mensuelles
#'
#' @export
#'
comparer_annee_scenario <- function(data, data_annuel, data_mensuel, scenario_rcp) {
  data <- renommer_les_colonnes(data)
  data_annuel <- renommer_les_colonnes_climat_annuel(data_annuel)
  data_mensuel <- renommer_les_colonnes_climat_annuel(data_mensuel)

  erreurs <- list()
  data_annuel  <- data_annuel  %>% filter(rcp == scenario_rcp)
  data_mensuel  <- data_mensuel  %>% filter(rcp == scenario_rcp)

  # Comparer les années précentes dans le fichier
  annee_annuel <- sort(unique(data_annuel$Annee))
  annee_mensuel <- sort(unique(data_mensuel$Annee))

  if (!identical(annee_annuel, annee_mensuel)) {
    erreurs[["diff_annee"]] <- paste( "Les valeurs des années annuelles vs mensuelles ne correspondent pas" )
  }

    return (erreurs)
  }



