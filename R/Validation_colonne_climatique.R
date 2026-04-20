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
valider_Mois <- function(data) {
    data <- renommer_les_colonnes_climat_annuel(data)

    erreurs <- list()

    validation <- data %>%
      group_by(rcp,Annee,PlacetteID ) %>%
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
          "mois pour rcp =", validation$rcp[i],
          ", Annee =", validation$Annee[i],
          ", PlacetteID =", validation$PlacetteID[i]
        )

        erreurs[[paste0("Annee_", validation$Annee[i],
                        "_Placette_", validation$PlacetteID[i],
                        "_rcp_", validation$rcp[i])]] <- message
      }
    }

    return (erreurs)
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

  # Filtrer selon le scénario
  data_annuel  <- data_annuel  %>% filter(rcp == scenario_rcp)
  data_mensuel <- data_mensuel %>% filter(rcp == scenario_rcp)

  if (nrow(data_annuel) == 0){
    erreurs[["data_annuel_vide"]] <- paste( "Aucune donnée climatique annuelle pour le scénario ", scenario_rcp )
  }
  if (nrow(data_mensuel) == 0){
    erreurs[["data_mensuel_vide"]] <- paste( "Aucune donnée climatique mensuelle pour le scénario ", scenario_rcp )
  }

  # Comparer les années précentes dans le fichier
  annee_annuel <- sort(unique(data_annuel$Annee))
  annee_mensuel <- sort(unique(data_mensuel$Annee))

  if (!identical(annee_annuel, annee_mensuel)) {
    erreurs[["diff_annee"]] <- paste( "Les valeurs des années annuelles vs mensuelles ne correspondent pas" )
  }

  # resultat_annuel <- data_annuel %>%
  #   group_by(rcp) %>%
  #   summarise(
  #     nb_annees = n_distinct(Annee),
  #     .groups = "drop"
  #   )
  #
  # resultat_mensuel <- data_mensuel %>%
  #   group_by(rcp) %>%
  #   summarise(
  #     nb_annees = n_distinct(Annee),
  #     .groups = "drop"
  #   )
  #
  # cible <- tibble(rcp = scenario_rcp)
  #
  # comparaison <- cible %>%
  #   left_join(resultat_annuel, by = "rcp") %>% rename(nb_annuel = nb_annees) %>%
  #   left_join(resultat_mensuel, by = "rcp") %>% rename(nb_mensuel = nb_annees) %>%
  #   mutate(
  #     nb_annuel = coalesce(nb_annuel, 0L),
  #     nb_mensuel = coalesce(nb_mensuel, 0L),
  #     nb_annee_coherent = (nb_annuel == nb_mensuel) & (nb_annuel > 0)
  #   )


    return (erreurs)
  }



