#'Vérifier les noms de colonnes des variables climatiques du fichier climTot
#'
#'
#' @param data Un dataframe représentant les données climatiques totales.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @export
#'
verifier_colonnes_ClimTot <- function(data) {

  data<- renommer_les_colonnes_climat_total(data)

  types_attendus <- list(
    Annee = "integer", rcp = "character", Aridity = "numeric", CMI = "numeric", CMIcm = "numeric",
    DD = "numeric", FFP = "numeric", MSP = "numeric", Max_ST = "numeric",Min_WT = "numeric",
    PAS = "numeric", PTot = "numeric", PUtile = "numeric", TMoy = "numeric",
    TSummer = "numeric", TmaxUtil = "numeric", Tmax_yr = "numeric", TotalVPD = "numeric", UtilVPD = "numeric")


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



#' Validation des données climatiques totales
#'
#'
#' @param data Un dataframe représentant les données
#' @param data_annuel Un dataframe représentant les données climatiques totales
#' @param scenario_rcp scenario rcp
#'
#' @return une liste des incohérences entre les données et les données climatiques
#'
#' @export
#'
validation_total <- function(data, data_total, scenario_rcp) {
  data <- renommer_les_colonnes(data)
  data_total <- renommer_les_colonnes_climat_total(data_total)

  erreurs <- list()

  # Filtrer selon le scénario
  #data_total  <- data_total  %>% filter(rcp == scenario_rcp)
  data_total <- dplyr::filter(data_total, rcp == scenario_rcp)

  # Validation données présentes
  if (nrow(data_total) == 0){
    erreurs[["data_total_vide"]] <- paste( "Aucune donnée climatique pour le scénario ", scenario_rcp )
  }

  else{
    # Validation année vs placetteId
    nb_ann <- n_distinct(data_total$Annee)

    resultat_total <- data_total %>%
      filter(PlacetteID %in% data$PlacetteID) %>%
      distinct(PlacetteID, Annee) %>%
      count(PlacetteID) %>%
      pull(n) %>%
      all(. == nb_ann)


    if (!resultat_total) {
      erreurs[["annee_manquante_annuel"]] <- paste( "Il manque des placettes dans le fichier climat pour certaines années" )
    }
  }

  return(erreurs)
}


