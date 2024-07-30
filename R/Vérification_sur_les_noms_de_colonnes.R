

#' Renommer les colonnes  du fichier des arbres
#'
#' La fonction \code{renommer_les_colonnes} renomme les colonnes d'un dataframe
#'
#' @param data Un dataframe, représentant le fichier des arbres, dont les colonnes doivent être renommées et réorganisées.
#'
#'
#' @details
#' La fonction suit les étapes suivantes :
#' \itemize{
#'   \item Définir un vecteur \code{ColOrdre} contenant les noms des colonnes souhaitées dans l'ordre désiré.
#'   \item Convertir les noms des colonnes existantes du dataframe en minuscules pour faciliter la correspondance.
#'   \item Convertir les noms des colonnes souhaitées en minuscules.
#'   \item Pour chaque nom de colonne souhaité, chercher sa correspondance parmi les noms des colonnes existantes.
#'   \item Si une correspondance est trouvée, renommer la colonne existante avec le nom souhaité.
#'   \item Retourner le dataframe avec les noms de colonnes mis à jour.
#' }
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `data` avec les colonnes suivantes :
#' # "Sdom_Bio","Veg_Pot","Latitude","Longitude","Altitude","Pente","PTot","TMoy",
#' # "GrwDays","Reg_Eco","Type_Eco", "Cl_Drai","Exposition","Age_moy","sand_015cm","cec_015cm"
#' # Appel de la fonction
#' data_renomme <- renommer_les_colonnes(data)
#'
#' # Le dataframe `data_renomme` aura les colonnes renommées et réorganisées selon `ColOrdre`.
#' }
#'
#' @export

renommer_les_colonnes <- function(data){

  ColOrdre<-c("PlacetteID","no_mes","origTreeID","Espece","Etat","DHPcm","Dom_Bio","Nombre",
              "Sdom_Bio","Veg_Pot","Latitude","Longitude","Altitude","Pente","PTot","TMoy",
              "GrwDays","Reg_Eco","Type_Eco", "Cl_Drai","Exposition","Age_moy","sand_015cm","cec_015cm")

  noms_colonnes_existants <- tolower(names(data))
  noms_colonnes_desires <- tolower(ColOrdre)

  for (i in seq_along(noms_colonnes_desires)) {
    index_colonne <- match(noms_colonnes_desires[i], noms_colonnes_existants)
    if (!is.na(index_colonne)) {
      names(data)[index_colonne] <- ColOrdre[i]
    }
  }

  return(data)
}

