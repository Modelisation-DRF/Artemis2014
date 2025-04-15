

#' Fonction qui permet de renommer les colonnes  du fichier d'intrant qui contient
#' la liste d'arbres à simuler.
#'
#' La fonction \code{renommer_les_colonnes} renomme les colonnes d'un dataframe
#'
#' @param data Un dataframe, contenant la liste d'arbres pour débuter la simulation
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

  ColOrdre <- c("PlacetteID", "origTreeID", "Espece", "DHPcm", "Nombre", "Etat", "Dom_Bio",
                "Sdom_Bio", "Reg_Eco", "Cl_Drai", "Veg_Pot", "Type_Eco", "Latitude", "Longitude",
                "Altitude", "PTot", "TMoy", "sand_015cm", "cec_015cm", "Age_moy", "Pente", "Exposition")


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





#' Vérifier la présence des colonnes obligatoires dans le fichier d'arbres
#'
#' La fonction \code{trouver_noms_absents} vérifie si toutes les colonnes obligatoires
#' sont présentes dans un dataframe représentant le fichier d'arbres. Elle retourne une liste
#' des noms des colonnes manquantes, le cas échéant.
#'
#' @param data Un dataframe représentant le fichier d'arbres.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @examples noms_absents_data<-trouver_noms_absents (Data, Mort_Modif,Acc_Modif)
#'
#' @export
#'
trouver_noms_absents <- function(Data, Mort_Modif,Acc_Modif) {

  ColOrdre1<- c("PlacetteID", "origTreeID", "Espece", "DHPcm", "Nombre", "Etat",
                "Reg_Eco", "Cl_Drai", "Veg_Pot", "Type_Eco", "Latitude", "Longitude",
                "Altitude", "Age_moy")

  ColOrdre2<- c("PlacetteID", "origTreeID", "Espece", "DHPcm", "Nombre", "Etat",
               "Reg_Eco", "Cl_Drai", "Veg_Pot", "Type_Eco", "Latitude", "Longitude",
               "Altitude")

  if(Mort_Modif=="QUE" || Mort_Modif=="BRT" || Acc_Modif=="GAM"){

    ColOrdre <- ColOrdre1
  }else{
    ColOrdre <- ColOrdre2
  }

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}







#' Vérifier la présence des colonnes obligatoires dans le fichier climat mensuel.
#'
#' La fonction \code{trouver_noms_absents_Climat_mensuel} vérifie si toutes les colonnes obligatoires
#' sont présentes dans un dataframe représentant le fichier climat mensuel. Elle retourne une liste
#' des noms des colonnes manquantes, le cas échéant.
#'
#' @param data Un dataframe représentant le fichier climat mensuel.
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @examples noms_absents_ClimMois<-trouver_noms_absents_Climat_mensuel(Data)
#'
#' @export
#'
trouver_noms_absents_Climat_mensuel <- function(Data) {

  ColOrdre <- c("PlacetteID","Annee","Mois","rcp","PTot","Tmin","Tmax")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}






#' Renommer les colonnes  du fichier des climats mensuels
#'
#' La fonction \code{renommer_les_colonnes_climat_mensuel} renomme les colonnes d'un dataframe
#'
#' @param data Un dataframe, représentant le fichier des climats mensuels, dont les colonnes doivent être renommées et réorganisées.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `data` avec les colonnes suivantes :
#' # "PlacetteID","Annee","Mois","rcp","Ptot","Tmin","Tmax"
#' # Appel de la fonction
#' data_renomme <- renommer_les_colonnes_climat_mensuel(data)
#'
#' # Le dataframe `data_renomme` aura les colonnes renommées et réorganisées selon `ColOrdre`.
#' }
#'
#' @export

renommer_les_colonnes_climat_mensuel <- function(data){

  ColOrdre <- c("PlacetteID","Annee","Mois","rcp","PTot","Tmin","Tmax")


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







#' Vérifier la présence des colonnes obligatoires dans le fichier climat annuel
#'
#' La fonction \code{trouver_noms_absents_Climat_annuel} vérifie si toutes les colonnes obligatoires
#' sont présentes dans un dataframe représentant le fichier climat annuel. Elle retourne une liste
#' des noms des colonnes manquantes, le cas échéant.
#'
#' @param data Un dataframe représentant le fichier climat annuel
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @examples noms_absents_ClimAn<-trouver_noms_absents_Climat_annuel(Data)
#'
#' @export
#'
trouver_noms_absents_Climat_annuel <- function(Data) {

  ColOrdre <- c("PlacetteID","Annee","rcp","Aridity","CMI","DD","FFP","PTot","Tmax_yr","TMoy")

  names(Data) <- tolower(names(Data))

  colone_minuscule <- lapply(ColOrdre, tolower)

  noms_absents <- setdiff(colone_minuscule, names(Data))

  return(noms_absents)

}






#' Renommer les colonnes  du fichier des climats annuel
#'
#' La fonction \code{renommer_les_colonnes_climat_annuel} renomme les colonnes d'un dataframe
#'
#' @param data Un dataframe, représentant le fichier des climats annuel, dont les colonnes doivent être renommées et réorganisées.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `data` avec les colonnes suivantes :
#' # "PlacetteID","Annee","rcp","Aridity","CMI","DD","FFP","PTot","Tmax_yr","TMoy"
#' # Appel de la fonction
#' data_renomme <- renommer_les_colonnes_climat_annuel(data)
#'
#' # Le dataframe `data_renomme` aura les colonnes renommées et réorganisées selon `ColOrdre`.
#' }
#'
#' @export

renommer_les_colonnes_climat_annuel <- function(data){

  ColOrdre <- c("PlacetteID","Annee","rcp","Aridity","CMI","DD","FFP","PTot","Tmax_yr","TMoy")


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
