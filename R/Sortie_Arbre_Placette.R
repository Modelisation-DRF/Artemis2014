


#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette et par année.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation du simulateur Artémis.
#'                   Typiquement un résultat retourné
#'                   par la fonction "simulateurArtemis".
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                  que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return  Retourne un dataframe contenant l'ensemble des arbres pour chacune des
#'          placettes, années.
#' @examples
#' #resultat <- SortieArbre(SimulHtVol ,simplifier=FALSE)
#' print(resultat)
#' @export


SortieArbre <- function(SimulHtVol ,simplifier=FALSE){

  select=dplyr::select

  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)

  ArbreSamare <- SimulHtVol %>%
    mutate (Stm2=pi*(DHPcm/200)^2,

          ) %>%
    select(PlacetteID, Annee,origTreeID, Nombre, GrEspece, Espece,
           Etat, DHPcm, hauteur_pred, vol_dm3, Stm2) %>%
    rename(ST_m2=Stm2, Vol_dm3=vol_dm3, Hautm=hauteur_pred) %>%
    relocate(PlacetteID,Annee,origTreeID, Espece,GrEspece, Etat, Nombre,DHPcm,Hautm,
             ST_m2,Vol_dm3)


  if(simplifier == TRUE){
    ArbreSamare_simp_min <-ArbreSamare %>% filter(Annee==MinAnnee )
    ArbreSamare_simp_max <-ArbreSamare %>% filter(Annee==MaxAnnee )
    ArbreSamare <-rbind(ArbreSamare_simp_min,ArbreSamare_simp_max)
  }


  return (ArbreSamare)

}
