


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
             ST_m2,Vol_dm3) %>%
    arrange(PlacetteID,Annee,origTreeID)


  if(simplifier == TRUE){
    ArbreSamare_simp_min <-ArbreSamare %>% filter(Annee==MinAnnee )
    ArbreSamare_simp_max <-ArbreSamare %>% filter(Annee==MaxAnnee )
    ArbreSamare <-rbind(ArbreSamare_simp_min,ArbreSamare_simp_max)%>%
      arrange(PlacetteID,Annee,origTreeID)
  }


  return (ArbreSamare)

}

#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à un groupe d'espèce par placette et par année avec une ligne présentant le total.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation du simulateur Artémis.
#'                   Typiquement un résultat retourné
#'                   par la fonction "simulateurArtemis".
#' @param simplifier Un booléen indiquant si les résultats doivent être simplifiés pour ne garder
#'                  que la première et la dernière année de la simulation. Par défaut, \code{FALSE}.
#' @return  Retourne un dataframe contenant le nombre de tiges par hectare, la surface terrière par hectare,
#'          le diamètre moyen quadratique, le volume marchand brut par hectare, la hauteur dominante et la hauteur moyenne
#'          par placette, groupe d'espèce et année.
#' @examples
#' #resultat <- Sortieplacette(SimulHtVol ,simplifier=FALSE)
#' print(resultat)
#' @export



Sortieplacette <- function(SimulHtVol ,simplifier=FALSE){

  select=dplyr::select

  MinAnnee = min(SimulHtVol$Annee)
  MaxAnnee = max(SimulHtVol$Annee)

  suppressMessages(
  placetteSamareT <- SimulHtVol %>%
                    group_by(PlacetteID, Annee, Etat) %>%
                    mutate(NbCum=cumsum(Nombre)) %>%
                    summarise(nbTi_HA=sum(Nombre)*25,ST_HA=sum((DHPcm/200)^2*pi*Nombre)*25,DMQ=(ST_HA/nbTi_HA/pi)^0.5*200,Vol_HA=sum(vol_dm3/1000*Nombre,na.rm = TRUE)*25,
                              HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/0.04)>=100))],na.rm = TRUE),NA),
                              Hauteur_Moy=sum(hauteur_pred*Nombre)/sum(Nombre)) %>%
                    mutate(GrEspece="TOT"))




  suppressMessages(
  placetteSamare <- SimulHtVol %>%
                    group_by(PlacetteID, Annee, Etat, GrEspece) %>%
                    mutate(NbCum=cumsum(Nombre)) %>%
                    summarise(nbTi_HA=sum(Nombre)*25,ST_HA=sum((DHPcm/200)^2*pi*Nombre)*25,DMQ=(ST_HA/nbTi_HA/pi)^0.5*200,Vol_HA=sum(vol_dm3/1000*Nombre)*25,
                              HDomM=ifelse(nbTi_HA>100,mean(hauteur_pred[1:first(which((NbCum/0.04)>=100))],na.rm = TRUE),NA),
                              Hauteur_Moy=sum(hauteur_pred*Nombre)/sum(Nombre)) %>%
                    rbind(placetteSamareT) %>%
                    arrange(PlacetteID,Annee,GrEspece,desc(Etat)))


  if(simplifier == TRUE){
    placetteSamare_simp_min <-placetteSamare %>% filter(Annee==MinAnnee )
    placetteSamare_simp_max <-placetteSamare %>% filter(Annee==MaxAnnee )
    placetteSamare <-rbind(placetteSamare_simp_min,placetteSamare_simp_max) %>%  arrange(PlacetteID,Annee,GrEspece,desc(Etat))

  }
  return(placetteSamare)

}
