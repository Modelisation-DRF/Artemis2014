#' Fonction qui extrait les variables climatiques de la période de simulation en cours.
#' Elle permet de produire les variables climatiques nécessaires aux modules de mortalité,
#' de croissance et de recrutement sensibles au climat.
#'
#'
#' @param Placettes Identifiant de la placette pour laquelle on veut extraire les
#'                  variables climatiques.
#'
#' @param Annee Année de debut de la période de simulation en cours.
#'
#' @param AnneeDep Année de départ de la simulation Artémis.
#'
#' @param t Durée de la période de simulation (habituellement 10 ans).
#'
#'
#' @param ClimTotPe Un dataframe contenant des prévisions climatiques annuelles des
#'                raster de biosim du package ExtractMap. Ce fichier contient
#'                toutes les variables nécessaires aux simulations de toutes les
#'                fonctions
#'
#'@param EvolClim Paramètre qui prend la valeure  0 pour un climat constant et
#'                de 1 pour une évolution du climat à travers le temps de
#'                simulation. Valeure par défaut de 0.
#'
#' @return Retourne un dataframe avec l'identifiant de la placette et les données
#'          climatiques formatées pour la période de simulation en cours.
#'
#' @export
#'
ClimatBiosimRaster<-function (Placettes, Annee, AnneeDep, t, ClimTotPe, EvolClim){

if (Annee<2091){
 An=Annee}else{
   An=2090
 }


if(EvolClim==1){ #methode pour evolution climatique

VarAn<-ClimTotPe%>%
       filter(Annee>=An & Annee<=(An+t)) %>%
       group_by(PlacetteID) %>%
       summarise(FFP=mean(FFP), PTotPeriode=mean(PTot), TMoyPeriode=mean(TMoy),
                 Tmax_yr=mean(Tmax_yr),CMI=mean(CMI), Aridity=mean(Aridity),
                 DD=mean(DD), CMIcm=mean(CMIcm), TotalVPD=mean(TotalVPD),
                 UtilVPD=mean(UtilVPD), Max_ST=mean(Max_ST), Min_WT=mean(Min_WT),
                 MSP=mean(MSP), PAS=mean(PAS), PUtile=mean (PUtile),
                 TmaxUtil=mean(TmaxUtil),TSummer=mean(TSummer)) %>%
       mutate(Snow_cat=ifelse(PAS<140,"low",ifelse(PAS<=200,"medium","high")))

suppressMessages(
  Placettes<-data.frame("PlacetteID"=Placettes) %>% left_join(VarAn))


}else{ #Climat historique calculé sur la période allant de 1991 au début de la simulation


  VarAn<-ClimTotPe %>%
         filter(Annee>=1991 & Annee<=AnneeDep) %>% #######Période de référence pour les courbes changé était entre 2000 et 1991 avant
         group_by(PlacetteID) %>%
         summarise(FFP=mean(FFP), PTotPeriode=mean(PTot), TMoyPeriode=mean(TMoy),
                   Tmax_yr=mean(Tmax_yr),CMI=mean(CMI), Aridity=mean(Aridity),
                   DD=mean(DD), CMIcm=mean(CMIcm), TotalVPD=mean(TotalVPD),
                   UtilVPD=mean(UtilVPD), Max_ST=mean(Max_ST), Min_WT=mean(Min_WT),
                   MSP=mean(MSP), PAS=mean(PAS), PUtile=mean (PUtile),
                   TmaxUtil=mean(TmaxUtil),TSummer=mean(TSummer)) %>%
         mutate(Snow_cat=ifelse(PAS<140,"low",ifelse(PAS<=200,"medium","high")))

  suppressMessages(
    Placettes<-data.frame("PlacetteID"=Placettes) %>% left_join(VarAn))
}

  return(Placettes)
}


