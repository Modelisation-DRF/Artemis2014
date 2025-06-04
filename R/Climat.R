#' Fonction qui extrait les variables climatiques de la période de simulation en cours.
#' Elle permet de produire les variables climatiques nécessaires aux modules de mortalité,
#' de croissance et de recrutement sensibles au climat.
#'
#'
#' @param Placettes Identifiant de la placette pour laquelle on veut extraire les variables
#'                  climatiques.
#'
#' @param Annee Année de debut de la période de simulation en cours.
#'
#' @param t Duree de la periode de simulation (habituellement 10 ans).
#'
#' @param ClimMois Un dataframe contenant des prévisions climatiques mensuelles issues du logiciel biosim
#'                dans lequel il y a une correspondance avec l'identifiant de placette et la
#'                valeure de l'argument année de la fonction.
#'
#' @param ClimAn Un dataframe contenant des prévisions climatiques annuelles issues du logiciel biosim
#'                dans lequel il y a une correspondance avec l'identifiant de placette et la
#'                valeure de l'argument année de la fonction.
#'
#'@param EvolClim Paramètre qui prend la valeure  0 pour un climat constant et de 1 pour une évolution du
#'                 climat à travers le temps de simulation. Valeure par défaut de 0.
#'
#'@param AccModif Choix de la fonction d'accroissement en diamètre "ORI" pour les
#'                 équations originales d'Artémis-2014, "BRT" pour les équations
#'                 Boosted regression tree de JieJie Wang 2022, "GAM" pour les
#'                 GAM de D'Orangeville 2019.
#'
#'@param RCP Scénario climatique choisi pour la simulation soit RCP 4.5 ou RCP 8.5.
#'           Ce paramètre est seulement utilisé si le paramètre EvolClim=1.
#'
#' @return Retourne un dataframe avec l'identifiant de la placette et les données
#'          climatiques formatées pour la période de simulation en cours.
#'
#' @export
#'
ClimatBiosim<-function (Placettes, Annee, t, RCP, ClimMois, ClimAn, EvolClim, AccModif){

if (Annee<2091){
 An=Annee}else{
   An=2090
 }
   # if (exists("Clim")==FALSE | exists("ClimAn")==FALSE){
  # suppressMessages(
  # ClimatFormat<-Placettes %>%
  #               group_by(PlacetteID,Latitude,Longitude,Altitude) %>%
  #               summarise() %>%
  #               ungroup %>%
  #               filter(is.na(Altitude)==FALSE & is.na(Latitude)==FALSE & is.na(Longitude)==FALSE) %>%
  #               mutate(Altitude=as.numeric(Altitude), Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude))
  # )
  #
  # }
  #
  # if (exists("Clim")==FALSE){
  # Clim<-as.data.frame(generateWeather("Climatic_Monthly",Annee,Annee+t,ClimatFormat$PlacetteID, ClimatFormat$Latitude,
  #                           ClimatFormat$Longitude, ClimatFormat$Altitude,
  #                           rep=1, repModel=1,rcp,climModel))
  # }
  # if (exists("ClimAn")==FALSE){
  # ClimAn<-as.data.frame(generateWeather("ClimaticQc_Annual",Annee,Annee+t,ClimatFormat$PlacetteID, ClimatFormat$Latitude,
  #                                     ClimatFormat$Longitude, ClimatFormat$Altitude,
  #                                     rep=1, repModel=1,rcp,climModel))
  # }

  if(EvolClim==1){ #methode pour evolution climatique

VarMois<-ClimMois %>%
      filter(Annee>=An & Annee<=(An+t)) %>%
      group_by(PlacetteID) %>%
      nest() %>%
      mutate(Var=map(data,VarClimMois)) %>%
      unnest(Var) %>%
      select(-data)

VarAn<-ClimAn %>%
  filter(Annee>=An & Annee<=(An+t)) %>%
       group_by(PlacetteID) %>%
       summarise(FFP=mean(FFP), PTotPeriode=mean(PTot), TMoyPeriode=mean(TMoy),
                 Tmax_yr=mean(Tmax_yr),CMI=mean(CMI), Aridity=mean(Aridity), DD=mean(DD)) %>%
      mutate(Max_ST=VarMois[[2]][[1]],
              Min_WT=VarMois[[2]][[2]],
              MSP=VarMois[[2]][[3]],
              PAS=VarMois[[2]][[4]],
              PUtile=VarMois[[2]][[5]],
              TmaxUtil=VarMois[[2]][[6]],
              TSummer=VarMois[[2]][[7]],
             #CMI=ifelse(AccModif=="GAM",VarMois[[2]][[6]],NA),
             Snow_cat=ifelse(PAS<140,"low",ifelse(PAS<=200,"medium","high")))

suppressMessages(
  Placettes<-data.frame("PlacetteID"=Placettes) %>% left_join(VarAn))


}else{ #Climat historique calculé sur les 30 années préceédentes le début de simulation

  VarMois<-ClimMois %>%
    #filter(Annee>=(An-30) & Annee<=An) %>%
    filter(Annee>=1991 & Annee<=2000) %>% #######Période de référence pour les courbes actuellement utilisées corespond au milieu de la plage de 30 ans
    group_by(PlacetteID) %>%
    nest() %>%
    mutate(Var=map(data,VarClimMois)) %>%
    unnest(Var) %>%
    select(-data)

  VarAn<-ClimAn %>%
    #filter(Annee>=(An-30) & Annee<=An) %>%
    filter(Annee>=1991 & Annee<=2000) %>% #######Période de référence pour les courbes actuellement utilisées correspond au milieu de la plage de 30 ans
    group_by(PlacetteID) %>%
    summarise(FFP=mean(FFP),PTotPeriode=mean(PTot), TMoyPeriode=mean(TMoy),
              Tmax_yr=mean(Tmax_yr),CMI=mean(CMI), Aridity=mean(Aridity), DD=mean(DD)) %>%
    mutate(Max_ST=VarMois[[2]][[1]],
           Min_WT=VarMois[[2]][[2]],
           MSP=VarMois[[2]][[3]],
           PAS=VarMois[[2]][[4]],
           PUtile=VarMois[[2]][[5]],
           TmaxUtil=VarMois[[2]][[6]],
           TSummer=VarMois[[2]][[7]],
           #CMI=ifelse(AccModif=="GAM",VarMois[[2]][[8]],NA),
           Snow_cat=ifelse(PAS<140,"low",ifelse(PAS<=200,"medium","high")))

  suppressMessages(
    Placettes<-data.frame("PlacetteID"=Placettes) %>% left_join(VarAn))
}

  return(Placettes)
}

########################################################################
###########################Fonction VarClimMois utilisé par ClimatBiosim
########################################################################

VarClimMois<-function(DataMois){

  TempsCMI<-length(unique(DataMois$Annee))
  TmaxSummer<-mean(DataMois$Tmax[which(DataMois$Mois %in% c(5,6,7,8,9))])
  TSummer<-mean(c(DataMois$Tmax[which(DataMois$Mois %in% c(5,6,7,8,9))], DataMois$Tmin[which(DataMois$Mois %in% c(5,6,7,8,9))]))
  TmaxUtil<-mean(DataMois$Tmax[which(DataMois$Mois %in% c(6,7,8))])
  TminWinter<-mean(DataMois$Tmin[which(DataMois$Mois %in% c(1,2,3))])
 # PrecSummer<-mean(DataMois$PTot[which(DataMois$Mois %in% c(5,6,7,8,9))])
  PrecSummer<-sum(DataMois$PTot[which(DataMois$Mois %in% c(5,6,7,8,9))])/TempsCMI
  #Snow<-mean(DataMois$PTot[which(DataMois$Mois %in% c(1,2,3))])
  Snow<-sum(DataMois$PTot[which(DataMois$Mois %in% c(1,2,3))])/TempsCMI
  PUtile<-mean(DataMois$PTot[which(DataMois$Mois %in% c(6,7,8))])

  #  if (AccModif=="GAM"){
  #   DataPET<-DataMois%>%
  #     select(Mois, Latitude, Altitude, Tmin, Tmax, Vent2, Radiation, PTot) %>%
  #     mutate(Radiation=ifelse(Mois %in% c(1,3,5,7,8,10,12),Radiation/31,Radiation/30))
  #
  #
  #   PET<-as.vector(penman(Tmin=DataPET$Tmin,Tmax=DataPET$Tmax,U2=DataPET$Vent2,lat=DataPET$Latitude[1],
  #                         Rs=DataPET$Radiation,z=DataPET$Altitude[1],crop='short', na.rm = TRUE))
  #
  #
  #   TempsCMI<-length(unique(DataMois$Annee))
  #   CMI<- as.vector(DataPET %>%
  #                    mutate(PET2=PET) %>%
  #                    filter(Mois %in% c(5,6,7,8,9)) %>%
  #                    mutate(CMI=PTot-PET2) %>%
  #                    summarise(sum(CMI)/TempsCMI))#####Modifié pour correspondre à l'article de d'Orangeville (sum ppt may-sept -sum pet may-sept)
  #
  #
  #   Climat<-list(TmaxSummer,TminWinter,PrecSummer,Snow,PUtile, TmaxUtil, TSummer, CMI[[1]])
  # }else{
  #
  #   Climat<-list(TmaxSummer,TminWinter,PrecSummer,Snow,PUtile, TmaxUtil, TSummer)
  # }
  #
  Climat<-list(TmaxSummer,TminWinter,PrecSummer,Snow,PUtile, TmaxUtil, TSummer)

  return(Climat)
}
