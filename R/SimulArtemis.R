
#'Fonction qui sert à appeler le simulateur Artemis et qui fournit les données
#'initiales ainsi qu'un choix de paramètres pour la simulation.
#'
#' @param Tendance # Tendance si =1 modifie les paramètres d'accroissement, de mortalité et de recrutement pour les vegetations
#'               FE2 et FE3 afin de n'utiliser que les intevalles de croissance se terminant après 1998
#'
#' @param Horizon Valeur numérique du nombre de périodes de 10 ans sur lesquelles
#'                le simulateur effectuera ses simulations (ex: 3 pour 30 ans de simulation).
#'
#' @param Residuel # Residuel si =1 placette avec coupe partielle deuis moins de 10 ans
#'
#' @param Data_ori Un dataframe contenant les valeurs de départ pour une liste
#'             d'arbres à simuler. Les champs: "PlacetteID","no_mes","origTreeID","Espece","Etat",
#'             "DHPcm","Dom_Bio","Nombre","Sdom_Bio","Veg_Pot","Latitude","Longitude","Altitude",
#'             "Pente","PTot","TMoy","GrwDays","Reg_Eco","Type_Eco", "Cl_Drai","Exposition",
#'             "Age_moy","sand_015cm","cec_015cm" doivent être présents. Si l'information
#'             sur certains champs n'est pas disponible, on peut le laisser vide.
#'
#'
#' @param FacHa # FacHa facteur d'espantion de la placette à l'hectare valeur par defaut à 25
#                 Clim sorties mensuelles de climat si abscente laisser vide
#                 ClimAn sorties annuelles de climat si abscente Laisser vide
#'
#' @param Clim
#' @param ClimAn
#'
#' @param EvolClim  # EvolClim valeur de 0 pour climat constant et de 1 pour évolution du climat
#'
#' @param AccModif # AccModif "BRT" pour les équations Boosted regression tree de JieJie Wang 2022, "GAM" pour les GAM de D'Orangeville 2019, "ORI' pour les équations originale d'Artemis
#'
#' @param MortModif # MortModif "ORI" pour les équation originales d'Artemis, "QUE" pour les équation calibrées par essence sensibles au climat "BRT"
#'
#' @param RCP  # RCP Scénario climatique choisi pour la simulation
#'
#' @return Retourne un dataframe contenant la liste des arbres, leur état, leur DHP,
#'         leur hauteur et leur volume pour chaque placette
#'
#' @examples
#' result <- simulateurArtemis(Data_ori = Data, Horizon = 3, Tendance = 0, Residuel = 0, FacHa = 25, EvolClim = 1, AccModif='GAM', MortModif='QUE', RCP='RCP45' )
#' print(result)
#' @export
#'


simulateurArtemis<-function(Data_ori,Horizon,Clim,ClimAn,Tendance=0,Residuel=0,FacHa=25,EvolClim=0,AccModif='ORI',MortModif='ORI',RCP='RCP45'){


  Para <- Para %>% mutate(Effect = str_to_lower(Effect))
  AnneeDep <- as.numeric(format(Sys.Date(), "%Y"))

  #Data_ori<-Data_ori %>% filter(Veg_Pot!="RE1")

  Data_ori<-renommer_les_colonnes(Data_ori)

  if (!missing(Clim)) {

  Clim<- renommer_les_colonnes_climat_mensuel(Clim)
  }

  if (!missing(ClimAn)) {
  ClimAn<- renommer_les_colonnes_climat_annuel(ClimAn)
  }

  Data_ori<-vevifier_variable_meteo(Data_ori)

  Data_ori<-vevifier_variable_Sol(Data_ori)

  Data_ori<-vevifier_variable_Sation(Data_ori)

  prep_data <- PrepareData(Data_ori, Clim, ClimAn, AccModif, EvolClim, MortModif, RCP, SpInd, ListeVp, SpGroups, Sp)
  Data <- prep_data[[1]]
  Models <- prep_data[[2]]
  Clim <- prep_data[[3]]
  ClimAn <- prep_data[[4]]
  rm(prep_data)

  registerDoFuture()
  list_plot <- unique(Data$PlacetteID)
  plan(multisession)
  Final<- bind_rows(
    foreach(x = iter(list_plot), .packages = c("gbm"))  %dorng%
      {ArtemisClimat(Para=Para,  Data=Data[Data$PlacetteID==x,],
                     AnneeDep=AnneeDep, Horizon=Horizon, FacHa=FacHa, Tendance=Tendance, Residuel=Residuel, Clim=Clim, ClimAn =ClimAn,
                     EvolClim =EvolClim, AccModif=AccModif, MortModif= MortModif, RCP=RCP, Models = Models)}
  )


  if (EvolClim==1){


    PTotTMoyEvol<-ClimAn %>%
      mutate(Annee=Annee-1) %>%
      select(PlacetteID,Annee,PTot,TMoy)########Ajuste la température et les précipitations pour le calcul de la hauteur

    Final<-Final %>%
      select(-PTot,-TMoy) %>%
      inner_join(PTotTMoyEvol)
    rm(PTotTMoyEvol)

  } else{

    Final<-Final

  }

  Final<-Final %>%
    mutate(milieu=substr(Type_Eco,4,4)) %>%
    rename(id_pe=PlacetteID, dhpcm=DHPcm, essence=GrEspece,no_arbre=origTreeID, nb_tige=Nombre,
           altitude=Altitude,veg_pot=Veg_Pot,p_tot=PTot,t_ma=TMoy, reg_eco=Reg_Eco)

  nb_periodes <- Horizon+1

  ht <- relation_h_d(fic_arbres=Final, mode_simul='DET', nb_step=nb_periodes, reg_eco = TRUE, dt =10)


  Final2 <- cubage(fic_arbres=ht, mode_simul='DET', nb_step=nb_periodes)  %>%
    rename(PlacetteID=id_pe, DHPcm=dhpcm, GrEspece=essence, origTreeID=no_arbre, Nombre=nb_tige,
           Altitude=altitude, Veg_Pot=veg_pot, PTot=p_tot, TMoy=t_ma)

  return(Final2)

}
