
#'Fonction qui sert à appeler le simulateur Artemis 2014 et qui fournie les donnees
#'initiales ainsi qu'un choix de parametres pour la simulation.
#'
#' @param Tendance  Si =1 modifie les paramètres d'accroissement, de mortalité et de recrutement pour les vegetations
#'                  potentielles FE2 et FE3 afin de utiliser que les intevalles de croissance se terminant après 1998
#'
#' @param Horizon Valeur numérique du nombre de périodes de 10 ans sur lesquelles
#'                le simulateur effectuera ses simulations (ex: 3 pour 30 ans de simulation).
#'
#' @param Residuel Residuel si =1 placette avec coupe partielle deuis moins de 10 ans
#'
#' @param Data_ori Un dataframe contenant les valeurs de départ pour une liste
#'                d'arbres à simuler. Les champs: "PlacetteID","no_mes","origTreeID","Espece","Etat",
#'               "DHPcm","Dom_Bio","Nombre","Sdom_Bio","Veg_Pot","Latitude","Longitude","Altitude",
#'               "Pente","PTot","TMoy","GrwDays","Reg_Eco","Type_Eco", "Cl_Drai","Exposition",
#'               "Age_moy","sand_015cm","cec_015cm" doivent être présents. Si l'information
#'                sur certains champs n'est pas disponible, on peut le laisser vide.
#'
#'
#' @param FacHa  Facteur d'expansion de de toutes les placettes à l'hectare. Valeure par defaut fixée à 25
#
#' @param ClimMois Donnees climatiques mensuelles. Si abscente laisser vide
#'
#' @param ClimAn   Donnees climatiques annuelles. Si abscente laisser vide
#'
#' @param EvolClim  Valeure de 0 pour climat constant et de 1 pour evolution du climat à travers le temps de simulation
#'                  Valeure par defaut de 0
#'
#' @param AccModif Choix de fonction d'accroissement en diamètre "ORI" pour les équations originales d'Artémis 2014,
#'                 "BRT" pour les équations Boosted regression tree de JieJie Wang 2022,
#'                  "GAM" pour les GAM de D'Orangeville 2019
#'
#' @param MortModif Choix de fonction de mortalité "ORI" pour les équation originales d'Artemis 2014,
#'                 "QUE" pour les équation calibrées par essence sensibles au climat de Power et al. 2025
#'
#' @param RCP  Scenario climatique choisi pour la simulation soit 4.5 ou 8.5. Ce paramètre est seulement utilisé si le paramètre EvolClim=1
#'
#' @return Retourne un dataframe contenant la liste des arbres, leur état, leur DHP,
#'         leur hauteur et leur volume pour chaque placette
#'
#' @examples
#' result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 3, Tendance = 0, Residuel = 0, FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
#' print(result)
#'
#' result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 3, ClimMois = ClimMois_Exemple ,ClimAn = ClimAn_Exemple, Tendance = 0, Residuel = 0, FacHa = 25, EvolClim = 1, AccModif='BRT', MortModif='QUE', RCP='RCP45' )
#' print(result)
#' @export
#'


simulateurArtemis<-function(Data_ori,Horizon,ClimMois = NULL ,ClimAn = NULL,Tendance=0,Residuel=0,FacHa=25,EvolClim=0,AccModif='ORI',MortModif='ORI',RCP='RCP45'){


  if (!exists("Data_ori")){
    stop("Un data frame contenant l'inventaire de départ doit être passé à l'arguement Data_ori" )
  }

  if (!exists("Horizon")|Horizon==0){
    stop("Une valuere plus grande que 0 doit être passée à l'argument Horizon " )
  }

  if ((is.null(ClimMois)|is.null(ClimAn))&(EvolClim==1|AccModif!="ORI"|MortMofdif!="ORI")){
    stop("L'argument ClimAn et ClimMois ne peuvent pas être null
    lorsque EvolClim=1 ou que AccModif n'est pas égal à ORI
    ou que MortModif n'est pas égal à ORI" )
  }

  if(!Tendance %in% c(0,1)){
    stop("Les valeurs permises pour l'argument Tendance sont 0 ou 1")
  }

  if(!Residuel %in% c(0,1)){
    stop("Les valeurs permises pour l'argument Residuel sont 0 ou 1")
  }

  if(!MortModif %in% c("ORI","BRT")){
    stop("Les valeurs permises pour l'argument MortModif sont ORI ou BRT")
  }

  if(!AccModif %in% c("ORI","GAM","BRT")){
    stop("Les valeurs permises pour l'argument AccModif sont ORI, GAM ou BRT")
  }

  if(!RCP %in% c("RCP45","RCP85")){
    stop("Les valeurs permises pour l'argument RCP sont soit RCP45 ou RCP85")
  }



  Data_ori <- Data_ori %>% mutate(PlacetteID = paste0("P", PlacetteID))

  Para <- Para %>% mutate(Effect = str_to_lower(Effect))
  AnneeDep <- as.numeric(format(Sys.Date(), "%Y"))

  Data_ori<-renommer_les_colonnes(Data_ori)

  if (exists("ClimMois") && !is.null(ClimMois)) {

  ClimMois<- renommer_les_colonnes_climat_mensuel(ClimMois)

  ClimMois <- ClimMois %>% mutate(PlacetteID = paste0("P", PlacetteID))
  }

  if (exists("ClimAn") && !is.null(ClimAn)) {

  ClimAn<- renommer_les_colonnes_climat_annuel(ClimAn)
  ClimAn <- ClimAn %>% mutate(PlacetteID = paste0("P", PlacetteID))
  }

  Data_ori<-vevifier_variable_meteo(Data_ori)

  Data_ori<-vevifier_variable_Sol(Data_ori)

  Data_ori<-vevifier_variable_Sation(Data_ori)

  if( !"Age_moy" %in% names(Data_ori)){

    Data_ori <- Data_ori %>% mutate(Age_moy = 50)
  }

  prep_data <- PrepareData(Data_ori, ClimMois, ClimAn, AccModif, EvolClim, MortModif, RCP, SpInd, ListeVp, SpGroups, Sp)
  Data <- prep_data[[1]]
  Models <- prep_data[[2]]
  ClimMois <- prep_data[[3]]
  ClimAn <- prep_data[[4]]
  rm(prep_data)

  registerDoFuture()
  options(future.globals.maxSize= 891289600)###Monte la tolérence à 850 megs pour les éléments passés dans do futur
  plan(multisession, workers=availableCores()/2)#####Limite le nombre de coeurs utilisé pour éviter de planter l'ordi
  #plan(multisession)

  list_plot <- unique(Data$PlacetteID)

  Final<- bind_rows(
    foreach(x = iter(list_plot), .packages = c("gbm"))  %dorng%
      {ArtemisClimat(Para=Para,  Data=Data[Data$PlacetteID==x,],
                     AnneeDep=AnneeDep, Horizon=Horizon, FacHa=FacHa, Tendance=Tendance, Residuel=Residuel, ClimMois=ClimMois, ClimAn =ClimAn,
                     EvolClim =EvolClim, AccModif=AccModif, MortModif= MortModif, RCP=RCP, Models = Models)}
  )

  plan(sequential)

  if (EvolClim==1){


    PTotTMoyEvol<-ClimAn %>%
      mutate(Annee=Annee) %>%
      ungroup() %>%
      select(PlacetteID,Annee,PTot,TMoy)########Ajuste la température et les précipitations pour le calcul de la hauteur

suppressMessages(
    Final<-Final %>%
      select(-PTot,-TMoy) %>%
      inner_join(PTotTMoyEvol))

    rm(PTotTMoyEvol)

  } else{

    Final<-Final

  }

  Final<-Final %>%
    mutate(milieu=substr(Type_Eco,4,4)) %>%
    rename(id_pe=PlacetteID, dhpcm=DHPcm, essence=GrEspece,no_arbre=origTreeID, nb_tige=Nombre,
           altitude=Altitude,veg_pot=Veg_Pot,p_tot=PTot,t_ma=TMoy, reg_eco=Reg_Eco)

  nb_periodes <- Horizon+1

  ht <- OutilsDRF::relation_h_d(fic_arbres=Final, mode_simul='DET', nb_step=nb_periodes, reg_eco = TRUE, dt =10)


  Final2 <- OutilsDRF::cubage(fic_arbres=ht, mode_simul='DET', nb_step=nb_periodes)  %>%
    rename(PlacetteID=id_pe, DHPcm=dhpcm, GrEspece=essence, origTreeID=no_arbre, Nombre=nb_tige,
           Altitude=altitude, Veg_Pot=veg_pot, PTot=p_tot, TMoy=t_ma)

  Final2 <- Final2 %>% mutate(PlacetteID = gsub("^P", "", PlacetteID))

  return(Final2)

}
