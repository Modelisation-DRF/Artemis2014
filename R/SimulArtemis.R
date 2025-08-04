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
#' @param ClimMois Donnees climatiques mensuelles. Si absente laisser vide
#'
#' @param ClimAn   Donnees climatiques annuelles. Si absente laisser vide
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
#' @param Coupe_ON Vecteur contenant le numéro du traitement de coupe (0-18) pour chaque
#'              décennie à simuler. Le nombre d'éléments doit être égal à Horizon.
#'              Utiliser NA pour les décennies sans coupe. Par défaut = NULL (pas de coupe).
#'
#' @param Coupe_modif Une liste de data.frames et/ou de nombres contenant les modificateurs
#'                    de probabilité de coupe. Par défaut = NULL (pas de modificateurs).
#'                    La liste doit avoir la même longueur que le nombre de périodes (Horizon).
#'                    Chaque élément peut être:
#'                    - Un nombre unique (s'applique à toutes les essences)
#'                    - Un data.frame avec colonnes 'essence' et 'modifier'
#'                    - NA (pas de modification pour cette période)
#'
#' @param TBE Vecteur contenant les valeurs 0 ou 1 pour indiquer les décennies avec
#'            un effet de TBE (Tordeuse des bourgeons de l'épinette).
#'            Le nombre d'éléments doit être égal à Horizon.
#'            1 = effet TBE présent, 0 = pas d'effet TBE.
#'            Par défaut = NULL (pas de TBE).
#'
#' @return Retourne un dataframe contenant la liste des arbres, leur état, leur DHP,
#'         leur hauteur et leur volume pour chaque placette
#'
#' @examples
#' \dontrun{
#' result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 3, Tendance = 0, Residuel = 0,
#' FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
#' print(result)
#'
#' # Avec effet TBE
#' result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 4,
#' TBE = c(1, 1, 1, 1), Tendance = 0, Residuel = 0,
#' FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
#' print(result)
#'
#' # Avec traitement de coupe
#' result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 6,
#' Coupe_ON = c(3, NA, NA, NA, NA, 8), Tendance = 0, Residuel = 0,
#' FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
#' print(result)
#'
#' # Avec modificateurs de coupe
#' result <- simulateurArtemis(Data_ori = Donnees_Exemple, Horizon = 4,
#' Coupe_ON = c(3, NA, NA, 8),
#' Coupe_modif = list(80, NA, NA, 0), Tendance = 0, Residuel = 0,
#' FacHa = 25, EvolClim = 0, AccModif='ORI', MortModif='ORI', RCP='RCP45' )
#' print(result)
#' }
#'
#' @export
#'


simulateurArtemis<-function(Data_ori,Horizon,ClimMois = NULL ,ClimAn = NULL,Tendance=0,Residuel=0,FacHa=25,EvolClim=0,AccModif='ORI',MortModif='ORI',RCP='RCP45', Coupe_ON = NULL,
                          Coupe_modif = NULL, TBE = NULL){


  if (!exists("Data_ori")){
    stop("Un data frame contenant l'inventaire de d\u00E9part doit \u00EAtre pass\u00E9 \u00E0 l'argument Data_ori" )
  }

  if (!exists("Horizon")|Horizon==0){
    stop("Une valeur plus grande que 0 doit \u00EAtre pass\u00E9e \u00E0 l'argument Horizon " )
  }

  if ((is.null(ClimMois)|is.null(ClimAn))&(EvolClim==1|AccModif!="ORI"|MortModif!="ORI")){
    stop("L'argument ClimAn et ClimMois ne peuvent pas \u00EAtre null
    lorsque EvolClim=1 ou que AccModif n'est pas \u00E9gal \u00E0 ORI
    ou que MortModif n'est pas \u00E9gal  ORI" )
  }

  if(!Tendance %in% c(0,1)){
    stop("Les valeurs permises pour l'argument Tendance sont 0 ou 1")
  }

  if(!Residuel %in% c(0,1)){
    stop("Les valeurs permises pour l'argument Residuel sont 0 ou 1")
  }

  if(!MortModif %in% c("ORI","QUE")){
    stop("Les valeurs permises pour l'argument MortModif sont ORI ou QUE")
  }

  if(!AccModif %in% c("ORI","GAM","BRT")){
    stop("Les valeurs permises pour l'argument AccModif sont ORI, GAM ou BRT")
  }

  if(!RCP %in% c("RCP45","RCP85")){
    stop("Les valeurs permises pour l'argument RCP sont soit RCP45 ou RCP85")
  }

  if (!is.null(Coupe_ON)) {
    if (!is.vector(Coupe_ON) || !is.numeric(Coupe_ON)) {
      stop("L'argument Coupe_ON doit être un vecteur numérique")
    }
    if (length(Coupe_ON) != Horizon) {
      stop(paste("La longueur de Coupe_ON doit être égale à Horizon (", Horizon, ")", sep=""))
    }
    # Vérifier que les valeurs sont valides (0-18 ou NA)
    valid_values <- c(0:18, NA)
    if (!all(Coupe_ON %in% valid_values)) {
      stop("Les valeurs de Coupe_ON doivent être entre 0 et 18 ou NA")
    }
  }

  # Validation du paramètre Coupe_modif
  if (!is.null(Coupe_modif)) {
    if (!is.list(Coupe_modif)) {
      stop("L'argument Coupe_modif doit être une liste")
    }
    if (length(Coupe_modif) != Horizon) {
      stop(paste("La longueur de Coupe_modif doit être égale à Horizon (", Horizon, ")", sep=""))
    }

    # Valider chaque élément de la liste
    for (i in 1:length(Coupe_modif)) {
      elem <- Coupe_modif[[i]]

      # Skip NULL elements
      if (is.null(elem)) {
        next
      }

      # Handle single values (numbers or NA)
      if (length(elem) == 1) {
        if (is.na(elem)) {
          next  # Skip NA values
        } else if (is.numeric(elem)) {
          if (elem < -80 || elem > 160) {
            stop(paste("Le modificateur à la position", i, "doit être entre -80 et 160"))
          }
        } else {
          stop(paste("L'élément à la position", i,
                     "doit être un nombre, un data.frame, NA ou NULL"))
        }
      }
      # Handle data frames or lists with multiple elements
      else if (is.data.frame(elem)) {
        if (!all(c("ess_ind", "modifier") %in% names(elem))) {
          stop(paste("Le data.frame à la position", i,
                     "doit contenir les colonnes 'ess_ind' et 'modifier'"))
        }
        if (any(elem$modifier < -80 | elem$modifier > 160)) {
          stop(paste("Tous les modificateurs dans le data.frame à la position", i,
                     "doivent être entre -80 et 160"))
        }
      }
      # Handle other multi-element objects
      else {
        stop(paste("L'élément à la position", i,
                   "doit être un nombre, un data.frame, NA ou NULL"))
      }
    }
  }

  # Validation du paramètre TBE
  if (!is.null(TBE)) {
    if (!is.vector(TBE) || !is.numeric(TBE)) {
      stop("L'argument TBE doit être un vecteur numérique")
    }
    if (length(TBE) != Horizon) {
      stop(paste("La longueur de TBE doit être égale à Horizon (", Horizon, ")", sep=""))
    }
    # Vérifier que les valeurs sont 0 ou 1
    if (!all(TBE %in% c(0, 1))) {
      stop("Les valeurs de TBE doivent être 0 ou 1")
    }
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
  #plan(sequential) #temporaire

  list_plot <- unique(Data$PlacetteID)

  Final<- bind_rows(
    foreach(x = iterators::iter(list_plot), .packages = c("gbm"))  %dorng%
      {ArtemisClimat(Para=Para,  Data=Data[Data$PlacetteID==x,],
                     AnneeDep=AnneeDep, Horizon=Horizon, FacHa=FacHa, Tendance=Tendance, Residuel=Residuel, ClimMois=ClimMois, ClimAn =ClimAn,
                     EvolClim =EvolClim, AccModif=AccModif, MortModif= MortModif, RCP=RCP, Models = Models, Coupe_ON = Coupe_ON, Coupe_modif = Coupe_modif,
                     TBE = TBE)}
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

  ht <- OutilsDRF::relation_h_d(fic_arbres=Final, mode_simul='DET', nb_step=nb_periodes,
                                reg_eco = TRUE, dt =10, grouping_vars = "Annee")


  Final2 <- OutilsDRF::cubage(fic_arbres=ht, mode_simul='DET', nb_step=nb_periodes)  %>%
    rename(PlacetteID=id_pe, DHPcm=dhpcm, GrEspece=essence, origTreeID=no_arbre, Nombre=nb_tige,
           Altitude=altitude, Veg_Pot=veg_pot, PTot=p_tot, TMoy=t_ma)

  Final2 <- Final2 %>% mutate(PlacetteID = gsub("^P", "", PlacetteID))

  return(Final2)

}

#chemin <- "C:/Users/boini5/OneDrive - BuroVirtuel/Bureau/MRNF Projects/Artemis2014/data-raw/Intrant_Test.csv"
#head_data <- read.csv(chemin, sep = ";", nrows = 10)
#vec_Coupe_ON <- c(1, NA, NA)
#test_ess <- data.frame(
#  ess_ind = c("CHR"),
#  modifier = c(50))
#vec_coupeModifier <- list(test_ess, NA, NA)
#TBE <- c(1, 1 ,1)
##Result33 <- suppressMessages(simulateurArtemis(Data_ori = head_data, Horizon = 5, ClimMois = NULL, ClimAn = NULL, AccModif='ORI', MortModif='ORI', RCP='RCP45')) %>% arrange(PlacetteID, origTreeID, Annee)
#Result8 <- suppressMessages(simulateurArtemis(Data_ori = head_data ,Horizon = 3,ClimMois = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45',
#                                            Coupe_ON = vec_Coupe_ON, Coupe_modif = vec_coupeModifier, TBE = TBE) %>% arrange(PlacetteID,origTreeID,Annee))


