#' Fonction qui éffectue la simulation d'Artemis. Elle procède pour chacune des placettes à
#' l'estimation de la mortalité, de l'accroissement en diamètre et du recrutement pour chacque périodes de simumulation
#' une à la suite de l'autre. Elle retourne un dataframe contenant les arbres vivants avec leur diamètres pour la placette à
#' chacune des périodes de simulation.
#'
#' @param Para Paramètres des différents modules d'Artémis contenus dans un dataframe.
#'
#' @param Data Dataframe contenant la liste d'arbres initiale de la placette à simuler.
#'
#' @param AnneeDep Année de début de la simulation. Si laissée vide, l'annnées courante sera inscrite.
#'
#' @param Horizon Valeure numérique du nombre de périodes de 10 ans sur lesquelles
#'                le simulateur effectuera ses simulations (ex: 3 pour 30 ans de simulation).
#'
#' @param FacHa Facteur d'expansion de la placette à l'hectare. Valeure par defaut fixée à 25.
#'
#' @param Tendance Si =1 les paramètres d'accroissement, de mortalité et de recrutement sont modifiés pour les végétations
#'                  potentielles FE2 et FE3 afin dutiliser deulement les intevalles de croissance se terminant après 1998.
#'
#' @param Residuel Inscrire 1 si la placette a été affectée par une coupe partielle depuis moins de 10 ans.
#'
#' @param ClimMois Données climatiques mensuelles. Si abscente laisser vide.
#'
#' @param ClimAn Données climatiques annuelles. Si abscente laisser vide.
#'
#' @param EvolClim Paramètre qui prend la valeure de 0 pour climat constant et
#'                 de 1 pour une évolution du climat à travers le temps de
#'                 simulation. Valeure par defaut de 0.
#'
#' @param AccModif Choix de la fonction d'accroissement en diamètre, "ORI" pour les
#'                 équations originales d'Artémis-2014, "BRT" pour les équations
#'                 Boosted regression tree de JieJie Wang 2022, "GAM" pour les
#'                 équations de D'Orangeville 2019.
#'
#'@param MortModif Choix de fonction de mortalité ,"ORI" pour les équations
#'                 originales d'Artémis-2014,"QUE" pour les équations calibrées
#'                 par essence sensibles au climat de Power et al. 2025.
#'
#'@param RCP Scénario climatique choisi pour la simulation soit RCP 4.5 ou 8.5.
#'           Ce paramètre est seulement utilisé si le paramètre EvolClim=1.
#'
#'@param Models Liste dans laquelle les modèles d'accroissement et de moratlité
#'               (à l'exception des modele d'Artémis-2014) sont inclus.
#'
#' @return Retourne un dataframe contenant la liste d'arbres vivants de la
#'         placette simulée avec leur DHP pour chaque période de simulation.
#'
#' @export
#'
ArtemisClimat<- function(Para, Data, AnneeDep, Horizon, FacHa=25,Tendance, Residuel, ClimMois, ClimAn, EvolClim, AccModif, MortModif, RCP, Models){


  #Longueur d'un pas de simulation

  LongPas=10

  t <- LongPas #IA: j'ai mis ça en paramètre de la fonction

  # Liste des placettes a simuler
  ListePlac <- unique(Data$PlacetteID)

  # Nombre de placettes a simuler
  NbPlac <- length(ListePlac)

  # Changement Vp pour tendance
  Data <- Data %>%
    mutate (Veg_Pot=ifelse(Tendance==1 & Veg_Pot=="FE2","FEX",
                           ifelse(Tendance==1 & Veg_Pot=="FE3","FEY",Veg_Pot)))


  # Initialiser l'annee de depart et la correction du biais
  Plac <- Data %>%
    filter(Etat %in% c(10,12,40,42,30,32,20,22)) %>%
    mutate(Annee = AnneeDep, Variance = 0, Etat="vivant")

  #Création placette origine
  PlacOri<-Plac %>%
    mutate(ArbreID=origTreeID) %>%
    select(Annee,PlacetteID, origTreeID,Espece,GrEspece,
           Etat, Nombre, DHPcm,Veg_Pot, Type_Eco, Reg_Eco, Altitude, PTot, TMoy, Cl_Drai)

  # info placette
  info_plac <- Plac[1,] %>% select(-Nombre, -DHPcm, -Variance, -Etat, -origTreeID, -GrEspece, -Espece)

  # VP de la placette
  Veg_Pot <- Plac$Veg_Pot[1]

  # Classe de drainage de la placette
  Drainage <- Plac$Cl_Drai[1]
  DrainageCl<-ifelse(Drainage >=50 | Drainage <=16, "HydXer","MesSub")

  # Texture de la placette
  Texture<-ifelse(substr(Plac$Type_Eco[1],4,4) %in% c("2","3","5","6"),"FinMoy","GrosOrg")

  # Pente de la placette
  PenteCl<-ifelse(Plac$Pente[1]>30,"EF", ifelse(Plac$Pente[1]<=8,"B",ifelse(Plac$Pente[1]<=15,"C",ifelse(Plac$Pente[1]<=30,"D","A"))))

  # Selection de la correction pour le biais selon la vp
  Cor<-ListeCor$Cor[which(ListeCor$Veg_Pot==Veg_Pot)]

  # Effet des coupes partielles à 0
  Coupe0 <-ifelse(Residuel[1]==1,1,0)
  Coupe <-0
  Coupe1<-0

  # Variables climatiques de la placette
  PTot <- Plac$PTot[1]
  TMoy <- Plac$TMoy[1]

  # Variable Region Ouest
  RegionOuest<-ifelse(Plac$Reg_Eco[1] %in% c("1a","2b","2c","3d","4f","4h","4g","5i","5h"),0,1)


  # Effet TBE a 0
  tbe <- 0
  tbe1 <- 0

  # Liste des especes des groupes d'especes de la vp de la placette
  EspecesVp <- ListeSpVp[which(ListeSpVp$VegPotName==Veg_Pot),]

  # Liste des groupes d'especes de la vp de la placette
  Especes<- EspecesVp %>%
    select(SpeciesGroupName) %>%
    rename(GrEspece=SpeciesGroupName) %>%
    unique()

  # Classes de drainage pour modèles BRT
  if (AccModif=="BRT"){

    Plac$Moist<-ifelse(Plac$Cl_Drai[1] %in% c(0,10,11),"X",
                       ifelse(Plac$Cl_Drai[1] %in% c(50,51,60,61),"H","M"))
  }

  # Lecture du climat
  if (!(AccModif=="ORI" & MortModif=="ORI" & EvolClim==0) ){

    ClimPe<-ClimMois %>% filter(PlacetteID==info_plac$PlacetteID)
    ClimAnPe<-ClimAn %>% filter(PlacetteID==info_plac$PlacetteID)

  # Climat historique si on utilise des équations sensibles au climat sinon variables lues dans Plac

      ClimatHisto<-ClimatBiosim(Placettes = Plac$PlacetteID[1],Annee=2020, t, RCP=RCP, ClimPe, ClimAnPe, EvolClim, AccModif) #Annee de départ définie à 2020 pour climat historique
  }

   # Initialisation du fichier qui contiendra les résultats de simulation de la placette
  outputTot <- c()


  ######################### boucle pour les k decennies a simuler ##########################
  for (k in 1:Horizon) {


    ###################################### Mise a jour des variables a l'echelle de la placette #######################


    # Si premier pas de simulation, on utilise le fichier de depart de la placette
    if   (k==1) {
      Plac <- Plac %>%
        mutate(ArbreID=origTreeID)
      Annee <-AnneeDep


    }  else {                            # Si 2e pas de simulation ou plus, on prend le fichier qui contient les simulations et on garde seulement le dernier pas
      Plac <- outputTot %>%
        filter(Annee == AnneeDep+((k-1)*t) & Etat=="vivant") #IA: j'ai changé le 10 pour t

      Annee<-Plac$Annee[1]

      #Mise à jour coupes
      Coupe1<-ifelse(Coupe==1,1,0)
      Coupe<-ifelse(Coupe0==1,1,0)
      Coupe0<-0


    }

    # Ajout des donnees CO2 de la période si nécessaire

    if((AccModif=="BRT")) {
      if (EvolClim==1) {
        Plac$CO2<-CO2$CO2[which(abs(CO2$AnneeMoy-Annee)==min(abs(CO2$AnneeMoy-Annee)) & CO2$rcp==RCP)]
      } else {
        Plac$CO2<-CO2$CO2[which(abs(CO2$AnneeMoy-Annee)==min(abs(CO2$AnneeMoy-Annee)) & is.na(CO2$rcp==TRUE))]
      }
    }


    #Variable anc

    anc<-ifelse(Plac$Annee[1]<2008,1,0)

    Plac<-BAL(Plac,FacHa=FacHa)

    # calcul variable echelle placette
    sum_st_ha <- sum(Plac$ST_m2[which(Plac$Etat=="vivant")])
    n_arbre_ha <- sum(Plac$Nombre[which(Plac$Etat=="vivant")])*FacHa
    n_arbre <- sum(Plac$Nombre[which(Plac$Etat=="vivant")])
    mq_DHPcm <- sqrt(sum_st_ha/n_arbre_ha*40000/3.1416)

    # Données Climatiques de la période si on utilise pas la version originale

    if (EvolClim==1){

      ClimatModif<-ClimatBiosim(Placettes = Plac$PlacetteID[1],Annee, t, RCP=RCP, ClimPe, ClimAnPe, EvolClim, AccModif)

    }


    ################################################ Mortalite ###############################################

    # fichier des arbres de la placette pour appliquer le module de mortalite
    Mort <- Plac

    ###################################Mortalité de base Artemis########################

    if (MortModif=="ORI") {
      # Modification des TMoy et PTot
      if (EvolClim==1){
        Mort<-Mort %>%
              mutate(PTot=ClimatModif$PTotPeriode, TMoy=ClimatModif$TMoyPeriode)
      }

    # on applique la fonction de mortalite sur chacun des arbres avec la fonction nest()

     PredMort <- Mort %>%
           mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
             PTot=PTot,RegionOuest=RegionOuest,sum_st_ha=sum_st_ha, Drainage=Drainage,
             Veg_Pot=Veg_Pot, TMoy=TMoy) %>%
      group_by(origTreeID) %>%
      nest() %>%
      mutate(pred_mort = map(data,mort)) %>%
      unnest(pred_mort) %>%
      select(-data) # contient 2 variables: origTreeID et pred_mort

    }


    ###################################Mortalité avec equations par sp et varibles climatique HP#####
    if (MortModif=="QUE"){

      if (EvolClim==0){ClimatQUE=ClimatHisto} else {ClimatQUE=ClimatModif}

      PredMort<-mortQUE(Mort, ClimatQUE, Models, DrainageCl, PenteCl, Texture, Coupe, Coupe0, sum_st_ha, t)
    }



    ############################################## ACCROISSEMENT ####################################################

    # fichier des arbres de la placette pour appliquer le module d'accroissement
    Accrois <- Plac


    ##########################################Accroissement de base avec Artémis##################################

    # on applique la fonction d'accroissement sur chacun des arbres avec la fonction nest()
    if (AccModif=="ORI"){
    PredAcc <- Accrois %>%
      mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
             PTot=PTot,RegionOuest=RegionOuest,sum_st_ha=sum_st_ha, Drainage=Drainage, Veg_Pot=Veg_Pot) %>%
      group_by(origTreeID) %>%
      nest() %>%
      mutate(pred_acc = map(data,accrois)) %>%
      unnest(pred_acc) %>%
      mutate(pred_acc=round(ifelse(pred_acc<0,0,(exp(pred_acc+Cor)-1))*10)/10)%>%
      select(-data)
    }

    #########################Accroissement avec modèles BRT##################################
    if (AccModif=="BRT"){

      if (EvolClim==0){ClimatBRT=ClimatHisto} else {ClimatBRT=ClimatModif}

      PredAcc<-AccroissementBRT(Accrois,ClimatBRT,Models, sum_st_ha, t)

      rm(ClimatBRT)
      }

    #########################Accroissement avec modèles GAM##################################
    if (AccModif=="GAM"){
      Accrois<-Accrois %>%
        mutate(BA=sum_st_ha,
              # Age_moy=ifelse(k==1,Age_moy,Age_moy+10),
               stand_stage=ifelse(Age_moy <=70,"immature",ifelse(Age_moy<=100,"mature","old")))

      if (EvolClim==0){ClimatGAM=ClimatHisto} else {ClimatGAM=ClimatModif}

      PredAcc<-AccroissementGAM(Accrois, ClimatGAM, Models, t)

      rm(ClimatGAM)
    }


    # on ajoute l'accroisement predit et la mortalite predite au fichier des arbres
    # on recalcule le nombre de tiges que représente l'arbre ou la classe de dhp
    Accrois <- inner_join(Accrois, PredAcc, by = "origTreeID") %>%
      inner_join(PredMort, by = "origTreeID") %>%
      mutate(Etat = "vivant",
             Nombre = Nombre * (1-pred_mort),
             Variance = Variance + (exp(Cor)-1)*exp(2*log(pred_acc+1+Cor)+Cor),
             DHPcmIni=DHPcm,
             DHPcm=ifelse((DHPcm+pred_acc)>100,100,(DHPcm+pred_acc))) %>%   #Bloc les diamètres à 100 cm pour éviter les dérapages
      arrange(origTreeID) %>%
      select(-pred_mort)


    #################################################Recrutement################################
    ###########################################################################################

    # calcul des variables a l'echelle de groupe d'Especes
    suppressMessages(
      EspecesSp <- Plac %>%
        group_by(GrEspece) %>%
        summarise(nbtiges_ess=sum(Nombre), nbtiges_ess_ha=sum(Nombre)*FacHa, St_ha_ess=sum(ST_m2)))

    # ajouter les groupes d'Especes no presents dans la placette
    EspecesFinal <- left_join(Especes, EspecesSp, by="GrEspece") %>%
      arrange(GrEspece) %>%
      mutate_at(vars(nbtiges_ess:St_ha_ess), ~replace(., is.na(.), 0)) %>%
      mutate(groupe=GrEspece) # il faut une copie de la variable pour utiliser nest()


    # on applique la fonction de recrutement sur chacun des groupes d'Especes avec la fonction nest()
    PredRecrue <- EspecesFinal %>%
      mutate(anc=anc,Coupe=Coupe,Coupe0=Coupe0,Coupe1=Coupe1,t=t,tbe=tbe,tbe1=tbe1,n_arbre=n_arbre,
             n_arbre_ha=n_arbre_ha, PTot=PTot, TMoy=TMoy, RegionOuest=RegionOuest,sum_st_ha=sum_st_ha,
             Drainage=Drainage,Veg_Pot=Veg_Pot, mq_DHPcm=mq_DHPcm) %>%
      group_by(GrEspece) %>%
      nest() %>%
      mutate(pred=map(data,fctrecrue)) %>%
      unnest(pred) %>%
      separate(pred, c("Nombre", "DHPcm", "Variance"), sep=" ") %>%
      mutate(PlacetteID=as.character(info_plac[1,1]), Etat="vivant", origTreeID = last(Accrois$origTreeID),
             Nombre = as.numeric(Nombre), DHPcm = as.numeric(DHPcm), Variance = as.numeric(Variance),
             Espece=ifelse(GrEspece %in% c("AUT","F_0","F_1","CHX","EPX","F0R","PEU","PIN","RES","FEU"),NA,GrEspece)) %>%
      select(-data)

    ID<-seq(from=1,to=nrow(PredRecrue), by=1)
    PredRecrue$origTreeID<-PredRecrue$origTreeID+ID
    PredRecrue$ArbreID<-PredRecrue$origTreeID


    if (AccModif!="ORI" | MortModif=="QUE") {
      # Séparer recrues EPX entre EPN et EPB
      PropEPB<-Models[[8]]
      PropEPB<-PropEPB$PropEPB[which(PropEPB$VEG_POT==Veg_Pot)]

      # Si presence de EPB dans veg_Pot on passe par ici Sinon on passe par else pour
      # ajouter Essence dans Essence_ori et modifier NOMBRE_TIGES
      if (PropEPB>0){
        PredRecrue <- PredRecrue %>%
          filter(GrEspece=='EPX') %>%
          mutate(Espece="EPB",Nombre=PropEPB*Nombre,
                 origTreeID=max(Accrois$origTreeID)+nrow(PredRecrue)+1) %>%
          rbind(PredRecrue) %>%
          mutate(Espece=ifelse(GrEspece=="EPX" & is.na(Espece==TRUE),"EPN",Espece)) %>%
          mutate(ifelse(Espece=="EPN",Nombre-(Nombre*PropEPB),Nombre)) %>%
          filter(Nombre>0)
      }else {
        PredRecrue <- PredRecrue %>%
          mutate(Espece=ifelse(GrEspece=="EPX","EPN",Espece))
      }
    # on ajoute les variables a l'echelle de la placette aux recrues
      output3 <- left_join(PredRecrue,info_plac, by="PlacetteID") %>%
                 mutate(Moist=ifelse(Plac$Cl_Drai[1] %in% c(0,10,11),"X",
                              ifelse(Plac$Cl_Drai[1] %in% c(50,51,60,61),"H","M")),
                 Age_moy=Accrois$Age_moy[1])
    }else {

      output3 <- left_join(PredRecrue, info_plac, by="PlacetteID")
    }


    ######################################### fin recrue ##################


    # on ajoute les recrues aux arbres de la placette
    Predictions <- bind_rows(Accrois, output3) %>%   ####J'ai garde juste les vivants (pas mis output)
                   arrange(PlacetteID, origTreeID) %>%
                   mutate(Annee = AnneeDep+(t*k)) %>%
                   mutate(Age_moy=Age_moy+t) %>%
                   select(-pred_acc, -ST_m2, -st_ha_cumul_gt)

    Predictions$Cl_Drai<-PlacOri$Cl_Drai[1]  #####Ajouté pour suivre cette variable

    Test<-BAL( Predictions,FacHa=FacHa)


    # calcul variable echelle placette
    St_Test <- sum(Test$ST_m2[which(Test$Etat=="vivant")])
    n_Test <- sum(Test$Nombre[which(Test$Etat=="vivant")])*FacHa


    if (n_Test>5000 | St_Test >60){

      break

       }######Arrete simulation si les valeurs limites sont dépassées

    # on ajoute le donnees du pas de simulation en cours aux autres pas de simulation pour la placette
    outputTot <- bind_rows(outputTot, Predictions)


  }  # fin de la boucle d'un pas de simulation

  if (k>1){
  outputTot<-outputTot %>%
    select(Annee,PlacetteID,origTreeID,Espece,  GrEspece, Etat,
           Nombre, DHPcm, Veg_Pot, Type_Eco, Reg_Eco, Altitude, PTot, TMoy, Cl_Drai) %>%
    bind_rows(PlacOri) %>%
    arrange(Annee,origTreeID)
  } else{
    outputTot<-outputTot %>%
      select(Annee,PlacetteID,origTreeID,Espece,  GrEspece, Etat,
             Nombre, DHPcm, Veg_Pot, Type_Eco, Reg_Eco, Altitude, PTot, TMoy, Cl_Drai) %>%
      bind_rows(PlacOri) %>%
      arrange(Annee,origTreeID)
  }


  return(outputTot)

}
