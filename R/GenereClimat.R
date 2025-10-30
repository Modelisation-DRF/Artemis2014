#' Fonction qi permet de générer des données climatiques pour le fichier de données
#' initiales. Les données climatiques sont générées avec l'API BioSIM et sont
#' basées sur les prévisions du modèle climatique global RCM4. La fonction utilise 5
#' répétition du modèle et moyenne les prévisions. La fonction retourne une liste de
#' deux dataframe, le premier contenant les prévisions à l'échelle annuelle et le deuxième
#' à l'échelle mensuelle. La fonction peut-être longue à exécuter (environ 0.5 seconde par placette
#' et par période de simulation de 10 ans)
#'
#' @param Data_Ori Un dataframe contenant les coordonnées géographiques des placettes à simuler.
#'                 Les champs: "PlacetteID","Latitude","Longitude","Altitude", doivent être présents.
#'                 Le dataframe peut être une liste d'arbres ou une liste de placettes.
#' @param AnneeDep Année de départ de la simulation à effectuer dans Artémis
#' @param AnneeFin Année de fin de la simulation à effectuer dans Artémis
#' @param RCP Scenario climatique choisi pour la simulation soit "RCP45"  ou "RCP85"
#' @return La fonction retourne une liste de deux dataframe, le premier contenant
#'        les prévisions à l'échelle annuelle et le deuxième à l'échelle mensuelle.
#'
#' @export
#'
GenereClimat <- function(Data_Ori, AnneeDep, AnneeFin, RCP = "RCP45") {

  Data_Ori <- Data_Ori %>%
    mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude) , Altitude = as.numeric(Altitude))


  if (AnneeFin > as.numeric(format(Sys.Date(), "%Y"))) {
    suppressMessages(
      Placettes <- Data_Ori %>%
        group_by(PlacetteID, Latitude, Longitude, Altitude) %>%
        summarise()
    )


    AnneeDep <- ifelse(AnneeDep > 1991, 1991, AnneeDep)###1991 pour période climat historique 1981-2010

    QcAn <- as.data.frame(BioSIM::generateWeather("ClimaticQc_Annual", AnneeDep, AnneeFin, Placettes$PlacetteID, Placettes$Latitude,
      Placettes$Longitude, Placettes$Altitude,
      rep = 5, repModel = 1, rcp = RCP, climModel = "RCM4"
    ))

    CMIMois <- as.data.frame(BioSIM::generateWeather(c("Climate_Moisture_Index_Monthly","VaporPressureDeficit_Monthly"), AnneeDep,
                            AnneeFin, Placettes$PlacetteID, Placettes$Latitude, Placettes$Longitude, Placettes$Altitude,
      rep = 5, repModel = 1, rcp = RCP, climModel = "RCM4"
    ))

    suppressMessages(
      shutdownClient()
    )

    ClimAn <- QcAn[, c(1, 6, 17, 8, 12, 13, 24, 7)]
    names(ClimAn) <- c("PlacetteID", "Annee", "FFP", "PTot", "TMoy", "Tmax_yr", "Aridity", "DD")
    ClimAn$rcp <- RCP
    ClimAn <- ClimAn[, c(1, 9, 2:8)]

    suppressMessages(
      ClimAn <- ClimAn %>%
        group_by(PlacetteID, rcp, Annee) %>%
        summarise(FFP = mean(FFP), PTot = mean(PTot), TMoy = mean(TMoy), Tmax_yr = mean(Tmax_yr), Aridity = mean(Aridity), DD = mean(DD))
    )

    ClimMois_ini <- CMIMois[, c(1, 6, 7, 8, 9, 10, 12, 22)]
    names(ClimMois_ini) <- c("PlacetteID", "Annee", "Mois", "Tmax",  "Tmin", "PTot", "CMI", "VPD")
    ClimMois_ini$rcp <- RCP
    ClimMois_ini <- ClimMois_ini[, c(1, 9, 2:8)]

    suppressMessages(
      ClimMois <- ClimMois_ini %>%
        group_by(PlacetteID, rcp, Annee, Mois) %>%
        summarise(Tmin = mean(Tmin), Tmax = mean(Tmax), PTot = mean(PTot)*10)#####Conversion des précipitations de cm en mm
    )


    suppressMessages(
      CMI <- ClimMois_ini %>%
             filter(Mois %in% c(5, 6, 7, 8, 9)) %>%
             group_by(PlacetteID, Annee, Mois) %>%
             summarise(CMI = mean(CMI)) %>%
             group_by(PlacetteID, Annee) %>%
             summarise(CMI = sum(CMI)*10) #####Conversion CMI cm en CMI en mm
    )


    suppressMessages(        #CMI en cm et VPD pour Accroissement Fortin 2026
     CMIVPD <- ClimMois_ini %>%
               filter(Mois %in% c( 6, 7, 8)) %>%
               group_by(PlacetteID, Annee) %>%
               summarise(CMIcm = mean(CMI), VPD=mean(VPD))
    )

    suppressMessages(
      ClimAn <- ClimAn %>%
        left_join(CMI) %>%
        left_join(CMIVPD)
    )

    rm(QcAn, CMIMois, CMI)

    ClimTot <- list(ClimAn, ClimMois)

  } else {
    suppressMessages(
      Placettes <- Data_Ori %>%
        group_by(PlacetteID, Latitude, Longitude, Altitude) %>%
        summarise()
    )

    QcAn <- as.data.frame(BioSIM::generateWeather("ClimaticQc_Annual", AnneeDep, AnneeFin, Placettes$PlacetteID, Placettes$Latitude,
      Placettes$Longitude, Placettes$Altitude,
      rep = 1, repModel = 1, rcp = RCP, climModel = "RCM4"
    ))

    suppressMessages(
      shutdownClient()
    )

    ClimTot <- QcAn[, c(1, 6, 7:28)]
    names(ClimTot) <- c(
      "PlacetteID", "Annee", "DD", "PTot", "UtilPrec", "GrowingSeasonPrec",
      "TMin", "TMoy", "TMax", "GrowingSeasonTMoy", "JulyTMoy", "DaysWithoutFrost",
      "ConsecutiveDaysWithoutFrost", "GrowingSeasonLength", "LastFrostDay",
      "FirstFrostDay", "TotalVPD", "UtilVPD", "PET", "Aridity", "SnowfallProportion",
      "TotalSnowfall", "TotalRadiation", "GrowingSeasonRadiation"
    )
  }

  return(ClimTot)
}
