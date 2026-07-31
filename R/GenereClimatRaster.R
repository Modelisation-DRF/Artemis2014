#' Fonction qi permet de récupérer les données climatiques pour le fichier de données
#' initiales. Les données climatiques sont récupérées des couches de climat future
#' du package ExtractMap. Ces couches ont été créées à partit de bioSim pour des
#' cellules de 2km par 2km à partir. Les modèles GCM4_ESM2, Hadley GEM2-ES,
#' RCM4_ESM_22km pour 2 scénarios RCP avec 50 répétitions. La fonction retourne une liste de
#' deux dataframe, le premier contenant les prévisions à l'échelle annuelle et le deuxième
#' à l'échelle mensuelle.
#'
#' @param Data Un dataframe contenant les coordonnées géographiques des placettes à simuler.
#'                 Les champs: "PlacetteID","Latitude","Longitude","Altitude", doivent être présents.
#'                 Le dataframe peut être une liste d'arbres ou une liste de placettes.
#' @param AnneeDep Année de départ de la simulation à effectuer dans Artémis
#' @param AnneeFin Année de fin de la simulation à effectuer dans Artémis
#' @param RCP Scenario climatique choisi pour la simulation soit "RCP45"  ou "RCP85"
#' @return La fonction retourne un dataframe contenant les prévisions à l'échelle
#'        annuelle pour toutes les variables climatiques  utilisées par les modèles.
#'
#' @export
#'
GenereClimatRaster <- function(Data, AnneeDep, AnneeFin, RCP = "RCP45") {

  if (AnneeFin<=AnneeDep){
    stop("AnneeFin doit être supérieure à AnneeDep" )
  }

  if (!RCP %in% c("RCP45","RCP85")){
    stop("La variable RCP doit soit prendre la valeur RCP45 ou RCP85")
  }

  Data <- Data %>%
              group_by(PlacetteID) %>%
              summarise(latitude = first(Latitude), longitude = first(Longitude)) %>%
              rename(id_pe = PlacetteID) %>%
              select(id_pe,latitude, longitude)####structure le fichier pour ExtractMap

#Créer la liste des variables à obtenir

  BorneSup<-case_when(AnneeFin<2001~1,AnneeFin<2011~2,AnneeFin<2021~3,AnneeFin<2031~4,
                      AnneeFin<2041~5,AnneeFin<2051~6,AnneeFin<2061~7,AnneeFin<2071~8,.default=9)

  nom_climat_futur_per <- c("1991-2020", "2001-2030", "2011-2040", "2021-2050", "2031-2060", "2041-2070", "2051-2080", "2061-2090", "2071-2100")
  nom_climat_futur_per<-nom_climat_futur_per[1:BorneSup]
  nom_climat_futur_var <- c("Aridity", "CMI", "CMIcm", "DD", "FFP", "MSP", "Max_ST", "Min_WT", "PAS", "PTot", "PUtile", "TMoy", "TSummer", "TmaxUtil", "Tmax_yr", "TotalVPD", "UtilVPD")
  nom_climat_futur_rcp<-RCP


  interpoler <- function(df, AnneeDep, AnneeFin, var) {

    annees <- seq(AnneeDep-30, AnneeFin)

    interp <- approx(
      x = df$AnneeCentrale,
      y = df$Variable,
      xout = annees,
      rule = 2  # extrapolation si nécessaire
    )

    result<-data.frame(Annee = interp$x,Variable = interp$y)
    names(result)[2]<-var

    return (result)

  }


  for (i in 1:length(nom_climat_futur_var)){

  nom_climat_futur_var_i<-nom_climat_futur_var[i]

  nom_climat_futur <- apply(
    expand.grid(
      nom_climat_futur_per,
      nom_climat_futur_rcp,
      nom_climat_futur_var_i,
      stringsAsFactors = FALSE
    ),
    1,
    paste,
    collapse = "_"
  )


  climat_futur_values <- extract_map_plot(file=Data, liste_raster="cartes_climat_futur",variable=nom_climat_futur)


  data_long <- climat_futur_values %>%
    pivot_longer(
      cols = starts_with("P"),
      names_to = "Periode",
      values_to = "Variable"
    )


  data_long <- data_long %>%
    mutate(
      AnneeDebut = as.numeric(substr(Periode,2,5)),
      AnneeFin = as.numeric(substr(Periode,7,10)),
      AnneeCentrale = (AnneeDebut + AnneeFin) / 2
    )


   # 4. Appliquer pour chaque placette
  resultat <- data_long %>%
    group_by(id_pe) %>%
    arrange(AnneeCentrale) %>%
    group_modify(~ interpoler(.x, AnneeDep, AnneeFin, var= nom_climat_futur_var_i)) %>%
    rename(PlacetteID=id_pe)

  if (i==1){
  ClimTot<-resultat
  }else{
    suppressMessages(
      ClimTot<-ClimTot %>% left_join(resultat)
    )
  }

  }

  ClimTot$rcp<-RCP
  ClimTot<-ClimTot[,c(1,2,20,3:19)]

  return(ClimTot)
}
