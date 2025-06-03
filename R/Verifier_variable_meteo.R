#' Fonction qui vérifie si les variables climatiques de base température annuelle moyenne,
#' précipitations totales et growing degree days. Si elle sont absentes, elles
#' sont estimée à l'aide du package extract_map.
#'
#'@param data Un dataframe contenant la liste d'arbres à simuler
#'
#'@return Retourne la liste d'arbre initiale avec les données météo ajoutée si
#'        elles sont absentes
#'
#'@export
#'
#'
vevifier_variable_meteo <- function(data){

  select=dplyr::select

  data <- data %>%
    rename(id_pe = PlacetteID, latitude = Latitude, longitude = Longitude)


  mes_variables <- c('GrwDays', 'PTot', 'TMoy')
  fonctions_validation <- list(valide_GrwDays, valide_Ptot, valide_Tmoy)
  noms_remplacement <- c("growingseasonlength", "totalprecipitation", "tmean")


  for (i in seq_along(mes_variables)) {
    if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
      data <- select(data, -!!rlang::sym(mes_variables[i]))
    }
  }


  variables_presentes <- intersect(mes_variables, names(data))
  for (col_names in variables_presentes) {
    if (!length(unique(data[[col_names]])) == 1) {
      data <- select(data, -!!rlang::sym(col_names))
    }

  }


  map_noms_variables <- c(GrwDays = "growingseasonlength",
                          PTot = "totalprecipitation",
                          TMoy = "tmean")

  variables_non_trouvees <- setdiff(mes_variables, names(data))

  if(!is_empty(variables_non_trouvees)){
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]

    data <- extract_map_plot(file=data, liste_raster="cartes_climat", variable=variables_a_extraire)

    if('tmean' %in% variables_a_extraire) {
      data <- rename(data, TMoy = tmean)
    }

    if('totalprecipitation' %in% variables_a_extraire) {
      data <- rename(data, PTot = totalprecipitation)
    }

    if('growingseasonlength' %in% variables_a_extraire) {
      data <- rename(data, GrwDays = growingseasonlength)
    }
  }

  data<-data %>% rename(PlacetteID=id_pe,Latitude = latitude, Longitude = longitude )

  return (data)
}

#' Fonction qui vérifie si la proportion de sable et de capacité d'échange cationique.
#' Si elle sont absentes, elles sont estimée à l'aide du package extract_map.
#'
#'@param data Un dataframe contenant la liste d'arbres à simuler.
#'
#'@return Retourne la liste d'arbres initiale avec les données de sol ajoutées si
#'        elles sont absentes.
#'
#'@export
#'
#'

vevifier_variable_Sol <- function(data){

  select=dplyr::select


  mes_variables <- c("cec_015cm","sand_015cm")
  fonctions_validation <- list(valide_cec, valide_sand)

  for (i in seq_along(mes_variables)) {
    if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
      data <- select(data, -!!rlang::sym(mes_variables[i]))
    }
  }

  data <- data %>%
    rename(id_pe = PlacetteID, latitude = Latitude, longitude = Longitude)



  variables_presentes <- intersect(mes_variables, names(data))
  for (col_names in variables_presentes) {
    if (!length(unique(data[[col_names]])) == 1) {
      data <- select(data, -!!rlang::sym(col_names))
    }
  }



  map_noms_variables <- c(sand_015cm = "sable",
                          cec_015cm = "cec")

  variables_non_trouvees <- setdiff(mes_variables, names(data))


  if(!is_empty(variables_non_trouvees)){
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]

    data <- extract_map_plot(file=data, liste_raster="cartes_sol", variable=variables_a_extraire, profondeur = 2)


    if('sable' %in% variables_a_extraire) {
      data <- rename(data, sand_015cm = sable)
    }

    if('cec' %in% variables_a_extraire) {
      data <- rename(data, cec_015cm = cec)
    }


  }
  data<-data %>% rename(PlacetteID=id_pe,Latitude = latitude, Longitude = longitude )

    return (data)


}


#' Fonction qui vérifie si la pente et l'exposition.
#' Si elle sont absentes, elles sont estimée à l'aide du package extract_map.
#'
#'@param data Un dataframe contenant la liste d'arbres à simuler.
#'
#'@return Retourne la liste d'arbres initiale avec les données de station ajoutées si
#'        elles sont absentes.
#'
#'@export
#'
#'
vevifier_variable_Sation <- function(data){

  select=dplyr::select


  mes_variables <- c("Pente", "Exposition")
  fonctions_validation <- list(valide_Pente, valide_Exposition)

  for (i in seq_along(mes_variables)) {
    if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
      data <- select(data, -!!rlang::sym(mes_variables[i]))
    }
  }

  data <- data %>%
    rename(id_pe = PlacetteID, latitude = Latitude, longitude = Longitude)



  variables_presentes <- intersect(mes_variables, names(data))
  for (col_names in variables_presentes) {
    if (!length(unique(data[[col_names]])) == 1) {
      data <- select(data, -!!rlang::sym(col_names))
    }
  }



  map_noms_variables <- c(Pente = "pente",
                          Exposition = "exposition")

  variables_non_trouvees <- setdiff(mes_variables, names(data))


  if(!is_empty(variables_non_trouvees)){
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]

    data <- extract_map_plot(file=data, liste_raster="cartes_station", variable=variables_a_extraire)


    if('pente' %in% variables_a_extraire) {
      data <- rename(data, Pente = pente)
    }

    if('exposition' %in% variables_a_extraire) {
      data <- rename(data, Exposition = exposition)
    }


  }
  data<-data %>% rename(PlacetteID=id_pe,Latitude = latitude, Longitude = longitude )

    return (data)


}

