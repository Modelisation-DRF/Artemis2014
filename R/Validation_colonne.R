#' Répertoire des erreurs dans le fichier des arbres
#'
#' La fonction \code{valide_data} vérifie la validité des données dans un dataframe représentant
#' le fichier des arbres et retourne une liste des erreurs trouvées.
#'
#' @param data Un dataframe représentant le fichier des arbres.
#'
#' @return Une liste des erreurs trouvées dans le fichier des arbres.
#'
#' @examples
#' \dontrun{
#' # Supposons que nous ayons un dataframe `data` représentant le fichier des arbres
#'
#' # Appel de la fonction
#' erreurs <- valide_data(data)
#'
#' # La fonction retournera une liste des erreurs trouvées dans le dataframe `data`
#' }
#'
#' @export

valide_data <- function(data) {
  data <- renommer_les_colonnes(data)

  validations <- list(
    valide_espece = " Code d'essence à l'extérieur de la plage de valeurs possibles",
    valide_Etat = "Code d'état à l'extérieur de la plage de valeurs possibles",
    valide_DHPcm = "Valeurs de DHP non permise (>9.0 et <160)",
    valide_Nombre = "valeur de nombre non numèrique",
    valide_Latitude = "Latitude à l'extérieur de la plage de valeurs possibles (>45 et <52,5)",
    valide_Longitude = " Longitude à l'extérieur de la plage de valeurs possibles (>-79.75 et <-57.0)",
    valide_Altitude = " Altitude à l'extérieur de la plage de valeurs possibles (<1500)",
   # verifier_arbre_uniques_par_placette = "plusieurs noarbre identiques pour la même placette ",
    # valide_Ptot = "Ptot non valide",
    # valide_Tmoy = "Tmoy non valide",
    valide_Type_Eco = "Type écologique requis",
    valide_Reg_Eco = "Valeure non permise pour Reg_Eco",
    valide_Pente = "Pente à l'extérieur de la plage de valeurs permises (0>= et <=100)",
    valide_sand = "sand_015cm à l'extérieur de la plage de valeurs possibles (>=0 et =<100)",
    valide_cec = "cec_015cm à l'extérieur de la plage de valeurs possibles (>=0 et =<20)",
    valide_Dom_Bio = " Valeure non permise pour Sdom_Bio",
    valide_Sdom_Bio = "Valeure non permise pour Sdom_Bio",
    valide_Cl_Drai = "Valeure non permise pour classe de rainage",
    valide_Veg_Pot = "Valeure non permise pour Veg_Pot"
    # valide_GrwDays = "GrwDays non valide"
  )

  # Initialiser la liste des erreurs
  erreurs <-list()

  # Itérer sur chaque validation
  for (nom_validation in names(validations)) {
    # Appeler dynamiquement la fonction de validation en utilisant do.call
    valide <- do.call(nom_validation, list(data = data))

    # Si la validation échoue, ajouter le message d'erreur correspondant à la liste
    if (!valide) {
      erreurs <- c(erreurs, validations[[nom_validation]])
    }
  }

  return(erreurs)
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'nombre' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'  # Exemple avec un dataframe valide
#' valide_Nombre(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Nombre' manquante)
#' valide_Nombre(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs négatives)
#' valide_Nombre(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Nombre(data) # Devrait retourner FALSE

valide_Nombre <- function(data){

  if(!"Nombre" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Nombre) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Nombre))){
    return (FALSE)
  }


  return(is.numeric(data$Nombre))

}




#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Espece' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_espece(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Espece' manquante)
#' valide_espece(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_espece(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_espece(data) # Devrait retourner FALSE

valide_espece <- function(data){

  if(!"Espece" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Espece) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Espece)) ){
    return (FALSE)
  }

  valeurs_autorisees <- c("AME", "AUR", "ERE", "ERG", "ERP", "MAS", "OSV", "PRP", "SAL",
                          "SOA", "SOD", "BOJ", "EPB", "EPN", "EPO", "EPR", "ERR", "ERA",
                          "ERN", "ERS", "CAC", "CAF", "CAR", "CEO", "CET", "CHB", "CHE",
                          "CHG", "CHR", "FRA", "FRN", "FRP", "NOC", "ORA", "ORR", "ORT",
                          "TIL", "AME", "AUR", "ERE", "ERG", "ERP", "MAS", "OSV", "PRP",
                          "SAL", "SOA", "SOD", "BOJ", "EPB", "EPN", "EPO", "EPR", "ERR",
                          "ERA", "ERN", "ERS", "CAC", "CAF", "CAR", "CEO", "CET", "CHB",
                          "CHE", "CHG", "CHR", "FRA", "FRN", "FRP", "NOC", "ORA", "ORR",
                          "ORT", "TIL", "BOG", "BOP", "PEB", "PED", "PEG", "PEH", "PET",
                          "HEG", "JUV", "MEJ", "MEL", "MEU", "PIB", "PID", "PIG", "PIR",
                          "PIS", "PRU", "THO", "SAB")


  return(all(data$Espece %in% valeurs_autorisees))
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Etat' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#' # Exemple avec un dataframe valide
#' valide_Etat(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Etat' manquante)
#' valide_Etat(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_Etat(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Etat(data) # Devrait retourner FALSE

valide_Etat <- function(data){

  if(!"Etat" %in% names(data)){
    return (FALSE)
  }
  if(length(data$Etat) == 0){
    return(FALSE)
  }
  if(any(is.na(data$Etat)) ){
    return (FALSE)
  }

  valeurs_autorisees<-c(10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 26, 27,
                        28, 29, 30, 32, 34, 36, 40, 42, 43, 44, 46, 50, 52, 54, 56 )

  return(all(data$Etat %in% valeurs_autorisees))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'DHPcm' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_DHPcm(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'DHPcm' manquante)
#' valide_DHPcm(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs supérieures à 160 cm)
#' valide_DHPcm(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_DHPcm(data) # Devrait retourner FALSE

valide_DHPcm <- function(data){

  if(!"DHPcm" %in% names(data)){
    return (FALSE)
  }
  if( length(data$DHPcm) == 0){
    return(FALSE)
  }

  if(any(is.na(data$DHPcm)) ){
    return (FALSE)
  }
  return(all(between(data$DHPcm, 9.0, 160.0) ))

}



#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Latitude' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Latitude(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Latitude' manquante)
#' valide_Latitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Latitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Latitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Latitude' par 'Placette')
#' valide_Latitude(data) # Devrait retourner FALSE

valide_Latitude <- function(data){

  if(!all(c("PlacetteID", "Latitude") %in% names(data))){
    return (FALSE)
  }

  if(length(data$Latitude) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Latitude)) ){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique =  n_distinct(Latitude) == 1 && all(Latitude > 45 & Latitude < 52.5)

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Longitude' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Longitude(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Longitude' manquante)
#' valide_Longitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Longitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Longitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Longitude' par 'Placette')
#' valide_Longitude(data) # Devrait retourner FALSE

valide_Longitude <- function(data){

  if(!all(c("PlacetteID", "Longitude") %in% names(data))){
    return (FALSE)
  }

  if(length(data$Longitude) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Longitude)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(

      valeur_unique = n_distinct(Longitude) == 1 && all(Longitude > -79.75 & Longitude < -57.0)

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Altitude' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#'# Exemple avec un dataframe valide
#' valide_Altitude(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Altitude' manquante)
#' valide_Altitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Altitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Altitude(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Altitude' par 'Placette')
#' valide_Altitude(data) # Devrait retourner FALSE

valide_Altitude <- function(data){

  if(!all(c("PlacetteID", "Altitude") %in% names(data))){
    return (FALSE)
  }
  if(length(data$Altitude) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Altitude))){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Altitude) == 1 && all(Altitude < 1500)
    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Ptot' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#' # Exemple avec un dataframe valide
#' valide_Ptot(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Ptot' manquante)
#' valide_Ptot(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Ptot(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Ptot(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Ptot' par 'Placette')
#' valide_Ptot(data) # Devrait retourner FALSE
#'
valide_Ptot <- function(data){
  if(!all(c("PlacetteID", "Ptot") %in% names(data))|| any(is.na(data$Ptot))){
    return (FALSE)
  }


  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Ptot) == 1 && all(Ptot < 2000)
    )
  return(all(resultats$valeur_unique))

}




valide_cec <- function(data) {
  if(!all(c("PlacetteID", "cec_015cm") %in% names(data)) || any(is.na(data$cec_015cm))) {
    return(FALSE)
  }

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(cec_015cm) == 1 && all(between(cec_015cm, 0, 20))
    )

  return(all(resultats$valeur_unique))
}


valide_sand <- function(data) {
  if(!all(c("PlacetteID", "sand_015cm") %in% names(data)) || any(is.na(data$sand_015cm))) {
    return(FALSE)
  }

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(sand_015cm) == 1 && all(between(sand_015cm, 0, 100))
    )

  return(all(resultats$valeur_unique))
}



#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Tmoy' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'
#' # Exemple avec un dataframe valide
#' valide_Tmoy(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Tmoy' manquante)
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Tmoy' par 'Placette')
#' valide_Tmoy(data) # Devrait retourner FALSE
#'
valide_Tmoy <- function(data){
  if(!all(c("PlacetteID", "Tmoy") %in% names(data))|| any(is.na(data$Tmoy))){
    return (FALSE)
  }

  if(any(is.na(data$Tmoy)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Tmoy) == 1 && all(Tmoy > -10 & Tmoy < 10 )

    )

  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Type_Eco' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'# Exemple avec un dataframe valide
#' valide_Type_Eco(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Type_Eco' manquante)
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Type_Eco' par 'Placette')
#' valide_Type_Eco(data) # Devrait retourner FALSE
#'
valide_Type_Eco <- function(data){

  if (!all(c("PlacetteID", "Type_Eco") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Type_Eco) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Type_Eco)) ){
    return (FALSE)
  }
  valeurs_autorisees<-c('FC10', 'FC11', 'FC12', 'FE10', 'FE11', 'FE12', 'FE13', 'FE15', 'FE16', 'FE20', 'FE21',
                        'FE22', 'FE23', 'FE24', 'FE25', 'FE26', 'FE28', 'FE30', 'FE31', 'FE32', 'FE32H', 'FE33',
                        'FE34', 'FE35', 'FE36', 'FE42', 'FE43', 'FE45', 'FE50', 'FE51', 'FE52', 'FE52P', 'FE53',
                        'FE60', 'FE61', 'FE62', 'FE62P', 'FE65', 'FE66', 'FO10', 'FO12', 'FO14', 'FO15', 'FO16',
                        'FO18', 'ME13', 'ME16', 'MF12', 'MF15', 'MF16', 'MF18', 'MJ10', 'MJ11', 'MJ12', 'MJ12P',
                        'MJ13', 'MJ14', 'MJ15', 'MJ15P', 'MJ16', 'MJ18', 'MJ20', 'MJ20P', 'MJ21', 'MJ22', 'MJ22P',
                        'MJ23', 'MJ24', 'MJ24P', 'MJ25', 'MJ25P', 'MJ26', 'MJ28', 'MS10', 'MS10P', 'MS11', 'MS12',
                        'MS13', 'MS14', 'MS15', 'MS16', 'MS18', 'MS20', 'MS20P', 'MS21', 'MS22', 'MS22F', 'MS22P',
                        'MS23', 'MS23F', 'MS24', 'MS25', 'MS25F', 'MS25P', 'MS25Q', 'MS25S', 'MS26', 'MS26F', 'MS40',
                        'MS42', 'MS43', 'MS60', 'MS61', 'MS62', 'MS62P', 'MS63', 'MS65', 'MS66', 'RB10', 'RB11', 'RB12',
                        'RB13', 'RB14', 'RB15', 'RB16', 'RB17', 'RB18', 'RB22', 'RB23', 'RB51', 'RB52', 'RB53', 'RB55',
                        'RB55Q', 'RB56', 'RC37', 'RC38', 'RC39', 'RE10', 'RE11', 'RE11V', 'RE12', 'RE12P', 'RE13', 'RE14',
                        'RE15', 'RE15P', 'RE15Q', 'RE15S', 'RE16', 'RE20', 'RE21', 'RE21P', 'RE21Q', 'RE21V', 'RE22', 'RE22M',
                        'RE22P', 'RE23', 'RE24', 'RE25', 'RE25M', 'RE25P', 'RE25Q', 'RE25S', 'RE26', 'RE26S', 'RE32', 'RE37',
                        'RE37P', 'RE38', 'RE39', 'RE40', 'RE42', 'RP10', 'RP10P', 'RP11', 'RP12', 'RP13', 'RP14', 'RP15',
                        'RS10', 'RS11', 'RS12', 'RS12P', 'RS13', 'RS14', 'RS15', 'RS15P', 'RS16', 'RS18', 'RS20', 'RS20M',
                        'RS20P', 'RS21', 'RS22', 'RS22M', 'RS22P', 'RS22S', 'RS23', 'RS23M', 'RS24', 'RS24V', 'RS25', 'RS25M',
                        'RS25P', 'RS25Q', 'RS25S', 'RS26', 'RS34', 'RS35', 'RS37', 'RS37P', 'RS38', 'RS39', 'RS40', 'RS42', 'RS50',
                        'RS51', 'RS52', 'RS53', 'RS54', 'RS55', 'RS56', 'RS75', 'RT10', 'RT11', 'RT12', 'RT12P', 'RT14', 'RT15', 'RT16')

  if(!all(data$Type_Eco %in% valeurs_autorisees)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Type_Eco) == 1

    )
  return(all(resultats$valeur_unique))

}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Reg_Eco' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'# Exemple avec un dataframe valide
#' valide_Reg_Eco(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Reg_Eco' manquante)
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs non autorisées)
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Reg_Eco' par 'Placette')
#' valide_Reg_Eco(data) # Devrait retourner FALSE
#'
valide_Reg_Eco <- function(data){
  if (!all(c("PlacetteID", "Reg_Eco") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Reg_Eco) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Reg_Eco)) ){
    return (FALSE)
  }

  valeurs_autorisees<-c("1a", "2a", "2b", "2c", "3a", "3b", "3c", "3d", "4a",
                        "4b", "4c", "4d", "4e", "4f", "4g", "4h", "5a", "5b",
                        "5c", "5d", "5e", "5f", "5g", "5h", "5i", "5j", "5k",
                        "6a", "6b", "6c", "6d", "6e", "6f", "6g", "6h", "6i",
                        "6j", "6k", "6l", "6m", "6n", "6o", "6p", "6q", "6r",
                        "7a", "7b", "7c")

  if(!all(data$Reg_Eco %in% valeurs_autorisees)){
    return (FALSE)
  }

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Reg_Eco) == 1

    )
  return(all(resultats$valeur_unique))
}

#' Fonction pour vérifier que les valeurs saisies dans la colonne 'Pente' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#'# Exemple avec un dataframe valide
#' valide_Pente(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'Pente' manquante)
#' valide_Pente(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_Pente(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_Pente(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Pente' par 'Placette')
#' valide_Pente(data) # Devrait retourner FALSE
#'
valide_Pente <- function(data){
  if (!all(c("PlacetteID", "Pente") %in% names(data))) {
    return(TRUE)
  }
  if(length(data$Pente) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Pente)) ){
    return (TRUE)
  }
  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(

      valeur_unique = n_distinct(Pente) == 1 && (all(Pente >= 0 & Pente <= 100)|| is.na(Pente))
    )
  return(all(resultats$valeur_unique))
}


#' Fonction pour vérifier que les valeurs saisies dans la colonne 'GrwDays' sont correctes.
#' @param data fichier des arbres
#' @return retourne vrai ou faux s'il détecte des erreurs.
#' @examples
#' # Exemple avec un dataframe valide
#' valide_GrwDays(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'GrwDays' manquante)
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA)
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'GrwDays' par 'Placette')
#' valide_GrwDays(data) # Devrait retourner FALSE
#'
valide_GrwDays <- function(data){
  if(!all(c("GrwDays","PlacetteID") %in% names(data))|| any(is.na(data$GrwDays))){
    return (FALSE)
  }
  if(any(is.na(data$GrwDays)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(GrwDays) == 1 && all(GrwDays >=1 & GrwDays <365 )

    )
  return(all(resultats$valeur_unique))

}




valide_Dom_Bio <- function(data){
  if (!all(c("PlacetteID", "Dom_Bio") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Dom_Bio) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Dom_Bio)) ){
    return (FALSE)
  }

  valeurs_autorisees <- c('1', '2', '3', '4', '5', '6', '7',NA)

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(

      valeur_unique = n_distinct(Dom_Bio) == 1 && all(Dom_Bio %in% valeurs_autorisees)
    )
  return(all(resultats$valeur_unique))
}



valide_Sdom_Bio <- function(data) {
  # Check for required columns
  if (!all(c("PlacetteID", "Sdom_Bio") %in% names(data))) {
    return(FALSE)
  }

  # Check for empty data or NA values in Sdom_Bio
  if (nrow(data) == 0 || any(is.na(data$Sdom_Bio))) {
    return(FALSE)
  }

  # Define allowed values
  valeurs_autorisees <- c('1', '2E', '2O', '3E', '3O', '4E', '4O', '5E', '5O', '6E', '6O', '7E', NA)

  # Group by PlacetteID and summarize
  resultats <- data %>%
    group_by(PlacetteID) %>%
    summarize(
      valeur_unique = n_distinct(Sdom_Bio) == 1 & all(Sdom_Bio %in% valeurs_autorisees),
      .groups = 'drop'
    )

  # Return TRUE if all values are unique and authorized, otherwise FALSE
  return(all(resultats$valeur_unique))
}

valide_Cl_Drai <- function(data){
  if (!all(c("PlacetteID", "Cl_Drai") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Cl_Drai) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Cl_Drai)) ){
    return (FALSE)
  }

  valeurs_autorisees <- c("0", "10", "11", "12", "13", "14", "16"
                          , "20", "21", "22", "23", "24", "30", "31",
                          "32", "33", "34", "40", "41", "42", "43",
                          "44", "50", "51", "52", "53", "54", "60",
                          "61", "62", "63", "64" )

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(

      valeur_unique = n_distinct(Cl_Drai) == 1 && all(Cl_Drai %in% valeurs_autorisees)
    )
  return(all(resultats$valeur_unique))
}



valide_Veg_Pot <- function(data){
  if (!all(c("PlacetteID", "Veg_Pot") %in% names(data))) {
    return(FALSE)
  }
  if(length(data$Veg_Pot) == 0){
    return(FALSE)
  }

  if(any(is.na(data$Veg_Pot)) ){
    return (FALSE)
  }

  valeurs_autorisees <- c('FC1', 'FE1', 'FE2', 'FE3', 'FE4', 'FE5', 'FE6', 'FEX', 'FEY',
                          'FO1', 'MF1', 'MJ1', 'MJ2', 'MS1', 'MS2', 'MS6', 'RB1', 'RC3',
                          'RE1', 'RE2', 'RE3', 'RP1', 'RS1', 'RS2', 'RS3', 'RS5', 'RT1')

  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(

      valeur_unique = n_distinct(Veg_Pot) == 1 && all(Veg_Pot %in% valeurs_autorisees)
    )
  return(all(resultats$valeur_unique))
}



valide_Age_moy <- function(data){
  if(!all(c("Age_moy","PlacetteID") %in% names(data))|| any(is.na(data$Age_moy))){
    return (FALSE)
  }
  if(any(is.na(data$Age_moy)) ){
    return (FALSE)
  }
  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Age_moy) == 1 && all(Age_moy >=0 & Age_moy <999 )

    )
  return(all(resultats$valeur_unique))

}


valide_Exposition <- function(data){
  if(!all(c("Exposition","PlacetteID") %in% names(data))|| any(is.na(data$Exposition))){
    return (TRUE)
  }
  if(any(is.na(data$Exposition)) ){
    return (TRUE)
  }
  resultats <- data %>%
    group_by(PlacetteID) %>%
    reframe(
      valeur_unique = n_distinct(Exposition) == 1 && all(Exposition >=0 & Exposition <360 )

    )
  return(all(resultats$valeur_unique))

}


#' Fonction pour vérifier que chaque arbres est unique dans chaque placette
#' @param data fichier des arbres
#' @return Retourne vrai ou faux s'il y a des arbres qui se répètent.
#' @examples
#'  # Exemple avec un dataframe valide
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner TRUE
#'
#' # Exemple avec un dataframe invalide (colonne 'NoArbre' manquante)
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (valeurs NA dans 'Placette' ou 'NoArbre')
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner FALSE
#'
#' # Exemple avec un dataframe invalide (arbres non uniques dans une placette)
#' verifier_arbre_uniques_par_placette(data) # Devrait retourner FALSE
#'
verifier_arbre_uniques_par_placette <- function(data) {


  if (!all(c("Placette", "NoArbre") %in% names(data))) {
    return(FALSE)
  }
if(any(is.na(data$Placette))){
  return(FALSE)
}
  if(any(is.na(data$NoArbre))){
    return(FALSE)
  }
  if(length(data$Placette) == 0){
    return(FALSE)
  }
  if(length(data$NoArbre) == 0){
    return(FALSE)
  }

  resultats <- data %>%
    group_by(Placette) %>%
    reframe(
      tous_uniques = n_distinct(NoArbre) == n(),
      .groups = 'drop'
    )

  return(all(resultats$tous_uniques))

}



