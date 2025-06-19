#' Sortie Billonnage Petro
#'
#'
#' @param Data Un dataframe représentant le fichier d'arbres.
#' @param Type "DHP" pour utiliser les équations régionalisées basées seulement sur le DHP
#'             "DHP2015" pour utiliser les équations de 2015 basées seulement sur le DHP
#'
#'
#' @return Une liste des noms des colonnes manquantes.
#'
#' @export
#'
SortieBillonage <- function(Data, Type ){
  # Data=fic; Type="DHP2015"
  Data_ori <- Data
  select <- dplyr::select
  # Petro a une équation pour CHR, attribuer CHR aux 3 autres especes de chenes
  # Si recrue de plus de 23 cm, elle n'a rien dans Espece, utiliser GrEspece
  Data <- Data %>%
    filter(DHPcm > 23) %>%
    mutate(Espece_original = Espece,
           Espece = ifelse(GrEspece %in% c("CHR","CHG","CHB","CHE","CHX"), "CHR", GrEspece),
           Espece = ifelse(is.na(Espece), Espece_original, Espece))
  # essences billonnage: BOJ ERR ERS HEG CHR BOP
  data <- Data %>% filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"))
  if (nrow(data) == 0) {
    Data <- Data_ori %>% mutate(erreur = "Code d'essence a l'exterieur de la plage de valeurs possibles pour billonage")
    return(Data)
  }
  data1 <- data %>% mutate(bilonID = seq_len(nrow(data)))
  billo <- Billonage::SIMBillonnageABCD_DHP(data1, Type)

  final <- left_join(data1, billo, by = "bilonID") %>%
    dplyr::select(-Espece) %>%
    rename(Espece = Espece_original) %>%
    arrange(PlacetteID, Annee, GrEspece)

  final <- final %>% select(-bilonID)

  final <- final %>%
    mutate(across(c(DER, F1, F2, F3, F4, P), ~ .x * 1000))

   #Transformer sdom_bio pour correspondre au format Sybille
  final <- final %>%
    mutate(sdom_bio = ifelse(
      substr(sdom_bio, 2, 2) == "E",
      paste0(substr(sdom_bio, 1, 1), "EST"),
      ifelse(
        substr(sdom_bio, 2, 2) == "O",
        paste0(substr(sdom_bio, 1, 1), "OUEST"),
        sdom_bio
      )
    ))

    return(final)
  }
