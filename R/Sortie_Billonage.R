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
           Espece = ifelse(Espece %in% c("CHR","CHG","CHB","CHE","CHX"), "CHR", Espece))
           #Espece = ifelse(is.na(Espece), Espece_original, Espece)
  # essences billonnage: BOJ ERR ERS HEG CHR BOP
  data <- Data %>% filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"))
  if (nrow(data) == 0) {
    # Copier la structure des colonnes nécessaires depuis Data
    cols_needed <- c("PlacetteID", "Annee", "origTreeID")
    final_transpo <- Data[0, ..cols_needed]

    # Ajouter les colonnes supplémentaires avec les types forcés
    final_transpo[, grade_bille := character(0)]
    final_transpo[, vol_bille_dm3 := numeric(0)]
    return(final_transpo)
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

  #On transpose les colonnes de Petro pour avoir la colonne grade_bille avec les valeurs
  billonage_cols <- c("DER", "F1", "F2", "F3", "F4", "P")
  existing_cols <- intersect(billonage_cols, colnames(final))

  final_transpo <- final %>%
    pivot_longer(cols = all_of(existing_cols),
                 names_to = "grade_bille",
                 values_to = "vol_bille_dm3") %>%
    select(PlacetteID, Annee, origTreeID, Residuel, grade_bille, vol_bille_dm3)

  #On enleve les possibles erreurs de fichiers en mettant le fichier en data.table
  final_transpo <- suppressMessages(setDT(final_transpo))

  return(final_transpo)
}
#Result <- Result[, Espece := "HHH"]

#result6 <- SortieBillonage(Result77, "DHP")
