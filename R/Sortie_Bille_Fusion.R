#' Fusion des sorties Sybille et Petro
#'
#' Cette fonction combine les résultats des fonctions SortieSybille et SortieBillonage
#' pour produire une table fusionnée des données avec les volumes par grade.
#'
#' @param Data Un data.frame ou data.table contenant les données d'inventaire forestier avec les colonnes suivantes:
#'   \itemize{
#'     \item Veg_Pot - Code de végétation potentielle, ex: "MS2"
#'     \item PlacetteID - Identifiant de la placette
#'     \item DHPcm - Diamètre à hauteur de poitrine en cm
#'     \item Altitude - Altitude en mètres
#'     \item hauteur_pred - Hauteur prédite de l'arbre en mètres
#'     \item origTreeID - Numéro de l'arbre
#'     \item Espece - Code d'essence de l'arbre, ex: "SAB"
#'     \item Cl_Drai - Classe de drainage ex: "2"
#'     \item Etat - État de l'arbre ("vivant", "mort", "recrue")
#'     \item Nombre - Nombre d'arbres représentés par cette observation dans une placette de 400 m2
#'     \item sdom_bio - Sous-domaine bioclimatique. ex: "1", "2E", "4O"
#'   }
#' @param Type Le type de billonage à utiliser pour la fonction SortieBillonage
#' @param dhs Hauteur de souche standard en mètres (point de départ des mesures), initialisée à 0.15(15 cm)
#' @param nom_grade1 Nom du premier type de bille.
#' @param nom_grade2 Nom du deuxième type de bille.(si besoin)
#' @param nom_grade3 Nom du troisième type de bille.(si besoin)
#' @param long_grade1 Longueur de la première bille en pieds(multiple de 2 pieds).
#' @param long_grade2 Longueur de la deuxième bille en pieds.(multiple de 2 pieds)(si besoin)
#' @param long_grade3 Longueur de la troisième bille en pieds.(multiple de 2 pieds)(si besoin)
#' @param diam_grade1 Diamètre minimal au fin bout de la première bille en cm.
#' @param diam_grade2 Diamètre minimal au fin bout de la deuxième bille en cm.(si besoin)
#' @param diam_grade3 Diamètre minimal au fin bout de la troisième bille en cm.(si besoin)
#' @param Simplifier Initialisé à FALSE. If TRUE, on garde que la première et dernière année de la simulation
#'
#' @return Un data.table fusionné contenant toutes les colonnes d'Artémis et les colonnes pour le billonage (grade_type et vol_bille_dm3)
#'
#' @details
#' La fonction effectue les opérations suivantes:
#' \itemize{
#'   \item Appelle SortieSybille avec les paramètres de grades spécifiés
#'   \item Standardise les noms de colonnes de Sybille
#'   \item Appelle SortieBillonage avec le type spécifié
#'   \item Transpose les données Petro pour avoir une ligne par coupe
#'   \item Fusionne les deux tables avec rbind
#'   \item Supprime les colonnes non nécessaires
#'   \item Remplace les valeurs NA de volume par 0.0
#'   \item Trie les données par PlacetteID, Annee, et origTreeID
#' }
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation basique
#' resultat <- SortieBillesFusion(mes_donnees, "TypeA", nom_grade1 = "Sciage long",  long_grade1 = 4, diam_grade1 = 8)
#'
#' # Avec paramètres de grades personnalisés
#' resultat <- SortieBillesFusion(
#'   mes_donnees,
#'   "TypeB",
#'   dhs = 0.20,
#'   nom_grade1 = "Sciage long",
#'   long_grade1 = 4,
#'   diam_grade1 = 8
#' )
#' }
#'
#' @seealso \code{\link{SortieSybille}}, \code{\link{SortieBillonage}}
#' @export
SortieBillesFusion <- function(Data, Type, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA, nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                               nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA, Simplifier = FALSE) {
  setDT(Data)

  # On obtient Sybille
  Sybille <- SortieSybille(Data, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                               nom_grade3, long_grade3, diam_grade3)

  Sybille <- Sybille[!is.na(grade_bille)]

  #If simplifier true, on garde que la première et dernière année de simulation
  Data_Arbre <- SortieArbre(Data, simplifier = Simplifier)

  # On obtient Petro
  Petro <- SortieBillonage(Data, Type)

  #On fusionne les 2
  Fusion <- rbind(Petro, Sybille, fill = TRUE)

  setDT(Fusion)

  #Arrondissement des valeurs à 6 décimales pour précision
  cols_to_round_data <- c("Nombre", "DHPcm", "Hautm", "ST_m2", "Vol_dm3")
  Data_Arbre[, (cols_to_round_data) := lapply(.SD, function(x) round(x, 6)), .SDcols = cols_to_round_data]

  Fusion[, vol_bille_dm3 := round(vol_bille_dm3, 6)]

  Fusion <- unique(Fusion)

  #On merge le Data de base avec notre fichier de billons
  Fusion_complete <- merge(Data_Arbre, Fusion,
                          by = c("PlacetteID", "Annee", "origTreeID"),
                          all.x = TRUE)

  #On remplace les NA par 0
  Fusion_complete[is.na(vol_bille_dm3), vol_bille_dm3 := 0.0]
  setorder(Fusion, PlacetteID, Annee, origTreeID)


#
  ## Maintenant appliquer unique()
  #Fusion_complete <- unique(Fusion_complete)

  return(Fusion_complete)
}

#vec_Coupe_ON <- c(1, NA, NA)
#test_ess <- data.frame(
#  ess_ind = c("CHR"),
#  modifier = c(50))
#vec_coupeModifier <- list(test_ess, NA, NA)
#TBE <- c(1,1,1)
#Result1 <- suppressMessages(simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3,ClimMois = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45',
#                                              Coupe_ON = vec_Coupe_ON, Coupe_modif = vec_coupeModifier, TBE = TBE) %>% arrange(PlacetteID,origTreeID,Annee))
#result445 <- SortieBillesFusion(Result77, Type = "DHP2015", dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 12, diam_grade1 = 12,
#  nom_grade2 = "sciage mid", long_grade2 = NA, diam_grade2 = 0, Simplifier = FALSE)
#result44 <- SortieBillesFusion(Result1, Type = "DHP2015", dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 12, diam_grade1 = 12,
#                              nom_grade2 = "sciage mid", long_grade2 = NA, diam_grade2 = 0)
