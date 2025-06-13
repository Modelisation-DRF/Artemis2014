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
#'
#' @return Un data.table fusionné contenant:
#' \itemize{
#'   \item PlacetteID - Identifiant de la placette
#'   \item Annee - Année de l'arbre
#'   \item origTreeID - Identifiant original de l'arbre
#'   \item grade_bille - Grade de la bille (DER, F1, F2, F3, F4, P, ou grades Sybille
#'   \item vol_bille_dm3 - Volume de la bille en dm³
#'   \item Cl_Drai - Classe de drainage
#'   \item Altitude - Altitude du site
#'   \item Veg_Pot - Végétation potentielle
#'   \item DHPcm - Diamètre à hauteur de poitrine en cm
#'   \item hauteur_pred - Hauteur prédite en mètres
#'   \item Stm2ha - Surface terrière en m²/ha
#' }
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
#'
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
#'
#' @return Un data.table fusionné contenant:
#' \itemize{
#'   \item PlacetteID - Identifiant de la placette
#'   \item Annee - Année de l'arbre
#'   \item origTreeID - Identifiant original de l'arbre
#'   \item grade_bille - Grade de la bille (DER, F1, F2, F3, F4, P, ou grades Sybille
#'   \item vol_bille_dm3 - Volume de la bille en dm³
#'   \item Cl_Drai - Classe de drainage
#'   \item Altitude - Altitude du site
#'   \item Veg_Pot - Végétation potentielle
#'   \item DHPcm - Diamètre à hauteur de poitrine en cm
#'   \item hauteur_pred - Hauteur prédite en mètres
#'   \item Stm2ha - Surface terrière en m²/ha
#' }
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
#'
SortieBillesFusion <- function(Data, Type, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA, nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                               nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA) {
  setDT(Data)

  # On obtient Sybille
  Sybille <- SortieSybille(Data, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                           nom_grade3, long_grade3, diam_grade3)

  # Renommage des colonnes Sybille
  setnames(Sybille, c("cl_drai", "ALTITUDE", "veg_pot", "dhpcm", "HAUTEUR_M", "st_ha"),
           c("Cl_Drai", "Altitude", "Veg_Pot", "DHPcm", "hauteur_pred", "Stm2ha"), skip_absent = TRUE)

  # On obtient Petro
  Petro <- SortieBillonage(Data, Type)

  # Vérification simple et fix si nécessaire
  billonage_cols <- c("DER", "F1", "F2", "F3", "F4", "P")
  existing_cols <- intersect(billonage_cols, colnames(Petro))

  if(length(existing_cols) == 0) {
    # Si aucune colonne billonage, retourner juste Sybille
    cat("Aucune colonne billonage trouvée, retour Sybille seulement\n")
    Sybille[, c("st_ha", "nbTi_ha") := NULL]
    return(Sybille)
  }

  # Votre code existant avec les colonnes qui existent
  Petro_transpo <- melt(Petro,
                        measure.vars = existing_cols,
                        variable.name = "grade_bille")

  setnames(Petro_transpo, c("value"), c("vol_bille_dm3"))
  Fusion <- rbind(Petro_transpo, Sybille, fill = TRUE)

  # Nettoyer
  cols_to_remove <- intersect(c("diam_fb_cm", "long_bille_pied","st_ha", "nbTi_ha", "type"),
                              colnames(Fusion))
  if(length(cols_to_remove) > 0) {
    Fusion[, (cols_to_remove) := NULL]
  }

  Fusion[is.na(vol_bille_dm3), vol_bille_dm3 := 0.0]
  setorder(Fusion, PlacetteID, Annee, origTreeID)
  return(Fusion)
}

#Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3,Clim = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
 #arrange(PlacetteID,origTreeID,Annee)
#result1 <- SortieBillesFusion(Result, Type = "DHP2015", dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 4, diam_grade1 = 8)
