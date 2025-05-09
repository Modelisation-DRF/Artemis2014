

SortieSybille <- function(Data) {

  #Préparer toutes les colonnes pour utilisation de Sybille
  setDT(Data)

  #Calculer nbTi_ha et st_ha et inclure dans Data
  Data[Etat != 'mort', `:=`(
    nbTi_ha = sum(Nombre/0.04),
    st_ha = sum(pi*(DHPcm/200)^2 * Nombre/0.04)
  ), by = .(PlacetteID, Annee)]

  #Renommer les colonnes pour préparer le Data dans Sybille
  setnames(Data, c("Veg_Pot", "PlacetteID", "DHPcm", "Altitude", "hauteur_pred", "origTreeID", "Espece"),# "old_name2", "old_name2", "old_name2"),
             c("veg_pot", "id_pe", "DHP_Ae", "ALTITUDE", "HAUTEUR_M", "no_arbre", "essence"))# "no_arbre", "old_name2", "old_name2", "old_name2"))

  print(Data)
  Data_filter <- Data[, .(essence, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae, HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE)]
  print(Data_filter)
  Data_calculated <- OutilsDRF::calcul_vol_bille(Data_filter)
  print(Data_calculated)
}

###
Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3,Clim = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
 arrange(PlacetteID,origTreeID,Annee)

SortieSybille(Result)
