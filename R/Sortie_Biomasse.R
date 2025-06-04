
#' Fonction qui structure un dataframe de sortie dont chaque ligne correspond
#' à chacun des arbres par placette et par année et où la biomasse du bois, de l'écorce
#' du feuillage, des branches et la biomasse totale est rapportée à l'aide des équations
#' de Lambert et al. 2005.
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation du simulateur Artémis.
#'                   Typiquement un résultat retourné
#'                   par la fonction "simulateurArtemis".
#'
#' @return  Retourne un dataframe contenant l'ensemble des arbres pour chacune des
#'          placettes, années avec leur prévisions de biomasse.
#' @export

SortieBiomasse<-function (SimulHtVol){

ListeSp<-unique(ParaBiomasse$Espece)[which(is.na(unique(ParaBiomasse$Espece))==FALSE)]
SimulHtVolEss<-SimulHtVol %>%
               filter(Espece %in% ListeSp) %>%
               inner_join(ParaBiomasse,by=c("Veg_Pot","Espece"),relationship = "many-to-many") %>%
               rename(GrEspece=GrEspece.x) %>%
               select(-GrEspece.y)


SimulHtVolGrEss<-SimulHtVol %>%
                  filter(!Espece %in% ListeSp) %>%
                  inner_join(ParaBiomasse,by=c("Veg_Pot","GrEspece"),relationship = "many-to-many") %>%
                  rename(Espece=Espece.x) %>%
                  select(-Espece.y)

SimulHtVolTot<-rbind(SimulHtVolEss,SimulHtVolGrEss) %>%
               mutate(BioBois=awood*DHPcm^bwood*hauteur_pred^cwood,
                      BioEcorce=abark*DHPcm^bbark*hauteur_pred^cbark,
                      BioBranche=abranche*DHPcm^bbranche*hauteur_pred^cbranche,
                      BioFeuillage=afoliage*DHPcm^bfoliage*hauteur_pred^cfoliage,
                      BioTot=BioBois+BioEcorce+BioBranche+BioFeuillage) %>%
              select(PlacetteID,origTreeID,Espece,GrEspece,DHPcm,hauteur_pred,Nombre,Etat,BioBois,BioEcorce,BioBranche,BioFeuillage,BioTot)

rm(ListeSp,SimulHtVolEss)

return(SortieBiomasse)
}
