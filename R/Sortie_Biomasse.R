


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


return(SortieBiomasse)
}
