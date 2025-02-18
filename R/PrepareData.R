#' Fonction qui prépare les données et toutes les informations nécessaires pour effectuer
#' la simulation avec la fonction ArtemisClimat.
#'
#'
#'@param Data Un dataframe contenant une liste d'arbres avec leur characteristiques
#'            au départ de la simulation.
#'
#'@param Clim_tous Données climatiques mensuelles. Si absent laisser vide.
#'
#'@param ClimAn_tous Données climatiques annuelles. Si absent laisser vide.
#'
#'@param AccModif Choix de la fonction d'accroissement en diamètre: "ORI" pour les
#'                 équations originales d'Artémis 2014, "BRT" pour les équations
#'                 Boosted regression tree de JieJie Wang 2022, "GAM" pour les
#'                 équations GAM de D'Orangeville 2019.
#'
#'@param EvolClim Paramètre qui prend la valeure de 0 pour climat constant et
#'                 de 1 pour une évolution du climat à travers le temps de
#'                 simulation. Valeure par defaut de 0.
#'
#'@param MortModif Choix de fonction de mortalité "ORI" pour les équations
#'                 originales d'Artémis 2014,"QUE" pour les équation calibrées
#'                 par essence sensibles au climat de Power et al. 2025.
#'
#'@param RCP Scenario climatique choisi pour la simulation soit RCP 4.5 ou 8.5.
#'           Ce paramètre est seulement utilisé si le paramètre EvolClim=1.
#'
#'@param SpInd Un dataframe avec une colonne SpeciesID qui est un code d'essence
#'              et SpeciesName qui est le code d'essence usuel au Quebec.
#'
#'@param ListeVp Un dataframe avec une colonne VegPotID qui est un code numérique
#'                sequentiel de végétation potentielle et VegPotName qui
#'                est le code de végétation potentielle usuel au Québec.
#'
#'@param SpGroups Un dataframe associant les codes d'essence, de végétation
#'                 potentielles et de groupe d'essence.
#'
#'@param Sp Un dataframe avec une colonne SpeciesGroupID qui est un code
#'          numérique sequentiel de groupe d'essence et une colonne
#'          SpeciesGroupName qui correspond aux groupes d'essence d'Artémis-2014.
#'
#'@return Retourne une liste avec un dataframe des données formatées, les modèles
#'        en format .rds ansi que les paramètres de ces modèles, les données climatiques
#'        annuelles et mensuelles a être utilisées pour la simulation.
#'
#'@examples
#' result <- PrepareData(Data, Clim_tous, ClimAn_tous, AccModif, EvolClim, MortModif, RCP, SpInd, ListeVp, SpGroups, Sp)
#'
#' print(result)
#'@export
#'
PrepareData <- function(Data, Clim_tous, ClimAn_tous, AccModif, EvolClim, MortModif, RCP, SpInd, ListeVp, SpGroups, Sp) {



# Convertir Especes en GrEspece
Data<-Ess_groupe(Data, SpInd, ListeVp, SpGroups, Sp) #%>%
  #mutate(PlacetteID=as.character(PlacetteID),
   #      PlacetteID=paste("00",PlacetteID,sep=""),
    #     PlacetteID=substr(PlacetteID, nchar(PlacetteID)-10+1, nchar(PlacetteID)))


########################################################################################################
###Chargement des modules climat, des formes de pentes et des expositions##############################
##############des données climatiques pour simulations avec changements###############################
###########Mise en forme des pentes et expositions lorsque dans le fichier###########################
#############Attribution des pentes à partir de extract map#############################
####################################################################################################

#ListeMod<-c(rep("GAM",4),rep("BRT",4))
#ListeRCP<-c("RCP45","RCP45","RCP85","RCP85","RCP45","RCP45","RCP85","RCP85")
#ListeEvolCli<-c(1,0,1,0,1,0,1,0)
#ListeMort<-c(rep("QUE",8))

#for (l in 1:8){
# print(l)
#AccModif=ListeMod[l]
#RCP=ListeRCP[l]
#EvolClim=ListeEvolCli[l]
#MortModif=ListeMort[l]



if (AccModif!="ORI" | EvolClim==1 | MortModif=="QUE"){

  IndexPlacette<-Data %>% group_by(PlacetteID) %>% summarise()

  ClimMois <- Clim_tous %>%
    filter(rcp==RCP ) %>%
      #mutate(PlacetteID=as.character(PlacetteID),
       #      PlacetteID=paste("00",PlacetteID,sep=""),
        #     PlacetteID=substr(PlacetteID, nchar(PlacetteID)-10+1, nchar(PlacetteID))) %>%
      inner_join(IndexPlacette, by="PlacetteID")

  ClimAn <- ClimAn_tous %>%
    filter(rcp==RCP) %>%
     # mutate(PlacetteID=as.character(PlacetteID),
      #       PlacetteID=paste("00",PlacetteID,sep=""),
       #      PlacetteID=substr(PlacetteID, nchar(PlacetteID)-10+1, nchar(PlacetteID))) %>%
      inner_join(IndexPlacette, by="PlacetteID")
}else{
  ClimMois<-c()
  ClimAn<-c()
}

if (!(AccModif=="ORI" & MortModif=="ORI")){

  Models<-ChargeModeles(AccModif,MortModif)

  Vide<-Data[which(is.na(Data$Pente)==TRUE | is.na(Data$Exposition)==TRUE),]

  if (nrow(Vide)>0){

    Vide<-Vide %>%
          rename(id_pe=PlacetteID, latitude=Latitude, longitude=Longitude)

    Vide<-extract_map_plot(file=Vide, liste_raster="cartes_station", variable=c("pente","exposition"))

    Vide<-Vide %>%
          rename(PlacetteID=id_pe, Latitude=latitude, Longitude=longitude, Pente=pente, Exposition=exposition)

  }

  Plein<-Data[which(is.na(Data$Pente)==FALSE & is.na(Data$Exposition)==FALSE),]
  Data<-rbind(Plein,Vide)

  rm(Vide,Plein)

  Data<-Data %>%
    mutate(Aspect_Cat=ifelse(Exposition>120 & Exposition<=225,"W",
                             ifelse(Exposition>315 | Exposition<=45,"C","M"))) %>%
    mutate(Aspect_Cat=ifelse(Pente<=2,"C",Aspect_Cat))  #En accord avec Taylor et al. 2022 courriel JieJie Wang 22 aout
}else{
  Models<-c()
}

fic <- list(Data, Models, ClimMois, ClimAn)


return(fic)


}
