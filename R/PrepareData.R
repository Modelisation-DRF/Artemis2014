#'Fonction qui prépare les données en vue d'effectuer
#'une simulation avec le package Artémis-2014.
#'
#'
#'@param Data Un data frame qui contient la liste d'arbre de départ pour la
#'            simulation.
#'
#'@param Clim_tous Un data frame de données climatique mensuelles. Ce fichier
#'                 est seulement nécessaire pour les simulations effectuées avec
#'                 les modules d'accroissement ou de mortalité sensibles au climat.
#'
#'@param ClimAn_tous Un data frame de données climatiques annuelles. Ce fichier
#'                   est seulement nécessaire pour les simulations effectuées avec
#'                   les modules d'accroissement ou de mortalité sensibles au climat.
#'
#'@param AccModif Vecteur qui peut prendre les valeurs 'ORI' pour l'utilisation
#'                du module d'accroissement original d'Artémis-2014, 'BRT' pour
#'                les équations d'accroissement en diamètre de Wang. et al. 2023 ou
#'                'GAM' pour les équations d'accroissement de D'Orangeville et al. 2019.
#'
#'@param EvolClim Un vecteur qui prend la valeure binaire de 0 pour un climat stable
#'                ou de 1 pour un climat qui évolue. Si le climat évolue, il faut fournir
#'                les données climatiques pour chaque année de simulation avec les
#'                data frame Clim_tous et ClimAn_tous.
#'
#'
#'@param MortModif Un vecteur qui peut prendre la valeur 'ORI' pour l'utilisation
#'                  du module de mortalité original d'Artémis-2014 ou 'QUE' pour
#'                  utiliser les équations de mortalité de Power et al. 2025.
#'
#'@param RCP Vecteur qui prend la valeur 'RCP45' ou 'RCP85' selon le scénario de
#'           changements climatiques que l'on veut utiliser.
#'
#'@param SpInd Un data frame qui associe un code numérique à chacune des essences.
#'             Ce data frame est conservé en tant que données internes au package.
#'
#'@param ListeVp un data frame qui associe un code numérique à chacune des
#'               végétations potentielles. Ce data frame est conservé en tant que données
#'               internes au package.
#'
#'@param SpGroups Un data frame qui associe les codes numérique des essence, des
#'                groupes d'essence, des végétations potentielles des relations
#'                hauteur diamètre, de volume et de récolte.  Ce data frame est
#'                conservé en tant que données internes au package.
#'
#'@param Sp Un data frame qui associe un code numérique à chacun des groupes
#'          d'essences. Ce data frame est conservé en tant que données internes au package.
#'
#'@return La fonction retourne une liste comprenant le dataframe de données mis en forme,
#'        les modèles BRT ou GAM lorsqu'ils doivent être utilisés et les deux dataframes
#'        de données climatiques.
#'
#'@export
#'
PrepareData <- function(Data, Clim_tous, ClimAn_tous, ClimTot, AccModif, EvolClim, MortModif, RCP, SpInd, ListeVp, SpGroups, Sp) {



# Convertir Especes en GrEspece
Data<-Data %>%
      filter(DHPcm>=9.1) %>% ######Enlève les gaules qui peuvent être présentes
      mutate(Reg_Eco=substr(Reg_Eco,1,2))##Prend seulement les 2 premiers charactères

Data<-Ess_groupe(Data, SpInd, ListeVp, SpGroups, Sp)


########################################################################################################
###Chargement des modules climat, des formes de pentes et des expositions##############################
##############des données climatiques pour simulations avec changements###############################
###########Mise en forme des pentes et expositions lorsque dans le fichier###########################
#############Attribution des pentes à partir de raster lorsque abscente#############################
####################################################################################################



if (AccModif!="ORI" | EvolClim==1 | MortModif %in% c("QUE","CANEU")){

  IndexPlacette<-Data %>% group_by(PlacetteID) %>% summarise()

  if (!is.null(Clim_tous)){
  Clim <- Clim_tous %>%
    filter(rcp==RCP ) %>%
           inner_join(IndexPlacette, by="PlacetteID")
  }else{
    Clim<-c()
  }

  if (!is.null(ClimAn_tous)){
  ClimAn <- ClimAn_tous %>%
    filter(rcp==RCP) %>%
           inner_join(IndexPlacette, by="PlacetteID")
  }else{
    ClimAn<-c()
  }

  if (!is.null(ClimTot)){
    ClimTot <- ClimTot %>%
      filter(rcp==RCP) %>%
      inner_join(IndexPlacette, by="PlacetteID")
  }else{
    ClimTot<-c()
  }

}else{
  Clim<-c()
  ClimAn<-c()
  ClimTot<-c()
}

if (!(AccModif=="ORI" & MortModif=="ORI")){

  Models<-ChargeModeles(AccModif,MortModif)

  Vide<-Data[which(is.na(Data$Pente)==TRUE | is.na(Data$Exposition)==TRUE),]

  if (nrow(Vide)>0){
    Vide<-PentesAsp(Vide)
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

fic <- list(Data, Models, Clim, ClimAn, ClimTot)


return(fic)


}
