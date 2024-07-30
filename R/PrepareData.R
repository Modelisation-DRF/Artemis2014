

#'
#'@param Data
#'
#'@param Clim_tous
#'
#'@param ClimAn_tous
#'
#'@param AccModif
#'
#'@param EvolClim
#'
#'@param MortModif
#'
#'@param RCP
#'
#'@param SpInd
#'
#'@param ListeVp
#'
#'@param SpGroups
#'
#'@param Sp
#'
#'@return
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
#############Attribution des pentes à partir de raster lorsque abscente#############################
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

  Clim <- Clim_tous %>%
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
  Clim<-c()
  ClimAn<-c()
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

fic <- list(Data, Models, Clim, ClimAn)


return(fic)


}
