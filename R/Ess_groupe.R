
#'
#' @param Data
#'
#'@param SpInd
#'
#'@param ListeVp
#'
#'@param SpGroups
#'
#'@param Sp
#'
#' @return
#'
#' @examples
#' result <- Ess_groupe(Data, SpInd, ListeVp, SpGroups, Sp)
#'
#' print(result)
#' @export
#'
Ess_groupe<-function(Data, SpInd, ListeVp, SpGroups, Sp){

  # créer les groupes CHX, EPX, PEU et PIN
  Data<-Data %>%
    mutate (Espece_ori=Espece,
            Espece=ifelse(Espece %in% c("CHG","CHR","CHB"),"CHX",
                          ifelse(Espece %in% c("EPB","EPN","EPR"),"EPX",
                                 ifelse(Espece %in% c("PEG","PET","PEB","PED"), "PEU",
                                        ifelse (Espece %in% c("PIR","PIB","PIS"),"PIN",Espece)))))

  # Ne conserver que les essences individuelles traitées par artemis et attribuer le numero de l'essence
  suppressMessages(
             Data<-SpInd %>%
                   rename(Espece=SpeciesName) %>%
                   inner_join(Data, by="Espece", multiple = 'all'))

  # Ne conserver que les veg_pot traitées par artemis et attribuer le numero de la veg_pot
  suppressMessages(
           Data<-ListeVp %>%
                 rename(Veg_Pot=VegPotName) %>%
                 inner_join(Data, by="Veg_Pot", multiple='all'))

  # Attribuer le numéro des groupes d'essences en fonction de la veg_pot
  suppressMessages(
            Data<-SpGroups %>%
                  select(-RelationHDSpeciesID,-VolumeSpeciesID,-HarvestSpeciesID) %>%
                  inner_join(Data, by=c('VegPotID','SpeciesID'), multiple='all'))

  # Aller chercher le nom du groupes d'essences
  suppressMessages(
            Data<-Sp %>%
                  rename(GrEspece=SpeciesGroupName) %>%
                  inner_join(Data, by="SpeciesGroupID",  multiple='all') %>%
                  mutate(Espece=Espece_ori) %>%
                  select(-SpeciesGroupID,-VegPotID,-SpeciesID,-Espece_ori) %>%
                  select(PlacetteID,origTreeID,GrEspece,everything()))

return(Data)

}
