#' Fonction qui associe un groupe d'essence a chacun des arbres des données
#' initiales. Les groupes d'essences sont ceux d'Artémis-2014.
#'
#' @param Data Un dataframe contenant une liste d'arbres pour lesquels on veut
#'             associer un groupe d'essence à partir d'un champ "Essence".
#'
#'
#' @param SpInd Un dataframe avec une colonne SpeciesID qui est un code numérique séquentiel
#'              et SpeciesName qui est le code d'essence usuel au Quebec.
#'
#' @param ListeVp Un dataframe avec une colonne VegPotID qui est un code numérique
#'                sequentiel de végétation potentielle et VegPotName qui
#'                est le code de végétation potentielle usuel au Quebec.
#'
#' @param SpGroups Un dataframe associant les codes d'essence, de végétation
#'                 potentielles et de groupe d'essences.
#'
#' @param Sp Un dataframe avec une colonne SpeciesGroupID qui est un code
#'           numérique séquentiel de groupe d'essence et une colonne
#'           SpeciesGroupName qui correspond aux groupes d'essences d'Artémis-2014.
#'
#' @return Retourne un dataframe dans lequel chacun des arbres se voient associer
#'          un groupe d'essence.
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
