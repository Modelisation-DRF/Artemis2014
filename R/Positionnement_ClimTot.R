
#' Fonction qui produit des histogrammes des variables climatiques d'un fichier
#' ClimTot et qui les met en relation avec les données de calibrarion des modèles
#' d'accroissement en diamètre "QUE" et de mortalité "CANEU". Les lignes verticales rouges
#' représentent l'interval de confiance 95%. La fonction retourne une liste de graphique
#' soit un graphique par combinaison de modèle et d'essence
#'
#' @param Climat un fichier de données climatiques du même format que ceux générés
#'               par la fonction GenereClimatRaster
#'
#' @param Essence une liste d'essence pour lesquelles on veux vérifier la distribution
#'                du climat à travers les placettes.
#'
#'@param Model Une liste de modèles pour lesquels on veut repprésenter le climat en fonction
#'            de ses données de calibration. Les modèles disponibles sont seulement "Mort_CANEU"
#'            et "Acc_QUE" pour l'instant
#'
#'@param RCP Le scénario RCP présent dans le fichier Climat
#'
#'
#' @return La fonction retourne une liste de graphique ou chaque élément corespond à une combinaison de
#'         modèle et d'essence.
#'
#' @export

StatsClimTot<-function(Climat,Essence=NULL, Model=NULL, RCP=NULL){

  #Climat<-read.csv("P:/F1062/Modelisation/Hugues Power/Modeles/Sepaq-Bic/Climat85_41100.csv", sep=",")
  #StatsClimat<-read.csv("data-raw/StatsClimatModeles.csv", sep=";")

  if(is.null(Essence)==FALSE & is.null(Model)==FALSE){

    Param<-StatsClimatModel[which(StatsClimatModel$Essence %in% Essence & StatsClimatModel$Model %in% Model),]

  }

  if(is.null(Essence)==FALSE & is.null(Model)==TRUE){

    Param<-StatsClimatModel[which(StatsClimatModel$Essence %in% Essence),]

  }

  if(is.null(Essence)==TRUE & is.null(Model)==FALSE){

    Param<-StatsClimatModel[which(StatsClimatModel$Model %in% Model),]

  }

  if(is.null(Essence)==TRUE & is.null(Model)==TRUE){

    Param<-StatsClimatModel

  }


Climat$logPTot<-log(Climat$PTot)

Climat<-Climat %>%
        mutate(Decennie=case_when(Annee>1980 & Annee <=2010~"1980_2010",
                                  Annee>2010 & Annee<=2040~"2010_2040",
                                  Annee>2040 & Annee<=2070~"2040_2070",
                                 Annee>2070 & Annee<=2100~"2070_2100", .default = NA)) %>%
        filter(is.na(Decennie)==FALSE)

VarStats<-unique(Param$Variable)

Climat<-Climat %>%
        group_by(PlacetteID, Decennie) %>%
        summarise(
          across(all_of(VarStats),~mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )

#if("Mort_CANEU" %in% Param$Model){

setDT(Climat)
setDT(Param)

varsClim <- setdiff(names(Climat), c("PlacetteID", "Decennie"))

# Liste des combinaisons uniques

comb <- unique(Param[, .(Model, Essence)])

# Création de tous les jeux de données standardisés

ClimatStd <- data.table::rbindlist(
  lapply(seq_len(nrow(comb)), function(i){
    mod <- comb$Model[i]
    ess <- comb$Essence[i]

    # Paramètres de standardisation

    p <- Param[
      Model == mod &
      Essence == ess
    ]

    moy <- setNames(p$Moyenne, p$Variable)
    sd <- setNames(p$EcartType, p$Variable)

    vars.dispo <- intersect(varsClim, p$Variable)
    vars.manq <- setdiff(varsClim, p$Variable)

    # Copie pour éviter de modifier l'original

    tmp <- copy(Climat)

    # Standardisation des variables disponibles

    tmp[, (vars.dispo) :=
        lapply(vars.dispo,
               function(v)
               (get(v) - moy[v]) / sd[v]
        )]

    # Variables absentes des paramètres

    if (length(vars.manq) > 0) {
      tmp[, (vars.manq) := NA_real_]
    }



    # Identification du modèle et de l'essence

    tmp[, Model := mod]
    tmp[, Essence := ess]
    tmp

  }),

  use.names = TRUE,
  fill=TRUE

)


ClimatStd<-ClimatStd %>%
           data.table::melt(id.vars = c("PlacetteID", "Decennie", "Model", "Essence"),
                       variable.name = "Variable",value.name = "Valeur") %>%
           filter(is.na(Valeur)==FALSE) %>%
           left_join(Param)
#}

# if("ACC_QUE" %in% Param$Model){
#
#   ParamQUE<-Param %>%
#             filter(Model=="Acc_QUE") %>%
#             select(Essence,Model,Variable,Minimum,Maximum)
#
#   ClimatQUE<-Climat %>%
#              mutate(Model="Acc_QUE") %>%
#              data.table::melt(id.vars = c("PlacetteID", "Decennie", "Model"),
#                                variable.name = "Variable",value.name = "Valeur") %>%
#               inner_join(ParamQUE,relationship = "many-to-many")%>%
#               filter(is.na(Valeur)==FALSE)
#
#   GraphModeli<-ClimatQUE %>%
#
#     ggplot(aes(x=Valeur))+
#     geom_histogram()+
#     facet_grid(Essence~Variable+Decennie, scales="free_x")+
#     geom_vline(aes(xintercept=Minimum), colour="red")+
#     geom_vline(aes(xintercept=Maximum), colour="red")+
#     ggtitle(i)
#
#
#   }


ListeGraph<-list()
ClimatStd$Model_Essence<-paste0(ClimatStd$Model,"_",ClimatStd$Essence)

for (i in unique(ClimatStd$Model_Essence)){

  GraphModeli<-ClimatStd %>%
              filter(Model_Essence==i) %>%
              ggplot(aes(x=Valeur))+
                     geom_histogram()+
                     facet_grid(Variable~Decennie)+
                     geom_vline(aes(xintercept=Quant025STD), colour="red")+
                     geom_vline(aes(xintercept=Quant975STD), colour="red")+
                     ggtitle(paste0(i,"_",RCP))


  ListeGraph<-c(ListeGraph,GraphModeli)



}

return(ListeGraph)

}




