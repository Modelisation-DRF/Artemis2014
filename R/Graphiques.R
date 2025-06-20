#' Fonction qui permet de produire des graphiques des résultats de simulation.
#'
#' @param Data Une sortie de simulation du package Artemis.
#'
#' @param Espece Le code de groupe d'espece pour lequel on veut produire le
#'               graphique. Par défaut le graphique est produit pour le
#'               groupe d'essence "TOT".
#'
#' @param Variable Variable pour laquelle on veut produire le graphique.
#'                 Ce paramètre peut prendre la valeur "ST_HA" pour la surface
#'                 terriere, "Vol_HA" pour le volume marchand brut, "DQM" pour
#'                 le diamètre quadratique moyen, "nbTi_HA" pour le nombre de
#'                 tiges marchandes par ha.
#'
#' @param listePlacette Une liste qui comprend les identifiants de placettes
#'                      pour lesquelles on veut produire le graphique. Par defaut
#'                      toutes les placettes sont selectionnées.
#'
#' @return Retourne un objet ggplot qui est un graphique.
#'
#'
#' @export
#'
#'
Graph <- function (Data, Espece="TOT", Variable='ST_HA',listePlacette){

  if (length(listePlacette) == 0) {
    listePlacette <- unique(Data$PlacetteID)
  }


 Data <-SortiePlacette(Data) %>%
        filter(GrEspece==Espece & PlacetteID %in% listePlacette)

  if (Variable=='ST_HA'){
    Data$Yvar<-Data$ST_HA
    Etiquette="Surface terri\uE8re marchande (m2/ha)"
  }

  if (Variable=='Vol_HA'){
    Data$Yvar<-Data$Vol_HA
    Etiquette="Volume marchand (m3/ha)"
  }

  if (Variable=='DMQ'){
    Data$Yvar<-Data$DMQ
    Etiquette="Diam\uE8tre quadratique moyen (cm)"
  }

  if (Variable=='nbTi_HA'){
    Data$Yvar<-Data$nbTi_HA
    Etiquette="Densit\uE9 (nb/ha)"
  }


  if (Espece == "TOT") {Essence="Toutes essences"}
  if (Espece == "AUT") {Essence = "Autres essences"}
  if (Espece == "BOG") {Essence = "Feuillus intol\u00E9rants"}
  if (Espece == "BOJ") {Essence = "Bouleau jaune"}
  if (Espece == "BOP") {Essence = "Feuillus intol\u00E9rants"}
  if (Espece == "CET") {Essence = "Feuillus noble"}
  if (Espece == "CHX") {Essence = "Feuillus noble"}
  if (Espece == "EPX") {Essence = "\u00C9pinettes"}
  if (Espece == "ERP") {Essence = "Feuillus non commercial"}
  if (Espece == "ERR") {Essence = "\u00C9rable rouge"}
  if (Espece == "ERS") {Essence = "\u00C9rable \u00E0 sucre"}
  if (Espece == "F0R") {Essence = "Autres essences"}
  if (Espece == "FEU") {Essence = "Autres essences"}
  if (Espece == "FRA") {Essence = "Feuillus noble"}
  if (Espece == "FRN") {Essence = "Feuillus noble"}
  if (Espece == "F_0") {Essence = "Feuillus noble"}
  if (Espece == "F_1") {Essence = "Feuillus intol\u00E9rants"}
  if (Espece == "HEG") {Essence = "H\uE8tre \u00E0 grandes feuilles"}
  if (Espece == "MEL") {Essence = "R\uE9sineux"}
  if (Espece == "ORA") {Essence = "Feuillus noble"}
  if (Espece == "OSV") {Essence = "Feuillus noble"}
  if (Espece == "PEU") {Essence = "Feuillus intol\u00E9rants"}
  if (Espece == "PIG") {Essence = "R\uE9sineux"}
  if (Espece == "PIN") {Essence = "R\uE9sineux"}
  if (Espece == "PRP") {Essence = "Feuillus non commercial"}
  if (Espece == "PRU") {Essence = "R\uE9sineux"}
  if (Espece == "RES") {Essence = "R\uE9sineux"}
  if (Espece == "SAB") {Essence = "Sapin baumier"}
  if (Espece == "SAL") {Essence = "Feuillus non commercial"}
  if (Espece == "SOA") {Essence = "Feuillus non commercial"}
  if (Espece == "THO") {Essence = "R\uE9sineux"}
  if (Espece == "TIL") {Essence = "Feuillus noble"}


  ymax<-max(Data$Yvar)

  AnneeDep=min(Data$Annee)
  AnneeFin=max(Data$Annee)

  Yvar_min=floor(min(Data$Yvar))
  Yvar_max=ceiling(max(Data$Yvar))
  range_val <- Yvar_max - Yvar_min
  by_value <- range_val / 10

  dernieres_valeurs <- Data %>%
    group_by(PlacetteID) %>%
    slice(n()) %>%
    ungroup()


  GraphEvol<-Data%>%
    ggplot(aes(x=Annee,y=Yvar,group=PlacetteID, label = PlacetteID))+
    geom_line(aes(),show.legend=FALSE, lwd=1.25, colour="#b0cdf8")+
    ylim(0,ymax+5)+
    xlab(bquote(bold("Ann\uE9\u65 de la simulation")))+ ylab(paste(Etiquette))+
    scale_x_continuous(breaks = seq(AnneeDep, AnneeFin, by = 10))+
    scale_y_continuous(breaks = seq(Yvar_min, Yvar_max, by= by_value))+
    theme_bw() +
    ggtitle(paste(Etiquette,"  ",Essence))+
    theme(
      strip.background = element_rect(fill = "white"),
      axis.title=element_text(size=14,face="bold"),
      axis.text.x = element_text(angle = 45,  hjust=1),
      strip.text.x = element_text(size = 12,face="bold"),
      plot.title = element_text(hjust = 0.5,size=14,face="bold"))+
    geom_text(data=dernieres_valeurs,aes(label = PlacetteID), hjust = 1, vjust=-0.2,size=3)
  GraphEvol


  return(GraphEvol)
}
