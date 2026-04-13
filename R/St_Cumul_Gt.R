#'Fonction qui calcule la surface terrière des arbres de diamètre plus grand
#'que l'arbre ciblé par le calcul
#'
#'@param Data Un dataframe qui contient une liste d'arbres structuré par placette
#'            avec leur diamètre à hauteur de poitrine.
#'
#'@param FacHa Facteur d'expansion à l'hectare de la placette
#'
#'@return Retourne le dataframe fourni initialement avec une colonne supplémentaire
#'         'st_ha_cumul_gt' qui rapporte la surface terrière des arbres de plus grand
#'         diamètre de chacun des arbres.
#'
#'@export
#'

BAL<-function (Data,FacHa){

suppressMessages(
  Data <- Data %>%
          group_by(PlacetteID) %>%
          arrange(PlacetteID, desc(DHPcm)) %>%
          mutate(ST_m2 =ifelse(Etat=="vivant",(DHPcm^2)*3.1416/40000*Nombre*FacHa,0)) %>%
          left_join(Clade, by="GrEspece"))

  bal<-Data %>% # IA: j'ai changé le nom du data pour que ça ne soit pas le même nom que la fct: BAL pour bal
       group_by(DHPcm) %>%
       summarise(ST_m2_BAL=sum(ST_m2)) %>%
       arrange(desc(DHPcm)) %>%
       mutate(st_ha_cumul_gt=(cumsum(ST_m2_BAL)-ST_m2_BAL)) %>%
       select(DHPcm,st_ha_cumul_gt)

  suppressMessages(
                  Data<-Data %>%
                        left_join(bal,by="DHPcm"))

  return(Data)

}

#'Fonction qui calcule l'indice de diversité des espèces de Shannon
#'
#'@param Data Un dataframe qui contient une liste d'arbres structuré par placette
#'            avec leur diamètre à hauteur de poitrine et leur nombre dans la placette.
#'
#'@param FacHa Facteur d'expansion à l'hectare de la placette
#'
#'@return retourne l'indice de Shannon.
#'
#'@export
#'

Shannon<-function(Data,FacHa){

         Data<-Data %>%
                  mutate(Nombre_ha=sum(Nombre)*FacHa) %>%
                  group_by(GrEspece) %>%
                  summarise(pi=sum(Nombre)*FacHa/first(Nombre_ha), pi2=pi*log2(pi))

          Shannon<--sum(Data$pi2)

         return (Shannon)
}


#'Fonction qui calcule l'indice de diversité des diamètres de Gini
#'
#'@param Data Un dataframe qui contient une liste d'arbres structuré par placette
#'            avec leur diamètre à hauteur de poitrine et leur nombre dans la placette.
#'
#'@return Retourne l'indice de Gini.
#'
#'@export
#'


Gini <- function(Data) {

        x <- sort(Data$DHPcm)

        n <- length(Data$DHPcm)

        Gini <- (2 * sum((1:n) * x)) / (n * sum(x)) - (n + 1) / n

       return(Gini)
}

