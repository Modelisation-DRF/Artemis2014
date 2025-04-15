#'Fonction qui calcule la surface terrière des arbres de diamètre plus grand
#'que l'arbre ciblé par le calcul
#'
#'@param Data Un dataframe qui contient une liste d'arbres structuré par placette
#'            avec leur diamètre à hauteur de poitrine.
#'
#'@param FacHa Facteur d'expansion à l'hectare de la placette
#'
#'@return retourne le dataframe fourni initialement avec une colonne supplémentaire
#'         'st_ha_cumul_gt' qui rapporte la surface terrière des arbres de plus grand
#'         diamètre de chacun des arbres.
#'
#'@examples
#' result <- BAL(Data,FacHa)
#'
#' print(result)
#'@export
#'

BAL<-function (Data,FacHa){


  Data <- Data %>%
    group_by(PlacetteID) %>%
    arrange(PlacetteID, desc(DHPcm)) %>%
    mutate(ST_m2 =ifelse(Etat=="vivant",(DHPcm^2)*3.1416/40000*Nombre*FacHa,0))

  bal<-Data %>% # IA: j'ai changé le nom du data pour que ça ne soit pas le même nom que la fct: BAL pour bal
       group_by(DHPcm) %>%
       summarise(ST_m2_BAL=sum(ST_m2)) %>%
       arrange(desc(DHPcm)) %>%
       mutate(st_ha_cumul_gt=(cumsum(ST_m2_BAL)-ST_m2_BAL)) %>%
      select(DHPcm,st_ha_cumul_gt)

  suppressMessages(
                  Data<-Data %>%
                        left_join(bal))

  return(Data)

}
