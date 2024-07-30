


#'
#'@param Data
#'
#'@param FacHa
#'
#'@return
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
