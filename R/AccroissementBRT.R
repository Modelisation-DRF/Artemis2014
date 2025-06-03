#'Fonction qui estime l'accroissement en diamètre des arbres pour 1 interval de croissance.
#'La fonction utilise les modèles Boosted Regression Trees (BRT) de Wang et al. 2023
#'
#' @param Accrois Un dataframe contenant l'information à l'échelle de l'arbre.
#'
#' @param ClimatBRT Un dataframe contenant les variables climatiques utilisées par les modèles BRT.
#'
#' @param Models Une liste contenant les modèles BRT sous format .rds.
#'
#' @param sum_st_ha La surface terrière marchande de la placette.
#'
#' @param t La période de temps de l'intervalle de croissance (10 ans par défaut).
#'
#' @return Retourne un dataframe incluant le numéro d'arbre et la prévision
#'        d'accroissement diamétrale  en cm.
#'
#' @export
#'
AccroissementBRT<-function(Accrois,ClimatBRT, Models, sum_st_ha, t){


  EssGrBRT<-Models[[7]]

  suppressMessages(
    Input<-Accrois %>%
      ungroup %>%
      mutate(TreeBA=(DHPcm/2)^2*pi, BA9=sum_st_ha,Moist=as.factor(Moist),Aspect_Cat=as.factor(Aspect_Cat) ) %>%
      left_join(EssGrBRT, by="GrEspece") %>% # le fichier ne contient pas les EPX, ni EPB/EPN
      mutate(Ess_regroupe=ifelse(Espece %in% c("EPN","EPB"), Espece, Ess_regroupe)) %>% # on met EPB/EPN pour les manquants
      mutate(Ess_regroupe=replace(Ess_regroupe,Espece=="EPR", "EPN")) %>%
      left_join(ClimatBRT) %>%
      rename(Slope=Pente) %>%
      select(PlacetteID,origTreeID,DHPcm,Ess_regroupe,Max_ST,Min_WT,MSP,PAS,FFP,BA9,TreeBA,Moist,Slope,Aspect_Cat,CO2)
  )

  #Calcul des accroissements en cm2/an AVEC changements climat avec le bon modele pour la bonne espece

  Input$Bai_mod <- ifelse(Input$Ess_regroupe=="BOP",exp(predict(Models[[1]], newdata=Input, n.tree=1000))-1,
                          ifelse(Input$Ess_regroupe=="EPB", exp(predict(Models[[2]], newdata=Input,n.tree=1000))-1,
                                 ifelse(Input$Ess_regroupe=="EPN", exp(predict(Models[[3]], newdata=Input, n.tree=1000))-1,
                                        ifelse(Input$Ess_regroupe=="PIG", exp(predict(Models[[4]], newdata=Input, n.tree=2000))-1,
                                               ifelse(Input$Ess_regroupe=="PET", exp(predict(Models[[5]], newdata=Input, n.tree=1200))-1,
                                                      exp(predict(Models[[6]], newdata=Input, n.tree=1000))-1)))))###n.trees materiel supplémentaire Wang et al. 2022 PNAS



  PredAcc<-Input %>%
    mutate(Bai_mod=ifelse(Bai_mod<0,0,Bai_mod*t)) %>%  ####La croissance annuelle est ramenée sur l'interval de temps de t
    mutate(pred_acc=2*(Bai_mod/pi+(DHPcm/2)**2)**0.5-DHPcm) %>%    # Calcul du DHP modélisé en cm
    select( origTreeID,pred_acc)


  return(PredAcc)
}
