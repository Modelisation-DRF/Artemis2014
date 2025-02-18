#' Fonction d'accroissement diamétrale basé sur les modèles de D'Orangeville et al. 2019.
#'
#'
#'
#' @param Accrois Un dataframe présentant une ligne par arbre avec les variables
#'               dendrométriques nécessaires à l'application des modèles.
#'
#' @param ClimatGAM Un dataframe présentant les variables climatiques nécessaires
#'                  à l'application des modèles de D'Orangeville et al. 2019.
#'
#' @param Models Les modèles d'accroissement en diamétre de D'Orangevile et al. 2019
#'                en format .rds.
#'
#' @param t La période de temps de l'intervalle de croissance (10 ans par défaut).
#'
#' @return Retourne un dataframe incluant le numéro d'arbre et la prévision
#'        d'accroissement diamétrale  en cm.
#'
#' @examples
#' result <- AccroissementGAM(Accrois,ClimatGAM,Models,t)
#' print(result)
#' @export
#'

AccroissementGAM<-function(Accrois,ClimatGAM,Models,t){


  EssGrGAM<-Models[[7]]

  suppressMessages(
    Input<-Accrois %>%
      ungroup %>%
      mutate(TreeBA=(DHPcm/2)^2*pi, DHP_MM=DHPcm*10,
             TYPE_ECO=substr(Type_Eco,4,4),
             TYPE_ECO=ifelse(TYPE_ECO %in% c(1,2,3,4,5,6),TYPE_ECO,"org")) %>%
      left_join(EssGrGAM, by="GrEspece") %>% # le fichier ne contient pas les EPX, ni EPB/EPN
      mutate(Ess_regroupe=ifelse(Espece %in% c("EPN","EPB"), Espece, Ess_regroupe)) %>% # on met EPB/EPN pour les manquants
      mutate(Ess_regroupe=replace(Ess_regroupe,Espece=="EPR", "EPN")) %>%
      left_join(ClimatGAM) %>%
      rename(BAL=st_ha_cumul_gt, Ptot=PTotPeriode,Tmoy=TMoyPeriode, CMIPen_gs59=CMI, SLOPE=Pente, age_moy=Age_moy) %>%
      select(PlacetteID,origTreeID,DHPcm,DHP_MM, Ess_regroupe,BA,Ptot,Tmoy,Tmax_yr,CMIPen_gs59,Snow_cat, SLOPE,
             TYPE_ECO, age_moy,stand_stage, BAL, BA )
  )




  #Calcul des accroissements en cm2/an  avec le bon modele pour la bonne espece
  table(EssGrGAM$Ess_regroupe)
  Input$Bai_mod <- ifelse(Input$Ess_regroupe=="BOP", (exp(predict(Models[[1]], newdata=Input))*1.065403)-1, # smearing backtransfo exp(pred)*exp(1/2*mse)
                          ifelse(Input$Ess_regroupe=="EPB", (exp(predict(Models[[2]], newdata=Input))*1.107761)-1, # le -1 est-il à la bonne place?
                                 ifelse(Input$Ess_regroupe=="EPN", (exp(predict(Models[[3]], newdata=Input))*1.074767)-1,
                                        ifelse(Input$Ess_regroupe=="PIG", (exp(predict(Models[[4]], newdata=Input))*1.061282)-1,
                                               ifelse(Input$Ess_regroupe=="PET", (exp(predict(Models[[5]], newdata=Input))*1.066181)-1,
                                                      (exp(predict(Models[[6]], newdata=Input))*1.079217)-1))))) #SAB



  PredAcc<-Input %>%
    mutate(Bai_mod=ifelse(Bai_mod<0,0,Bai_mod*t)) %>%  ####La croissance annuelle est ramenée sur l'interval de temps de t
    mutate(pred_acc=2*(Bai_mod/pi+(DHPcm/2)**2)**0.5-DHPcm) %>%    # Calcul du DHP modélisé
    select( origTreeID,pred_acc)

  return(PredAcc)
}
