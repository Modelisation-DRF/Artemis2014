#' Fonction pour calculer l'accroissement diamétrale. Fonction de base d'Artémis-2014
#'
#'
#' @param Accrois Un dataframe d'une ligne qui contient toutes les valeurs utilisées
#'                par les équations d'accroissement d'Artémis.
#'
#' @return Retourne la prévision d'accroissement diamétrale en cm de l'arbre
#'         pour un interval de croissance.
#'
#' @export
#'
accrois<-function(Accrois){

  # table avec une colonne avec le nom des effets et une colonne avec la valeur de la variable explicative correspondante
  # les effets sont en ordre alphabetique
  Xacc<-data.frame(
    Effet.acc,
    "Value"=c(Accrois$anc, Accrois$Coupe0, Accrois$DHPcm,
              Accrois$DHPcm^2+Accrois$Variance,
              ifelse(Accrois$GrEspece=="SAB", Accrois$DHPcm^2+Accrois$Variance, 0),
              Accrois$DHPcm^2+Accrois$Variance,
              Accrois$DHPcm, 1, 1,
              log(Accrois$DHPcm)-0.5*log(Accrois$Variance/Accrois$DHPcm**2+1), log(Accrois$t),
              0,log(Accrois$sum_st_ha+1),log(Accrois$sum_st_ha+1),
              Accrois$PTot, Accrois$PTot, Accrois$st_ha_cumul_gt,
              Accrois$sum_st_ha, Accrois$sum_st_ha, Accrois$tbe,
              Accrois$tbe, Accrois$tbe1, Accrois$TMoy,Accrois$TMoy))

  # selectionner les parametres d'accroissement de la vp et du groupe d'Especes de l'arbre
  ParaAcc<-Para[which(Para$Veg_Pot==Accrois$Veg_Pot&Para$SubModuleID==2&(Para$Ess_groupe==Accrois$GrEspece|is.na(Para$Ess_groupe)==TRUE)),]

  # merger les parametres aux valeurs des variables, faire la multiplcation et sommer tous les effets
  Predacc <- merge(ParaAcc,Xacc,by = "Effect")
  acc <- sum(Predacc$ParameterEstimate*Predacc$Value)

}
