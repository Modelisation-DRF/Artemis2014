#' Fonction qui prévoie le nombre de recrues de plus de 9,1 cm pour chacune des étapes
#' de simulation
#'
#'
#'@param EspecesFinal un vecteur qui comprend un groupe d'espèce une colonne
#'                     raporte le nombre de tiges, le nombre de
#'                    tiges par hectare et la surface terrière.
#'
#'@return retourne un vecteur avec le nombre de recrue, leur DHP et la variance
#'
#'@examples
#' result <- fctrecrue(EspecesFinal)
#'
#' print(result)
#'@export
#'
#'
fctrecrue <- function(EspecesFinal) {

  # table avec une colonne avec le nom des effets et une colonne avec la valeur de la variable explicative correspondante
  # les effets sont en ordre alphabetique
  XRec <- data.frame(Effet.rec,
                     "Value"=c(EspecesFinal$anc, EspecesFinal$anc, EspecesFinal$Coupe,
                               ifelse(EspecesFinal$groupe=="HEG", EspecesFinal$Coupe, 0),
                               EspecesFinal$Coupe, EspecesFinal$Coupe0,
                               ifelse(EspecesFinal$groupe=="HEG", EspecesFinal$Coupe0, 0), EspecesFinal$Coupe1, 1, 1,
                               log(EspecesFinal$t), log(EspecesFinal$mq_DHPcm+1), log(EspecesFinal$n_arbre_ha+1),
                               log(EspecesFinal$sum_st_ha+1), log(EspecesFinal$sum_st_ha+1), EspecesFinal$mq_DHPcm,
                               EspecesFinal$mq_DHPcm, EspecesFinal$n_arbre_ha, EspecesFinal$nbtiges_ess,
                               EspecesFinal$nbtiges_ess, EspecesFinal$nbtiges_ess_ha,
                               EspecesFinal$nbtiges_ess_ha, EspecesFinal$PTot, EspecesFinal$PTot,
                               EspecesFinal$St_ha_ess, EspecesFinal$sum_st_ha, EspecesFinal$sum_st_ha,
                               EspecesFinal$sum_st_ha*EspecesFinal$nbtiges_ess,
                               EspecesFinal$tbe, EspecesFinal$tbe, EspecesFinal$tbe1,
                               EspecesFinal$TMoy, EspecesFinal$TMoy, ifelse(EspecesFinal$nbtiges_ess==0, 1, 0)))

  # selectionner les parametres de la presence de recrues
  ParaPres<-Para[which(Para$Veg_Pot==EspecesFinal$Veg_Pot & Para$SubModuleID==3 & (Para$Ess_groupe==EspecesFinal$groupe | is.na(Para$Ess_groupe)==TRUE)),]

  # selectionner les parametres du nombre de recures
  ParaNb<-Para[which(Para$Veg_Pot==EspecesFinal$Veg_Pot & Para$SubModuleID==4 & (Para$Ess_groupe==EspecesFinal$groupe | is.na(Para$Ess_groupe)==TRUE)),]
  # selectionner les parametres du dhp des recrues
  ParaDHP<-Para[which(Para$Veg_Pot==EspecesFinal$Veg_Pot & Para$SubModuleID==5 & (Para$Ess_groupe==EspecesFinal$groupe | is.na(Para$Ess_groupe)==TRUE)),]

  # merger les parametres aux valeurs des variables, faire la multipilcation et sommer tous les effets
  PredPres<-merge(ParaPres,XRec,by = "Effect")
  PredPres$Pred<-PredPres$ParameterEstimate*PredPres$Value
  Pres<-exp(sum(PredPres$Pred))/(1+exp(sum(PredPres$Pred)))

  PredNb<-merge(ParaNb,XRec,by = "Effect")
  PredNb$Pred<-PredNb$ParameterEstimate*PredNb$Value
  Nb<-exp(sum(PredNb$Pred))+1

  PredDHP<-merge(ParaDHP,XRec,by = "Effect")
  PredDHP$Pred<-PredDHP$ParameterEstimate*PredDHP$Value
  DHP<-exp(sum(PredDHP$Pred))/10+9.1

  NbTot<-Nb*Pres


  Recrue_Variance <- exp(2*sum(PredDHP$Pred))/100*ParaDHP$ParameterEstimate[nrow(ParaDHP)]

  pred_recrue <- paste(NbTot, DHP, Recrue_Variance)
}
