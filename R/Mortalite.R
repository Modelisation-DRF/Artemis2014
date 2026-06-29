#' Fonction pour calculer la probabilité de mortalité d'un arbre. Fonction de base d'Artémis-2014.
#'
#'
#' @param Mort Un dataframe d'une ligne qui contient toutes les valeurs utilisées
#'             par les équations de probabilité de mortalité d'Artémis-2014
#'             et une colonne MCH indiquant d'utiliser l'équation de mortalité pour la MCH pour les HEG (0 ou 1).
#'
#' @param MCH Binaire. 1 = indique la présence de la maladie corticale du hêtre durant tout l'horizon de simulation.
#'                     0 = absence de la maladie (par défaut).
#'
#' @return Retourne la probabilité de mortalité pour la période de simulation en cours.
#'
#' @export
#'
mort<-function (Mort, MCH=0){

  # table avec une colonne avec le nom des effets et une colonne avec la valeur de la variable explicative correspondante
  # les effets sont en ordre alphabetique
  #Mort=as.data.frame(Mort[[1]])

  # tous les effets possibles à travers toutes les VP
  X<-data.frame(Effet.mort,
                "Value"=c(Mort$anc, Mort$Coupe, Mort$Coupe0, Mort$Coupe1, Mort$DHPcm,
                          Mort$DHPcm**2+Mort$Variance, ifelse(Mort$GrEspece=="SAB", Mort$DHPcm, 0), Mort$DHPcm,
                          1, ifelse(Mort$GrEspece=="SAB",Mort$Coupe0, 0),
                          1, 1, log(Mort$DHPcm)-0.5*log(Mort$Variance/Mort$DHPcm**2+1),
                          log(Mort$DHPcm)-0.5*log(Mort$Variance/Mort$DHPcm**2+1),log(Mort$t), log(Mort$t)*Mort$tbe,
                          log(Mort$t)*Mort$tbe, 0, Mort$n_arbre,
                          Mort$PTot, Mort$PTot, Mort$RegionOuest, Mort$st_ha_cumul_gt,
                          ifelse(Mort$GrEspece=="SAB", Mort$st_ha_cumul_gt, 0),Mort$st_ha_cumul_gt, Mort$sum_st_ha,
                          Mort$sum_st_ha, Mort$tbe, Mort$tbe, Mort$tbe1, Mort$TMoy))

  # selectionner les parametres de mortalite de la vp et du groupe d'essences de l'arbre
  Drainagei<-ifelse(Mort$Drainage %in% c(0,1,10,11),1,
                    ifelse(Mort$Drainage %in% c(20,30,2,3,21),2,
                           ifelse(Mort$Drainage %in% c(40,4,41,31),3,4)))

  ParaMorti<-Para[which(Para$Veg_Pot==Mort$Veg_Pot & Para$SubModuleID==1 &
                          (Para$Ess_groupe==Mort$GrEspece | is.na(Para$Ess_groupe)==TRUE) &
                          (is.na(Para$drainage)==TRUE| Para$drainage==Drainagei)),]

  # merger les parametres aux valeurs des variables, faire la multiplication et sommer tous les effets
  #cloglog <- inner_join(ParaMorti, X, by = "Effect") %>%
  #   summarise(cloglog=sum(ParameterEstimate * Value))
  Pred <- merge(ParaMorti,X,by = "Effect")
  cloglog <- sum(Pred$ParameterEstimate*Pred$Value)
  #if (Mort$MCH==1 & (Mort$Espece=='HEG' | Mort$GrEspece=='HEG')) {
  if (MCH==1 & (Mort$Espece %in% 'HEG' | Mort$GrEspece %in% 'HEG')) { # Mort$Espece=='HEG' retourne NA si Espece=NA (possible pour les recrues). Il faut soit vérifier qu'à la fois Espece n'est pas NA et Espece=HEG. On peut aussi utiliser le %in% car ne retourne jamais NA : PredMort1$Espece %in% "HEG"
    cloglog <- -5.4430 + 0.0621*Mort$DHPcm + log(Mort$t)
    }
  pred_mort=(1-exp(-exp(cloglog)))##Change sept 2023 pour retourner directement la prob de mortalite

  return(pred_mort)

}

