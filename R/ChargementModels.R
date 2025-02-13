#'Fonction qui charge les modèles d'accroissement en diametre et de mortalite
#'nessecaire à la simulation. Les modèle sélectionnees dependent des valeures entree
#'aux arguments ACCModif et MortModif
#'
#'
#'
#' @param AccModif Parametre permettant de selectionner la fonction d'accroissement diametrale
#'                  utilisee par le simulateur. Le parametre peut prendre la valeur "ORI" pour
#'                  les fonctions d'accroissement originales d'Artemis-2014, "BRT" pour la fonction
#'                  d'accroissement Boosted Regression Tree de Wang et al. 2023 ou "GAM" pour la fonction
#'                  d'accroissement de  D'Orangeville et al. 2019.
#'
#' @param MortModif Parametre permettant de selectionner la fonction de mortalité
#'                  utilisee par le simulateur. Le paramètre peut prendre la valeure "ORI"
#'                  pour les fonctions de mortalité originales d'Artemis-2014 ou "QUE"
#'                  pour les fonction de mortalité de Power et al. 2025.
#'
#' @return Retourne une liste incluant des modele .rds et des fichiers nécessaires à
#'        l'Utilisation des modèles.
#'
#' @examples
#' result <- ChargeModeles(AccModif, MortModif)
#' print(result)
#' @export
#'
ChargeModeles<- function(AccModif, MortModif){

    # Proportions d'épinettes IA: je l'ai sorti des if


  if (AccModif=="BRT") {
    # Modeles BRT
    mod_peu<-mod_peu_BRT
    mod_sab<-mod_sab_BRT
    mod_epn<-mod_epn_BRT
    mod_pig<-mod_pig_BRT
    mod_bop<-mod_bop_BRT
    mod_epb<-mod_epb_BRT
    # Essences BRT
    suppressMessages(
      EssGr<- EssGr_BRT) #IA: j'ai utilisé un nom générique EssGr au lieu de EssGrBRT

  }

  if (AccModif=="GAM") {
    # Modeles GAM
    mod_bop<-mod_bop_GAM
    mod_epb<-mod_epb_GAM
    mod_epn<-mod_epn_GAM
    mod_pig<-mod_pig_GAM
    mod_peu<-mod_peu_GAM
    mod_sab<-mod_sab_GAM
    # Essences GAM
    suppressMessages(
      EssGr<-EssGr_GAM) #IA: j'ai utilisé un nom générique EssGr au lieu de EssGrGam

  }

 if (AccModif=="ORI"){

   mod_bop=NA
   mod_epb=NA
   mod_epn=NA
   mod_pig=NA
   mod_peu=NA
   mod_sab=NA
   EssGr=NA

}

  if (MortModif=='QUE') {
    # Parametres mortalite QUE
    suppressMessages(
      EssGrMortQUE<-EssGrMortQUEt
    )
    suppressMessages(
      ParaMortQUE<-ParaMortQUEt
    )
    suppressMessages(
      CovParmMortQUE<-CovParmMortQUEt
    )
  }
  else{
    EssGrMortQUE=NA
    ParaMortQUE=NA
    CovParmMortQUE=NA
  }

  Models<-list(mod_bop,mod_epb,mod_epn,mod_pig,mod_peu,mod_sab,EssGr,PropEPX,EssGrMortQUE,ParaMortQUE,CovParmMortQUE)

return(Models)
}
