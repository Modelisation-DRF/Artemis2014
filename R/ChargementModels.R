
#' @param AccModif
#'
#' @param MortModif
#'
#' @return
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
