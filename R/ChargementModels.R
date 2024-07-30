
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
    suppressMessages(
      PropEPX<-read_delim("Parametres/PropEPX.csv", delim=";")
    )

  if (AccModif=="BRT") {
    # Modeles BRT
    mod_peu<-readRDS("Data/BRT/GBM_aspen_MSP_MT6_Mar2022.rds")
    mod_sab<-readRDS("Data/BRT/GBM_bf_MSP_MT6_Mar2022.rds")
    mod_epn<-readRDS("Data/BRT/GBM_bs_MSP_MT6_Mar2022.rds")
    mod_pig<-readRDS("Data/BRT/GBM_jp_MSP_MT6_Mar2022.rds")
    mod_bop<-readRDS("Data/BRT/GBM_wb_MSP_MT6_Mar2022.rds")
    mod_epb<-readRDS("Data/BRT/GBM_ws_MSP_MT6_Mar2022.rds")
    # Essences BRT
    suppressMessages(
      EssGr<-read_delim("Data/BRT/Ess_Groupe_RegroupeBRT.csv", delim=";")) #IA: j'ai utilisé un nom générique EssGr au lieu de EssGrBRT

  }

  if (AccModif=="GAM") {
    # Modeles GAM
    mod_bop<-readRDS("Data/GAM/mod_bop.rds")
    mod_epb<-readRDS("Data/GAM/mod_epb.rds")
    mod_epn<-readRDS("Data/GAM/mod_epn.rds")
    mod_pig<-readRDS("Data/GAM/mod_pig.rds")
    mod_peu<-readRDS("Data/GAM/mod_peu.rds")
    mod_sab<-readRDS("Data/GAM/mod_sab.rds")
    # Essences GAM
    suppressMessages(
      EssGr<-read_delim("Data/GAM/Ess_Groupe_RegroupeGAM.csv", delim=";")) #IA: j'ai utilisé un nom générique EssGr au lieu de EssGrGam

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
      EssGrMortQUE<-read_delim("Data/MortQUE/Ess_Groupe_RegroupeMortQUE.csv", delim=";")
    )
    suppressMessages(
      ParaMortQUE<-read_delim("Data/MortQUE/ParaTOT.csv", delim=";")
    )
    suppressMessages(
      CovParmMortQUE<-read_delim("Data/MortQUE/CovParmsTOT.csv", delim=";")
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
