#' Fonction de prévision de la probabilité de mortalité basée sur les équations
#' de mortalité de Power et al. 2025.
#'
#'
#'@param Mort Un dataframe contenant une liste d'arbres avec les variables utilisées
#'            pour faire les prévisions.
#'
#'@param ClimatQUE Un dataframe contenenant les variables climatiques crées par la fonction
#'                 ClimatBiosim pour être utilisé dans les fonctions d'accroissement et de
#'                 mortalité sensibles au climat.
#'
#'@param Models Liste contenant les modeles .rds a être utilisés dans la simulation
#'              ainsi que les associations d'essences et les paramètres des modèles.
#'
#'@param DrainageCl Classe de drainage ou les drainages hydriques et xériques sont
#'                  regrouppeés en "HydXer" et les drainages mésiques et sub hydriques
#'                  sont regrouppés en "MesSub".
#'
#'@param PenteCl Classe de pente de la placette selon la claissification québécoise.
#'               La classe de pente est exprimée par une lettre et les pentes "E" et
#'               "F" sont regrouppées dans la classe "EF".
#'
#'@param Texture Classe de texture derivée du 4ieme charactère du type écologique.
#'                les texture sont regrouppées en "FinMoy" pour les texture fines
#'                et moyennes et "GrosOrg" pour les textures grossières et les sols
#'                organiques.
#'
#'@param Coupe Variable binaire où la valeure 1 identifie les placettes où une
#'             coupe partielle a été effectuée il y a plus de 10 ans mais moins de 20 ans.
#'
#'@param Coupe0 Variable binaire où la valeure 1 identifie les placettes ou une
#'             coupe partielle a été effectuee il y a moins de 10 ans.
#'
#'@param sum_st_ha Surface terriere marchande de la placette au debut de l'étape
#'                 de simulation.
#'
#'@param t Durée de l'étape de simulation (généralement 10 ans).
#'
#' @return Retourne un dataframe avec une colonne origTreeID et une colonne avec la
#'          probabilité de mortalité pour la période de simulation en cours.
#'
#' @export
#'
mortCANEU<-function(Mort, ClimatQUE, Models, DrainageCl, PenteCl, Texture, Coupe,
                  Coupe0, sum_st_ha, sum_st_ha_Res, sum_st_ha_Feu, t, mq_DHPcm, shannon, gini, Depot){

       EssGrCANEU<-Models[[12]]
       ParaMortCANEU<-Models[[13]] %>% mutate(Effect = str_to_lower(Effect)) %>% arrange(Effect,Essence)
       CovParmsCANEU<-Models[[14]]


       #######################Standardisation des variables#####################

       ParaSTD<-ParaMortCANEU %>%
                filter(Model=="CANEU" & is.na(Moyenne)==FALSE) %>%
                select(Essence,Effect,Moyenne,EcartType) %>%
                pivot_wider(names_from=Effect, values_from=c(Moyenne,EcartType)) %>%
                rename(Ess_regroupe=Essence)

       cols<-names(ParaSTD)[2:16]
       ParaSTD[cols][is.na(ParaSTD[cols])]<-0
       cols<-names(ParaSTD)[17:31]
       ParaSTD[cols][is.na(ParaSTD[cols])]<-1



 suppressMessages(
          Input<-Mort %>%
            ungroup %>%
            left_join(EssGrCANEU, by="GrEspece") %>% # le fichier ne contient pas les EPX, ni EPB/EPN
            mutate(Ess_regroupe=ifelse(Espece %in% c("EPN","EPB","EPR","PEB","PEG","PIB"), Espece, Ess_regroupe)) %>% # on associe les espèces regroupées aucx bonnes équations
            left_join(ClimatQUE))

 suppressMessages(
 Input<-Input %>%
           left_join(ParaSTD) %>%
           mutate(logDHPcm=log(DHPcm), logPTot=log(PTot)) %>%
           mutate(DDStd=(DD-Moyenne_dd)/EcartType_dd, DHPcmStd=(DHPcm-Moyenne_dhpcm)/EcartType_dhpcm,
                  giniStd=(gini-Moyenne_gini)/EcartType_gini,logDHPcmStd=(logDHPcm-Moyenne_logdhpcm)/EcartType_logdhpcm,
                  logPTotStd=(logPTot-Moyenne_logptot)/EcartType_logptot,PTotStd=(PTot-Moyenne_ptot)/EcartType_ptot,
                  PUtilStd=(PUtile-Moyenne_putil)/EcartType_putil,mq_DHPcmStd=(mq_DHPcm-Moyenne_qmd)/EcartType_qmd,
                  shannonStd=(shannon-Moyenne_shannon)/EcartType_shannon,st_ha_cumul_gtStd=(st_ha_cumul_gt-Moyenne_st_ha_cumul_gt)/EcartType_st_ha_cumul_gt,
                  sum_st_haStd=(sum_st_ha-Moyenne_sum_st_ha)/EcartType_sum_st_ha,
                  sum_st_ha_ResStd=(sum_st_ha_Res-Moyenne_sum_st_ha_res)/EcartType_sum_st_ha_res,
                  sum_st_ha_FeuStd=(sum_st_ha_Feu-Moyenne_sum_st_ha_feu)/EcartType_sum_st_ha_feu,
                  TMoyStd=(TMoy-Moyenne_tmoy)/EcartType_tmoy,VPDStd=(TotalVPD-Moyenne_vpd)/EcartType_vpd)
 )



          n<-nrow(Mort)


          listeEss<-c(rep("BOJ",n),rep("CHR",n),rep("EPB",n),rep("EPR",n),rep("ERR",n),rep("ERS",n),rep("FRN",n),rep("HEG",n),
                       rep("OSV",n),rep("PEB",n),rep("PEG",n),rep("PIB",n),rep("PRU",n),rep("THO",n),rep("TIL",n))
          listePente<-c(rep("B",n),rep("C",n),rep("D",n),rep("E",n), rep("F",n))

          Xmort<-matrix(0,ncol=178,nrow=n)

          # les 7 premieres colonnes sont pour l'effet âge pour 7 essences
          Xmort[,1:2]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("HEG","PEG")])*Coupe
          Xmort[,3:15]<-(Input$Ess_regroupe==listeEss[!listeEss %in%c("FRN","TIL")])*Coupe0
          Xmort[,16:20]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("BOJ","CHR","EPB","FRN","HEG")])*Input$DDStd
          Xmort[,21:24]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("BOJ","EPB","FRN","HEG")])*Input$DDStd*Input$DDStd
          Xmort[,25]<-(Input$Ess_regroupe=="HEG")*(Input$DDStd*Input$VPDStd)
          Xmort[,26]<-(Input$Ess_regroupe=="BOJ" & Depot %in% c("A","GLo","GMo","Lo","Mo"))*1
          Xmort[,27]<-(Input$Ess_regroupe=="EPR" & Depot %in% c("Tv","R"))*1
          Xmort[,28]<-(Input$Ess_regroupe=="ERR" & Depot=="RM")*1
          Xmort[,29]<-(Input$Ess_regroupe=="ERR" & Depot=="RS")*1
          Xmort[,30]<-(Input$Ess_regroupe=="ERR" & Depot %in% c("R","Tv","TE","C","MG"))*1
          Xmort[,31]<-(Input$Ess_regroupe=="FRN" & Depot=="RS")*1
          Xmort[,32:46]<-(Input$Ess_regroupe==listeEss)*Input$DHPcmStd
          Xmort[,47]<-(Input$Ess_regroupe=="PIB")*Input$DHPcmStd*Input$DHPcmStd
          Xmort[,48]<-(Input$Ess_regroupe=="BOJ")*Input$giniStd
          Xmort[,49:63]<-(Input$Ess_regroupe==listeEss)*1 # effet Intercept
          Xmort[,64:76]<-(Input$Ess_regroupe==listeEss[!listeEss %in%c("HEG","PIB")])*Input$logDHPcmStd
          Xmort[,77:78]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("ERS","THO")])*Input$logPTotStd
          Xmort[,79:81]<-(Input$Ess_regroupe=="EPB" & PenteCl==listePente[!listePente %in% c("B","E")])*1
          Xmort[,82:86]<-(Input$Ess_regroupe=="EPR" & PenteCl==listePente)*1
          Xmort[,87:89]<-(Input$Ess_regroupe=="PEG" & PenteCl==listePente[!listePente %in% c("E","F")])*1
          Xmort[,90:92]<-(Input$Ess_regroupe=="PRU" & PenteCl==listePente[!listePente %in% c("C","D")])*1
          Xmort[,93:96]<-(Input$Ess_regroupe=="TIL" & PenteCl==listePente[!listePente %in% c("B")])*1
          Xmort[,97:101]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("BOJ","ERR","FRN","PEB","PEG")])*Input$PTotStd
          Xmort[,102:103]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("ERR","PEB")])*Input$PTotStd*Input$PTotStd
          Xmort[,104]<-(Input$Ess_regroupe=="BOJ")*Input$PTotStd*Input$DDStd
          Xmort[,105:108]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("CHR","EPB","PIB","TIL")])*Input$PUtilStd
          Xmort[,109]<-(Input$Ess_regroupe=="EPB")*Input$PUtilStd*Input$PUtilStd
          Xmort[,110]<-(Input$Ess_regroupe=="PIB")*Input$PUtilStd*Input$TMoyStd
          Xmort[,111:114]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("ERR","HEG","PRU","THO")])*Input$mq_DHPcmStd
          Xmort[,115:118]<-(Input$Ess_regroupe==listeEss[listeEss %in% c("BOJ","EPB","ERS","HEG")])*Input$shannonStd
          Xmort[,119:131]<-(Input$Ess_regroupe==listeEss[!listeEss %in%c("HEG","OSV")])*Input$st_ha_cumul_gtStd
          Xmort[,132:136]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("ERS","FRN","PIB","THO","TIL")])*Input$sum_st_haStd
          Xmort[,137:146]<-(Input$Ess_regroupe==listeEss[!listeEss %in%c("ERS","FRN","PIB","THO","TIL")])*Input$sum_st_ha_FeuStd
          Xmort[,147:155]<-(Input$Ess_regroupe==listeEss[!listeEss %in%c("ERS","FRN","OSV","PIB","THO","TIL")])*Input$sum_st_ha_ResStd
          Xmort[,156:162]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("EPR","ERR","ERS","OSV","PEB","PIB","PRU")])*Input$TMoyStd
          Xmort[,163:168]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("EPR","ERR","ERS","OSV","PIB","PRU")])*Input$TMoyStd*Input$TMoyStd
          Xmort[,169:170]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("ERR","PEB")])*Input$TMoyStd*Input$PTotStd
          Xmort[,171:172]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("OSV","PRU")])*Input$TMoyStd*Input$VPDStd
          Xmort[,173:177]<-(Input$Ess_regroupe==listeEss[listeEss %in%c("EPR","HEG","OSV","PRU","THO")])*Input$VPDStd
          Xmort[,178]<-(Input$Ess_regroupe=="EPR")*Input$VPDStd*Input$TMoyStd


          # Matrice de parametres: il faut que les colonnes de Xmort soit dans le même ordre que celles de BetaMat
          BetaMat<-matrix(ParaMortCANEU$Estimate,ncol=1)

          # Calcul mortalite
          mort_pred <-as.matrix(Xmort) %*% as.matrix(BetaMat) + log(t)

          # Simulation des effets aléatoires

          fctRand<-function(Data){rnorm(1,mean=0,sd=Data)} # Fonction pour créer l'effet aléatoire
          ListeEss_regroupe<-unique(Input$Ess_regroupe)
          CovParmsCANEU<-CovParmsCANEU %>%
                         filter(Essence %in% ListeEss_regroupe) %>%
                         mutate(Variance=Variance^0.5*Correction)
          CovParmsCANEU<-CovParmsCANEU[rep(seq_len(nrow(CovParmsCANEU)), each = 1000), ]
          CovParmsCANEU$ranef<-sapply(CovParmsCANEU$Variance,fctRand)


            suppressWarnings(
             Output <- Input %>%
             select(origTreeID,Ess_regroupe) %>%
             mutate(pred_mort=mort_pred[,1]) %>%
             rename(Essence=Ess_regroupe) %>%
             left_join(CovParmsCANEU, by="Essence") %>%
             mutate(pred_mort=(1-exp(-exp(pred_mort+ranef)))) %>%  #le modele de mortalite est avec un lien cloglog
             group_by(origTreeID) %>%
             summarise(pred_mort=mean(pred_mort)))

          return (Output)

}
