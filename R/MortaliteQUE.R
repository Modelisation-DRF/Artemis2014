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
mortQUE<-function(Mort, ClimatQUE, Models, DrainageCl, PenteCl, Texture, Coupe, Coupe0, sum_st_ha, t){

       EssGrQUE<-Models[[9]]
       ParaMortQUE<-Models[[10]] %>% mutate(Effect = str_to_lower(Effect)) %>% arrange(Effect,Essence)  #1.8milis
       CovParmsQUE<-Models[[11]]

 suppressMessages(
          Input<-Mort %>%
            ungroup %>%
            left_join(EssGrQUE, by="GrEspece") %>% # le fichier ne contient pas les EPX, ni EPB/EPN
            mutate(Ess_regroupe=ifelse(Espece %in% c("EPN","EPB"), Espece, Ess_regroupe)) %>% # on met EPB/EPN pour les manquants
            mutate(Ess_regroupe=replace(Ess_regroupe,Espece=="EPR", "EPN")) %>%
            left_join(ClimatQUE))

          n<-nrow(Mort)

          listeEssA<-c(rep("BOP",n),rep("EPB",n),rep("EPN",n),rep("ERR",n),rep("PEU",n),rep("PIG",n),rep("SAB",n)) #0.01 milis
          listeEssB<-c(rep("BOP",n),rep("ERR",n),rep("SAB",n))
          listeEssC<-c(rep("BOJ",n),rep("BOP",n),rep("EPB",n),rep("EPN",n),rep("ERR",n),rep("PEU",n),rep("PIG",n),rep("SAB",n))
          listeEssD<-c(rep("BOP",n),rep("EPN",n),rep("SAB",n))
          listeEssE<-c(rep("BOJ",n),rep("BOP",n),rep("EPB",n),rep("ERR",n),rep("PEU",n),rep("PIG",n),rep("SAB",n))
          listePente<-c(rep("B",n),rep("C",n),rep("D",n),rep("EF",n))

          Xmort<-matrix(0,ncol=73,nrow=n)

          # les 7 premieres colonnes sont pour l'effet âge pour 7 essences
          Xmort[,1:7]<-(Input$Ess_regroupe==listeEssA)*Input$Age_moy
          Xmort[,8]<-(Input$Ess_regroupe=="ERR")*Input$cec_015cm
          Xmort[,9]<-(Input$Ess_regroupe=="SAB")*Input$cec_015cm
          #Xmort[,10]<-(Input$Ess_regroupe=="BOJ")*Input$CMI #revue modèle pour être conforme à la publication 3 oct 2023
          Xmort[,10:12]<-(Input$Ess_regroupe==listeEssB)*Coupe # effet coupe dans 3 essences
          Xmort[,13:20]<-(Input$Ess_regroupe==listeEssC)*Coupe0 # effet coupe0 dans 8 essences
          Xmort[,21]<-(Input$Ess_regroupe=="SAB")*Input$DD
          Xmort[,22:29]<-(Input$Ess_regroupe==listeEssC)*Input$DHPcm # effet DHPcm dans 8 essences
          Xmort[,30:32]<-(Input$Ess_regroupe==listeEssD & DrainageCl=="HydXer")*1 # effet DrainageCl dans 3 essences
          Xmort[,33:40]<-(Input$Ess_regroupe==listeEssC)*1
          Xmort[,41]<-(Input$Ess_regroupe=="EPN")*log(Input$Aridity+1)
          Xmort[,42]<-(Input$Ess_regroupe=="SAB")*log(Input$Aridity+1)
          Xmort[,43]<-(Input$Ess_regroupe=="BOJ")*log(Input$DD)
          Xmort[,44:50]<-(Input$Ess_regroupe==listeEssE)*log(Input$DHPcm)
          Xmort[,51]<-(Input$Ess_regroupe=="PIG")*log(Input$PTotPeriode)
          Xmort[,52]<-(Input$Ess_regroupe=="EPN")*log(Input$TMoyPeriode+5)
          Xmort[,53]<-(Input$Ess_regroupe=="PEU")*log(Input$TSummer)
          Xmort[,54:57]<-(Input$Ess_regroupe=="PIG" & PenteCl==listePente)*1
          Xmort[,58:61]<-(Input$Ess_regroupe=="SAB" & PenteCl==listePente)*1
          Xmort[,62]<-(Input$Ess_regroupe=="EPN")*Input$sand_015cm
          Xmort[,63:69]<-(Input$Ess_regroupe==listeEssA)*Input$st_ha_cumul_gt
          Xmort[,70]<-(Input$Ess_regroupe=="ERR")*sum_st_ha
          Xmort[,71]<-(Input$Ess_regroupe=="PIG" & Texture=="FinGros")*1
          Xmort[,72]<-(Input$Ess_regroupe=="BOP")*Input$TmaxUtil
          Xmort[,73]<-(Input$Ess_regroupe=="EPB")*Input$TMoyPeriode

          # Matrice de parametres: il faut que les colonnes de Xmort soit dans le même ordre que celles de BetaMat
          BetaMat<-matrix(ParaMortQUE$Estimate,ncol=1)

          # Calcul mortalite
          mort_pred <-Xmort %*% BetaMat + log(t)

          # Simulation des effets aléatoires

          fctRand<-function(Data){rnorm(1,mean=0,sd=Data^0.5)} # Fonction pour créer l'effet aléatoire
          ListeEss_regroupe<-unique(Input$Ess_regroupe)
          CovParmsQUE<-CovParmsQUE %>% filter(Essence %in% ListeEss_regroupe)
          CovParmsQUE<-CovParmsQUE[rep(seq_len(nrow(CovParmsQUE)), each = 500), ]
          CovParmsQUE$ranef<-sapply(CovParmsQUE$Variance,fctRand)


            suppressWarnings(
             Output <- Input %>%
             select(origTreeID,Ess_regroupe) %>%
             mutate(pred_mort=mort_pred[,1]) %>%
             rename(Essence=Ess_regroupe) %>%
             left_join(CovParmsQUE, by="Essence") %>%
             mutate(pred_mort=(1-exp(-exp(pred_mort+ranef)))) %>%  #le modele de mortalite est avec un lien cloglog
             group_by(origTreeID) %>%
             summarise(pred_mort=median(pred_mort)))

          return (Output)

}
