

#'
#'@param Mort
#'
#'@param ClimatQUE
#'
#'@param Models
#'
#'@param DrainageCl
#'
#'@param PenteCl
#'
#'@param Texture
#'
#'@param Coupe
#'
#'@param Coupe0
#'
#'@param sum_st_ha
#'
#'@param t
#'
#' @return
#'
#' @examples
#' result <- mortQUE(Mort, ClimatQUE, Models, DrainageCl, PenteCl, Texture, Coupe, Coupe0, sum_st_ha, t)
#'
#' print(result)
#' @export
#'
mortQUE<-function(Mort, ClimatQUE, Models, DrainageCl, PenteCl, Texture, Coupe, Coupe0, sum_st_ha, t){

       EssGrQUE<-Models[[9]]      #0.0003milis
       ParaMortQUE<-Models[[10]] %>% mutate(Effect = str_to_lower(Effect)) %>% arrange(Effect,Essence)  #1.8milis
       CovParmsQUE<-Models[[11]]   #0.0003milis

 suppressMessages(
          Input<-Mort %>%
            ungroup %>%
            left_join(EssGrQUE, by="GrEspece") %>% # le fichier ne contient pas les EPX, ni EPB/EPN
            mutate(Ess_regroupe=ifelse(Espece %in% c("EPN","EPB"), Espece, Ess_regroupe)) %>% # on met EPB/EPN pour les manquants
            mutate(Ess_regroupe=replace(Ess_regroupe,Espece=="EPR", "EPN")) %>%
            left_join(ClimatQUE))    #34 milis

          n<-nrow(Mort)

          listeEssA<-c(rep("BOP",n),rep("EPB",n),rep("EPN",n),rep("ERR",n),rep("PEU",n),rep("PIG",n),rep("SAB",n)) #0.01 milis
          listeEssB<-c(rep("BOP",n),rep("ERR",n),rep("SAB",n))
          listeEssC<-c(rep("BOJ",n),rep("BOP",n),rep("EPB",n),rep("EPN",n),rep("ERR",n),rep("PEU",n),rep("PIG",n),rep("SAB",n))
          listeEssD<-c(rep("BOP",n),rep("EPN",n),rep("SAB",n))
          listeEssE<-c(rep("BOJ",n),rep("BOP",n),rep("EPB",n),rep("ERR",n),rep("PEU",n),rep("PIG",n),rep("SAB",n))
          listePente<-c(rep("B",n),rep("C",n),rep("D",n),rep("EF",n))

          Xmort<-matrix(0,ncol=73,nrow=n)   #0.005 milis

          # les 7 premieres colonnes sont pour l'effet âge pour 7 essences
          Xmort[,1:7]<-(Input$Ess_regroupe==listeEssA)*Input$Age_moy # 0.01milis, ça met l'âge de la placette à certaines des lignes des 7 premieres colonnes
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
          BetaMat<-matrix(ParaMortQUE$Estimate,ncol=1) #0.005 milis

          # Calcul mortalite
          mort_pred <-Xmort %*% BetaMat + log(t) #0.02 milis  #IA: j'ai changé le nom du data mort par mort_pred, car mort c'est le nom de la fct de mortalité originale d'Artémis

          # Simulation des effets aléatoires

          fctRand<-function(Data){rnorm(1,mean=0,sd=Data^0.5)} # Fonction pour créer l'effet aléatoire
          ListeEss_regroupe<-unique(Input$Ess_regroupe)
          CovParmsQUE<-CovParmsQUE %>% filter(Essence %in% ListeEss_regroupe)
          CovParmsQUE<-CovParmsQUE[rep(seq_len(nrow(CovParmsQUE)), each = 500), ]
          CovParmsQUE$ranef<-sapply(CovParmsQUE$Variance,fctRand)

          # IA: sans le random
         # Output <- Input %>%
          #  select(origTreeID) %>%
           # mutate(pred_mort=mort_pred[,1])


         # Calcul des prévisions 170 milisecondes le merge est l'étape la plus longue diminuée de moitiée avec 500 rep peu d'impact sur preds
            suppressWarnings(
             Output <- Input %>%
             select(origTreeID,Ess_regroupe) %>%
             mutate(pred_mort=mort_pred[,1]) %>%
             rename(Essence=Ess_regroupe) %>%
             left_join(CovParmsQUE, by="Essence") %>% # IA: left_join va plus vite que merge
             mutate(pred_mort=(1-exp(-exp(pred_mort+ranef)))) %>%  #le modele de mortalite est avec un lien cloglog
             group_by(origTreeID) %>%
             summarise(pred_mort=median(pred_mort)))

          return (Output)

}
