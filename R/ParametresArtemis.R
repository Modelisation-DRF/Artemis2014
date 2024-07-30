


#' Praparation de parametres d'Artemis
#'Quand Artemis sera mis en package, tous ces fichiers devront être des fichiers rda
#'je mettrais aussi ListeCor, Effet.mort, Effet.acc, Effet.rec en fichiers rda.
#'On se fait un répertoire avec le code qui sert à transformer les fichiers en rda, comme ça, c'est facile si on doit le refaire


ParametresArtemis <- function() {

  # Praparation de parametres d'Artemis
  # Quand Artemis sera mis en package, tous ces fichiers devront être des fichiers rda
  # je mettrais aussi ListeCor, Effet.mort, Effet.acc, Effet.rec en fichiers rda.
   # On se fait un répertoire avec le code qui sert à transformer les fichiers en rda, comme ça, c'est facile si on doit le refaire


  # Fichier des vegetations potentielles


  Para <- Para %>% mutate(Effect = str_to_lower(Effect))

  ######################################################################################################
  ############################# Construction de vecteurs pour simulation ##############################
  ####################################################################################################


  # Merge des fichers des especes et des groupes d'especes, et ensuite merge avec la liste des vp,
  # pour obtenir la liste des Especes dans chaque groupe d'Especes de chaque VP
  ListeSpVp <- merge(SpGroups,Sp, by="SpeciesGroupID") %>%
    merge(ListeVp, by="VegPotID")


  # Somme des variances des effets aléatoires et residuelle par vp du module d'accroissement d'artemis pour la correction du biais du au log (sum(sigma2)/2)
  ListeCor <- CovParms %>%
    filter(CovParmEffet %in% c(1,3,4)) %>% # ne pas selectionner la correlation
    rename(Veg_Pot=Veg_pot) %>%
    group_by(Veg_Pot) %>%
    summarise(Cor = sum(ParameterEstimate)/2)

  # listes des effets du module de mortalite
  Effet.mort <- Para %>%
    filter(SubModuleID==1) %>%
    mutate(Effect = str_to_lower(Effect)) %>% # il y a des doublons d'effets a cause des minuscules/majuscule pour un meme nom
    select(Effect) %>%
    unique() %>%
    arrange(Effect)

  # listes des effets du module d'accroissement
  Effet.acc <- Para %>%
    filter(SubModuleID==2) %>%
    mutate(Effect = str_to_lower(Effect)) %>% # il y a des doublons d'effets a cause des minuscules/majuscule pour un meme nom
    select(Effect) %>%
    unique() %>%
    arrange(Effect)


  # listes des effets du module de recrutement
  Effet.rec <- Para %>%
    filter(SubModuleID %in% c(3,4,5)) %>%
    mutate(Effect = str_to_lower(Effect)) %>% # il y a des doublons d'effets a cause des minuscules/majuscule pour un meme nom
    filter(!Effect %in% c("dispersion","scale"))%>%
    select(Effect) %>%
    unique() %>%
    arrange(Effect)

  fic <- list(ListeVp, Para, Sp, SpGroups, SpInd, ListeSpVp, ListeCor, Effet.mort, Effet.acc, Effet.rec, CO2, Clim, ClimAn)
  return(fic)

}
