

SortieBillonage <- function(Data, Type ){

  # Data=fic; Type="DHP2015"

  select=dplyr::select

  # Petro a une équation pour CHR, attribuer CHR aux 3 autres especes de chenes
  # Si recrue de plus de 23 cm, elle n'a rien dans Espece, utiliser GrEspece (donc ça sera seulement ERS, ERR, BOJ, HEG qui passeront dans Petro, car les autres sont des groupes ou des ess pas de Petro)
  Data<- Data %>% filter(DHPcm >23) %>%
    mutate(Espece_original = Espece,
           Espece = ifelse(GrEspece %in% c("CHR","CHG","CHB","CHE","CHX"),"CHR", GrEspece),
           Espece = ifelse(is.na(Espece), Espece_original, Espece))

  # essences billonnage:         BOJ         ERR   ERS               HEG               CHR  BOP
  # essences samare:      "AUT" "BOJ" "EPX" "ERR" "ERS" "FEN" "FIN" "HEG" "RES" "SAB"

  data<- Data %>% filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR") ) # j'ai change CHX pour CHR car cette ligne ne fonctionnera jamais pour CHX, car le CHR est dans le Grespce FEN pour les recrues, et si on n'a l'espece original (pour les arbres qui sont là dès le départ, ça sera CHR, CHE, CHG ou CHB)

  if (nrow(data) == 0) {

    Data<- Data %>% mutate(erreur = "Code d'essence a l'exterieur de la plage de valeurs possibles pour billonage")

    return(Data)
  }

  data1 <- data %>% mutate(bilonID = seq_len(nrow(data)))


  billo <- Billonage::SIMBillonnageABCD_DHP(data1, Type)

  final <- left_join(data1, billo, by = "bilonID") %>%  mutate (Stm2ha=pi*(DHPcm/200)^2) %>%
    dplyr::select(-Espece) %>%
    rename(Espece = Espece_original) %>%
    arrange(PlacetteID,Annee,GrEspece)

  final <- final %>% select(-bilonID)


  return(final)

  }
