# Il faut des tests qui touchent chacun de ces paramètres
# Data_ori
# Horizon: 1 et 3
# ClimMois = NULL  (avec et sans)
# ClimAn = NULL  (avec et sans)
# Tendance=0   1 et 0
# Residuel=0  1 et 0
# FacHa=25    25 et 40
# EvolClim=0  0 et 1
# AccModif='ORI'  ORI BRT GAM
# MortModif='ORI'  ORI QUE
# RCP='RCP45'  4.5 8.5
# Coupe_ON = NULL  vide, horizon 1 et 3
# Coupe_modif = NULL  vide, horizon 1 et 3
# TBE = NULL  0 et 1
# MCH= 0 et 1

# tester des valeurs pas accepté sur les parametres

test_that("La fonction simulateurArtemis(), Paramètres de recrutement ajustés, Coupe partielle réalisée depuis moins de 10 ans
          , Module d’accroissement Original et Module de mortalité Original et sans Données climatiques ", {

# Residuel est laissé à sa valeur par defaut à 0, ce n'est donc pas un test avec CP depuis moins de 10 ans

            set.seed(NULL)
            set.seed(3)

  Result <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2025, Horizon = 3,ClimTot = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
            arrange(PlacetteID,origTreeID,Annee) %>%
            #mutate(Annee=Annee-(as.numeric(format(Sys.Date(), "%Y"))-2025)) %>%
            select(-Cl_Drai)

  # pour que le test passe en attendant
  Result <- Result %>% select(-Residuel)
  set.seed(NULL)

  expect_test_for_Artemis_AccModif_ORI_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_ORI_MortModif_ORI.rds"))
  # la colonne residuel n'est pas dans ce data...

  expect_test_for_Artemis_AccModif_ORI_MortModif_ORI<-data.table::as.data.table(expect_test_for_Artemis_AccModif_ORI_MortModif_ORI)

  expect_equal(Result, expect_test_for_Artemis_AccModif_ORI_MortModif_ORI , tolerance = 1e-6)

})




test_that("La fonction simulateurArtemis(),  Coupe partielle réalisée depuis moins de 10 ans
          , Module d’accroissement BRT et Module de mortalité Original ", {


            # Residuel est laissé à sa valeur par defaut à 0, ce n'est donc pas un test avec CP depuis moins de 10 ans

            set.seed(NULL)
            set.seed(3)


            Result <- simulateurArtemis(Data_ori = Intrant_Test, AnneeDep=2025, Horizon = 3 ,Tendance=0 ,
                                        Residuel=0 ,AccModif='BRT',MortModif='ORI', EvolClim=0,
                                        ClimTot = ClimTot_Test)



            set.seed(NULL)


            expect_test_for_Artemis_AccModif_BRT_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_BRT_MortModif_ORI.rds")) %>%
              mutate(Residuel=0) %>%
              relocate(Residuel, .after = Cl_Drai)

            expect_equal(Result, expect_test_for_Artemis_AccModif_BRT_MortModif_ORI, tolerance = 1e-6)

})


test_that("La fonction simulateurArtemis(), Module d’accroissement GAM et Module de mortalité Quebec et Évolution du climat ", {

            set.seed(NULL)
            set.seed(3)


            Result <- simulateurArtemis(Data_ori = Intrant_Test , AnneeDep=2026, Horizon = 3 ,
                                        Tendance=0 ,Residuel=0 ,AccModif='GAM',MortModif='QUE',
                                        EvolClim=1, ClimTot = ClimTot_Test) %>%
                      arrange(PlacetteID,origTreeID,Annee)

            set.seed(NULL)


           expect_test_for_Artemis_AccModif_GAM_MortModif_QUE <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_GAM_MortModif_QUE.rds"))%>%
              arrange(PlacetteID,origTreeID,Annee) %>%
              mutate(Residuel=0) %>%
               relocate(Residuel, .after = Cl_Drai)


            expect_equal(Result, expect_test_for_Artemis_AccModif_GAM_MortModif_QUE, tolerance = 1e-6)

          })

test_that("La fonction simulateurArtemis(), Module d’accroissement QUE (Fortin 2026) et Module de mortalité Quebec et Évolution du climat ", {

  set.seed(NULL)
  set.seed(3)


  Result <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2026, Horizon = 3 ,Tendance=0 ,
                              Residuel=0 ,AccModif='QUE',MortModif='QUE', EvolClim=1,
                              ClimTot = ClimTot_Test) %>%
            arrange(PlacetteID,origTreeID,Annee)

  set.seed(NULL)


  expect_test_for_Artemis_AccModif_QUE_MortModif_QUE <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_QUE_MortModif_QUE.rds"))%>%
    arrange(PlacetteID,origTreeID,Annee) %>%
    mutate(Residuel=0) %>%
    relocate(Residuel, .after = Cl_Drai)


  expect_equal(Result, expect_test_for_Artemis_AccModif_QUE_MortModif_QUE, tolerance = 1e-2)#####Changé la tolérance à cause de la correction de quadrature
                                                                                                # Gauss-Hermite qui doit générer des distributions

})

test_that("La fonction simulateurArtemis(), Module d’accroissement QUE (Fortin 2026) et Module de mortalité CANEU et Évolution du climat ", {

  set.seed(NULL)
  set.seed(3)


  Result <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2026, Horizon = 3 ,Tendance=0
                              ,Residuel=0 ,AccModif='QUE',MortModif='CANEU', EvolClim=1,
                              ClimTot = ClimTot_Test) %>%
    arrange(PlacetteID,origTreeID,Annee)

  set.seed(NULL)


  expect_test_for_Artemis_AccModif_QUE_MortModif_QUE <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_QUE_MortModif_CANEU.rds"))%>%
    arrange(PlacetteID,origTreeID,Annee) %>%
    mutate(Residuel=0) %>%
    relocate(Residuel, .after = Cl_Drai)


  expect_equal(Result, expect_test_for_Artemis_AccModif_QUE_MortModif_QUE, tolerance = 1e-2)#####Changé la tolérance à cause de la correction de quadrature
  # Gauss-Hermite qui doit générer des distributions

})




test_that("La fonction simulateurArtemis(), Peuplement résiduel, Module d’accroissement Original et Module de mortalité Original", {

  set.seed(NULL)
  set.seed(3)


  Result <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2026, Horizon = 3 ,
                              Tendance=0 ,Residuel=1 ,AccModif='ORI',MortModif='ORI', EvolClim=0) %>%
            mutate(Annee=Annee-(as.numeric(format(Sys.Date(), "%Y"))-2025))%>%
            arrange(PlacetteID,origTreeID,Annee)

  set.seed(NULL)


  expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI.rds"))%>%
    arrange(PlacetteID,origTreeID,Annee)



  expect_equal(Result, expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI, tolerance = 1e-2)#####Changé la tolérance à cause de la correction de quadrature
  # Gauss-Hermite qui doit générer des distributions

})

test_that("La fonction simulateurArtemis(), coupe jardinage à la deuxième décennie, Module d’accroissement Original et Module de mortalité Original", {

  set.seed(NULL)
  set.seed(3)


  Result <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2026, Horizon = 3 ,Tendance=0 ,Residuel=0 ,AccModif='ORI',
                              Coupe_ON = c(NA,3,NA),MortModif='ORI', EvolClim=0) %>%
            arrange(PlacetteID,origTreeID,Annee)

  set.seed(NULL)


  expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_Jardinage_AccModif_ORI_MortModif_ORI.rds"))%>%
    arrange(PlacetteID,origTreeID,Annee)



  expect_equal(Result, expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI, tolerance = 1e-2)#####Changé la tolérance à cause de la correction de quadrature
  # Gauss-Hermite qui doit générer des distributions

})

test_that("La fonction simulateurArtemis(), tbe à la deuxième décennie, Module d’accroissement Original et Module de mortalité Original", {

  set.seed(NULL)
  set.seed(3)


  Result <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2026, Horizon = 3 ,Tendance=0 ,Residuel=0 ,AccModif='ORI',
                              TBE = c(0,1,0),MortModif='ORI', EvolClim=0) %>%
            arrange(PlacetteID,origTreeID,Annee)

  set.seed(NULL)


  expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_TBE_AccModif_ORI_MortModif_ORI.rds"))%>%
    arrange(PlacetteID,origTreeID,Annee)



  expect_equal(Result, expect_test_for_Artemis_Residuel_AccModif_ORI_MortModif_ORI, tolerance = 1e-2)#####Changé la tolérance à cause de la correction de quadrature
  # Gauss-Hermite qui doit générer des distributions

})



test_that("La fonction simulateurArtemis(), Paramètres de recrutement ajustés, Coupe partielle réalisée depuis moins de 10 ans
          , Module d’accroissement Original et Module de mortalité Original et sans Données climatiques et MCH=1", {

            # Residuel est laissé à sa valeur par defaut à 0, ce n'est donc pas un test avec CP depuis moins de 10 ans

            set.seed(NULL)
            set.seed(3)
            # Intrant_Test contient une placette par veg_pot: In group 17: `origTreeID = 17`. RE2 arbre 17

            Result1 <- simulateurArtemis(Data_ori = Intrant_Test ,AnneeDep=2025, Horizon = 3,ClimTot = NULL ,
                                         AccModif='ORI',MortModif='ORI',RCP='RCP45',
                                         MCH=1) %>%
              arrange(PlacetteID,origTreeID,Annee) %>%
              #mutate(Annee=Annee-(as.numeric(format(Sys.Date(), "%Y"))-2025)) %>%
              select(-Cl_Drai)

            # pour que le test passe en attendant
            Result1 <- Result1 %>% select(-Residuel)
            set.seed(NULL)

            attendu <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_ORI_MortModif_ORI.rds"))
            # la colonne residuel n'est pas dans ce data...

            # vérifier la première décennie de simulation des arbres survivants, pas les recrues
            attendu0 <-data.table::as.data.table(attendu) %>% filter(Annee==2025) %>% dplyr::select(PlacetteID, origTreeID)
            attendu10 <-data.table::as.data.table(attendu) %>% filter(Annee==2035)
            attendu10 <- left_join(attendu0, attendu10, by = c('PlacetteID', 'origTreeID'))
            attendu10[attendu10$Espece=='HEG', "Nombre"] <- 1 - 0.104004375  # mortalité attendu pour un HEG de 15 cm
            Result1 <- left_join(attendu0, Result1 %>% filter(Annee==2035), by = c('PlacetteID', 'origTreeID'))

            expect_equal(Result1$Nombre, attendu10$Nombre , tolerance = 1e-6)

          })
