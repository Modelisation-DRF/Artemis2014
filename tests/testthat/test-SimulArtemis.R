

test_that("La fonction simulateurArtemis(), Paramètres de recrutement ajustés, Coupe partielle réalisée depuis moins de 10 ans
          , Module d’accroissement Original et Module de mortalité Original et sans Données climatiques ", {

            set.seed(NULL)
            set.seed(3)



  Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3,Clim = NULL ,ClimAn = NULL ,AccModif='ORI',MortModif='ORI',RCP='RCP45') %>%
            arrange(PlacetteID,origTreeID,Annee) %>%
            select(-Cl_Drai)


  set.seed(NULL)

  expect_test_for_Artemis_AccModif_ORI_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_ORI_MortModif_ORI.rds"))%>%
                                                          arrange(PlacetteID,origTreeID,Annee)
  expect_test_for_Artemis_AccModif_ORI_MortModif_ORI <-expect_test_for_Artemis_AccModif_ORI_MortModif_ORI [,c("Veg_Pot","Annee","PlacetteID", "origTreeID","Espece", "Etat", "Nombre",
                                                                                                            "DHPcm","Type_Eco", "reg_eco","Altitude", "PTot", "TMoy",
                                                                                                              "hauteur_pred", "milieu", "sdom_bio", "GrEspece","vol_dm3")]


  expect_test_for_Artemis_AccModif_ORI_MortModif_ORI<-as.data.table(expect_test_for_Artemis_AccModif_ORI_MortModif_ORI)

  expect_equal(Result, expect_test_for_Artemis_AccModif_ORI_MortModif_ORI , tolerance = 1e-6)

})




test_that("La fonction simulateurArtemis(),  Coupe partielle réalisée depuis moins de 10 ans
          , Module d’accroissement BRT et Module de mortalité Original ", {

            set.seed(NULL)
            set.seed(3)


            Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3 ,Tendance=0 ,Residuel=0 ,AccModif='BRT',MortModif='ORI', EvolClim=0, ClimMois = ClimMois_Test ,ClimAn = ClimAn_Test)
            Result<-Result %>% select(-Cl_Drai)#Enlevé drainage car rajouté après

            set.seed(NULL)


            Result<-Result %>%
                    arrange(PlacetteID,origTreeID,Annee)

            expect_test_for_Artemis_AccModif_BRT_MortModif_ORI <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_BRT_MortModif_ORI.rds"))%>%
                                                                    arrange(PlacetteID,origTreeID,Annee)

            expect_test_for_Artemis_AccModif_BRT_MortModif_ORI <-expect_test_for_Artemis_AccModif_BRT_MortModif_ORI[,c("Veg_Pot","Annee","PlacetteID", "origTreeID","Espece", "Etat", "Nombre",
                                                                                                                       "DHPcm","Type_Eco", "reg_eco","Altitude", "PTot", "TMoy",
                                                                                                                       "hauteur_pred", "milieu", "sdom_bio", "GrEspece","vol_dm3")]

            expect_test_for_Artemis_AccModif_BRT_MortModif_ORI <-as.data.table(expect_test_for_Artemis_AccModif_BRT_MortModif_ORI)

            expect_equal(Result, expect_test_for_Artemis_AccModif_BRT_MortModif_ORI, tolerance = 1e-6)

})


test_that("La fonction simulateurArtemis(), Module d’accroissement GAM et Module de mortalité Quebec et Évolution du climat ", {

            set.seed(NULL)
            set.seed(3)


            Result <- simulateurArtemis(Data_ori = Intrant_Test ,Horizon = 3 ,Tendance=0 ,Residuel=0 ,AccModif='GAM',MortModif='QUE', EvolClim=1, ClimMois = ClimMois_Test ,ClimAn = ClimAn_Test)
            Result<-Result %>% select(-Cl_Drai)#Enlevé drainage car rajouté après

            set.seed(NULL)


            Result<-Result %>%
              arrange(PlacetteID,origTreeID,Annee)

            expect_test_for_Artemis_AccModif_GAM_MortModif_QUE <- readRDS(test_path("fixtures", "expect_test_for_Artemis_AccModif_GAM_MortModif_QUE.rds"))%>%
              arrange(PlacetteID,origTreeID,Annee)


            expect_equal(Result, expect_test_for_Artemis_AccModif_GAM_MortModif_QUE, tolerance = 1e-6)

          })

