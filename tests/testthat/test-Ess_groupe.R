test_that("Test l'attribution des groupes d'essence à partir des essences", {



  GroupeTest<-Ess_groupe(Data=Donnees_Exemple,SpInd, ListeVp, SpGroups, Sp) %>%
              select(PlacetteID,origTreeID,GrEspece)

  expect_test_for_Artemis_Ess_groupe <- readRDS(test_path("fixtures", "expect_result_for_Ess_groupe.rds"))

  expect_equal(GroupeTest, expect_test_for_Artemis_Ess_groupe )
})
