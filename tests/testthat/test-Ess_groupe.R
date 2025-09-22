test_that("Test l'attribution des groupes d'essence à partir des essences", {


  df <- Donnees_Exemple

  GroupeTest<-Ess_groupe(Data=df,SpInd, ListeVp, SpGroups, Sp) %>%
              select(PlacetteID,origTreeID,GrEspece)

  expect_test_for_Artemis_Ess_groupe <- readRDS(test_path("fixtures", "expect_result_for_Ess_groupe.rds"))

  expect_true(isTRUE(all.equal(GroupeTest, expect_test_for_Artemis_Ess_groupe, check.attributes = FALSE)))

  #expect_equal(GroupeTest, expect_test_for_Artemis_Ess_groupe ) # ça ne fonctionne plus, à cause des attributs différents
})
