GenereClimat<-function (Data_Ori,AnneeDep, Horizon, RCP){


  Placettes<-Data_Ori %>%
             group_by(PlacetteID) %>%
             summarise()

  Annee<-data.frame("Annee"=seq(from=2020, to=AnneeDep+10*Horizon, by=1))

  ClimAn<-data.frame("rcp"=RCP,"FFP"=92, "PTot"=800, "TMoy"=3, "Tmax_yr"=9, "Aridity"=57,"DD"=1350,"CMI"=0)

  ClimMois=data.frame("rcp"=rep(RCP,12), Mois=seq(from=1, to=12, by=1), Tmin=c(-15,-14,-10,-1,5,10,15,15,10,5,-3,-7),
                      "Tmax"=c(-5,-5,0,8,15,20,26,25,20,12,5,0),"PTot"=c(82,65,67,74,100,94,115,99,99,100,100,95))

  ClimAn<-Placettes %>%
          merge(ClimAn) %>%
          merge(Annee) %>%
          arrange(PlacetteID, Annee)

  ClimMois<-Placettes %>%
          merge(ClimMois) %>%
          merge(Annee) %>%
          arrange(PlacetteID, Annee)

  ClimTot<-list(ClimAn,ClimMois)

  return(ClimTot)

}
