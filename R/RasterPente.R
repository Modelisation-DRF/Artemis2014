#############################Fonction pour donner l'orientation et la pente lorsque inconnu#################
############################Basé sur le modèle d'élévation numérique de terrain du Canada à une résolution de 200m#####
# test<-raster
#
# # #######À partir du modèle d'élévation du terrain crée un rasterbrick (2 layers) de pente et d'orientation
# Result<-terrain(test,opt=c("slope","aspect"),unit="degrees")
# #
# # #####Fichier de coordonnées pour extraire l'info du rasterbrick
#  Coord<-data.frame("Lat"=c(48.5,48.5),"Long"=c(-73,-73))
# #
# # ######Transfo du data frame en fichier spatial
#  CoordSp<-Coord
# coordinates(CoordSp)<-~Long+Lat
# #
# # ######Extraction des valeurs sur le rasterbric de pente et de terrain
# SlopeAspect<-extract(Result,CoordSp)

# ############Merge raster 032
# F032A<-raster("Data/GeoTif/Originaux/F032/032A.tif")
# F032B<-raster("Data/GeoTif/Originaux/F032/032B.tif")
# F032C<-raster("Data/GeoTif/Originaux/F032/032C.tif")
# F032D<-raster("Data/GeoTif/Originaux/F032/032D.tif")
# F032E<-raster("Data/GeoTif/Originaux/F032/032E.tif")
# F032F<-raster("Data/GeoTif/Originaux/F032/032F.tif")
# F032G<-raster("Data/GeoTif/Originaux/F032/032G.tif")
# F032H<-raster("Data/GeoTif/Originaux/F032/032H.tif")
# F032I<-raster("Data/GeoTif/Originaux/F032/032I.tif")
# F032J<-raster("Data/GeoTif/Originaux/F032/032J.tif")
# F032K<-raster("Data/GeoTif/Originaux/F032/032K.tif")
# F032L<-raster("Data/GeoTif/Originaux/F032/032L.tif")
# F032M<-raster("Data/GeoTif/Originaux/F032/032M.tif")
# F032N<-raster("Data/GeoTif/Originaux/F032/032N.tif")
# F032O<-raster("Data/GeoTif/Originaux/F032/032O.tif")
# F032P<-raster("Data/GeoTif/Originaux/F032/032P.tif")
# F032<-merge(F032A,F032B,F032C,F032D,F032E,F032F,F032G,F032H,F032I,F032J,F032K,F032L,F032M,F032N,F032O,F032P)
# writeRaster(F032,"Data/GeoTif/F032.tif",format="GTiff",overwrite=TRUE)
# #
# # ############Merge raster 022
# F022A<-raster("Data/GeoTif/Originaux/F022/022A.tif")
# F022B<-raster("Data/GeoTif/Originaux/F022/022B.tif")
# F022C<-raster("Data/GeoTif/Originaux/F022/022C.tif")
# F022D<-raster("Data/GeoTif/Originaux/F022/022D.tif")
# F022E<-raster("Data/GeoTif/Originaux/F022/022E.tif")
# F022F<-raster("Data/GeoTif/Originaux/F022/022F.tif")
# F022G<-raster("Data/GeoTif/Originaux/F022/022G.tif")
# F022H<-raster("Data/GeoTif/Originaux/F022/022H.tif")
# F022I<-raster("Data/GeoTif/Originaux/F022/022I.tif")
# F022J<-raster("Data/GeoTif/Originaux/F022/022J.tif")
# F022K<-raster("Data/GeoTif/Originaux/F022/022K.tif")
# F022L<-raster("Data/GeoTif/Originaux/F022/022L.tif")
# F022M<-raster("Data/GeoTif/Originaux/F022/022M.tif")
# F022N<-raster("Data/GeoTif/Originaux/F022/022N.tif")
# F022O<-raster("Data/GeoTif/Originaux/F022/022O.tif")
# F022P<-raster("Data/GeoTif/Originaux/F022/022P.tif")
# F022<-merge(F022A,F022B,F022C,F022D,F022E,F022F,F022G,F022H,F022I,F022J,F022K,F022L,F022M,F022N,F022O,F022P)
# # writeRaster(F022,"Data/GeoTif/F022.tif",format="GTiff",overwrite=TRUE)
#
# # ############Merge raster 031
# F031I<-raster("Data/GeoTif/Originaux/F031/031I.tif")
# F031J<-raster("Data/GeoTif/Originaux/F031/031J.tif")
# F031K<-raster("Data/GeoTif/Originaux/F031/031K.tif")
# F031L<-raster("Data/GeoTif/Originaux/F031/031L.tif")
# F031M<-raster("Data/GeoTif/Originaux/F031/031M.tif")
# F031N<-raster("Data/GeoTif/Originaux/F031/031N.tif")
# F031O<-raster("Data/GeoTif/Originaux/F031/031O.tif")
# F031P<-raster("Data/GeoTif/Originaux/F031/031P.tif")
#
# F031<-merge(F031I,F031J,F031K,F031L,F031M,F031N,F031O,F031P)
# writeRaster(F031,"Data/GeoTif/F031.tif",format="GTiff",overwrite=TRUE)
#
# # ############Merge raster 021
# F021E<-raster("Data/GeoTif/Originaux/F021/021E.tif")
# F021L<-raster("Data/GeoTif/Originaux/F021/021L.tif")
# F021M<-raster("Data/GeoTif/Originaux/F021/021M.tif")
# F021N<-raster("Data/GeoTif/Originaux/F021/021N.tif")
#
#
# F021<-merge(F021E,F021L,F021M,F021N)
# writeRaster(F021,"Data/GeoTif/F021.tif",format="GTiff",overwrite=TRUE)
#
# # ############Merge raster 012
# F012J<-raster("Data/GeoTif/Originaux/F012/012J.tif")
# F012K<-raster("Data/GeoTif/Originaux/F012/012K.tif")
# F012L<-raster("Data/GeoTif/Originaux/F012/012L.tif")
# F012M<-raster("Data/GeoTif/Originaux/F012/012M.tif")
# F012N<-raster("Data/GeoTif/Originaux/F012/012N.tif")
# F012O<-raster("Data/GeoTif/Originaux/F012/012O.tif")
#
#
# F012<-merge(F012J,F012K,F012L,F012M,F012N,F012O)
# writeRaster(F012,"Data/GeoTif/F012.tif",format="GTiff",overwrite=TRUE)
#
# RasterTot<-merge(F012,F021,F022,F031,F032)
# writeRaster(RasterTot,"Data/GeoTif/DEMTot.tif",format="GTiff",overwrite=TRUE)



#'
#'@param Vide
#'
#'@return
#'
#'@examples
#' result <- PentesAsp(Vide)
#'
#' print(result)
#'@export
#'

PentesAsp<-function (Vide){

DEM<-raster("Data/GeoTif/DEMTot.tif")
DEMPentesAsp<-terrain(DEM,opt=c("slope","aspect"),unit="degrees")

Donnees<-Vide %>%
        group_by(PlacetteID) %>%
        summarise(Latitude=first(Latitude),Longitude=first(Longitude))
CoordSp<-Donnees
coordinates(CoordSp)<-~Longitude+Latitude

PentesAsp<-raster::extract(DEMPentesAsp,CoordSp)

Donnees$PenteRaster<-tan(PentesAsp[,1]*pi/180)*100
Donnees$Exposition_Raster<-PentesAsp[,2]

suppressMessages(
Vide<-Vide %>%
      left_join(Donnees) %>%
      mutate(Pente=ifelse(is.na(Pente)==TRUE,PenteRaster,Pente),
             Exposition=ifelse(is.na(Exposition)==TRUE,Exposition_Raster,Exposition)) %>%
  select(-PenteRaster,-Exposition_Raster)
)

#rm(DEM,DEMPentesAsp, Donnees)
return(Vide)

}



