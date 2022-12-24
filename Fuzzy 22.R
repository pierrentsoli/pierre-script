# Chargement des packages----

library(geodata) # Download Geographic Data
library(raster) # Geographic Data Analysis and Modeling
library(tmap) # Thematic maps
library(terra) # Spatial Data Analysis
library(dismo) # 
library(terrainr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(ggplot2)
library(legendMap)
library(ggpubr)
library(viridis)
library(leaflet)
library(tmaptools)
library(shiny)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(esquisse)

# Téléchargement des shapefiles du Cameroun----
myzipfile<-"https://biogeo.ucdavis.edu/data/diva/adm/CMR_adm.zip"
download.file(myzipfile,destfile = "counties.zip")

# Décomprésser le contenu du shapefile----
unzip("counties.zip")

# Consulter la liste des fichier de l'environnement de travail----
list.files()

# Importer le shapefile CMR_adm1.shp----
counties<-sf::read_sf("CMR_adm1.shp")
counties

# Importer le shapefile CMR_adm1.shp3----
counties3<-sf::read_sf("CMR_adm3.shp")
counties3


# Visualiser le shapefile----
View(counties)
counties$ID<-counties$NAME_1
counties<-counties[c(11,10)]

# Importatio du jeu de données Fuzzy----
library(readxl)
Fuzzy <- read_excel("Données excell/Fuzzyrisk.xls")
View(Fuzzy)
Fuzzy

# Attacher le jeu de données Fuzzy----
attach(Fuzzy)

# Jointure des jeu de données Fuzzy & Counties---- 
mapfuzzy<-inner_join(counties,Fuzzy,by=c("ID"="Regions"))
View(mapfuzzy)
class(mapfuzzy)

# Carte des risques Fuzzy----
ggplot(mapfuzzy)+
  geom_sf(aes(fill=mapfuzzy$Risque))

# Télécharger les données climatiques CMIP5----

getData('worldclim', var='bio', res=2.5)
climw<-getData('worldclim',var='bio',res=5)

# Télécharger les données climatiques futures CMIP6----

tavg50<-cmip6_world("CNRM-CM6-1", "585", "2041-2060", 
                    var="tavg", res=5, path=tempdir())

prec50<-cmip6_world("CNRM-CM6-1", "585", "2041-2060", 
                    var="prec", res=5, path=tempdir())

tavg70<-cmip6_world("CNRM-CM6-1", "585", "2061-2080", 
                    var="tavg", res=5, path=tempdir())

prec70<-cmip6_world("CNRM-CM6-1", "585", "2061-2080", 
                    var="prec", res=5, path=tempdir())


# Télécharger les données climatiques actuelles du Cameroun via Worldclim----

cam<-worldclim_country("CMR",var = "tavg",res = 2.5,path = tempdir())

camprec<-worldclim_country("CMR",var = "prec",res = 2.5,path = tempdir())

# Superposer exactement les rasters aux shapefiles du cameroun----
camer<-mask(cam,counties3)
camprecact<-mask(camprec,counties3)

# Vérification à l'aide de la fonction plot----
plot(camer)
plot(camprecact)
# Convertion du raster en dataframe/combinaison de vecteurs----

camer_frame<-as.data.frame(camer,xy=TRUE)%>%drop_na()
view(camer_frame)
camer_precact<-as.data.frame(camprecact,xy=TRUE)%>%drop_na()
view(camer_precact)

terra::as.data.frame()


class(camprec)
class(camer)


# Sauvegarde du dataframe en fichier xlsx----
writexl::write_xlsx(camer_frame,"camtavg")
writexl::write_xlsx(camer_precact,"camprec20")
write_csv(camer_precact,"camprec20")

# Carte avec tmap----
tm_shape(counties3)+
  tm_polygons()+
  tm_legend(position=c("left", "bottom"), bg.color="grey95", frame=TRUE)+
  tm_compass(type="rose", position=c("left", "top"), show.labels = 2)
  
class(World)
class(camer)

# Inset map----
cmr0<-getData("GADM", country="CMR", level=0)
cmr<-cmr0
Cen=(cmr[cmr$NAME_1=="Centre",])
munnames=coordinates(Cen)
munnames=data.frame(munnames)
munnames$label=Cen$NL_NAME_2
pol<-data.frame(xmin=10.2,xmax=13 ,ymin=3.8 ,ymax=6.5)
p1<-ggplot()+geom_polygon(data=Cen, aes(long+0.009,lat+0.009, group=group), fill="#9ecae1")+
  geom_polygon(data=Cen, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
  geom_text(data=munnames, aes(x=X1, y=X2,label=label), size=1.6, colour="grey20")+
  coord_equal()+theme_bw()+xlab("")+ylab("")+
  scale_x_continuous(breaks=seq(10,16, 1), labels=c(paste(seq(10,16, 1),"°E", sep="")))+
  scale_y_continuous(breaks=seq(3,7.1, 0.5), labels=c(paste(seq(3,7.1, 0.5),"°N", sep="")))+
  theme(axis.text.y =element_text(angle = 90, hjust=0.5))
p1
p2<-ggplot()+geom_polygon(data=cmr0, aes(long,lat,group=group),colour="grey10",fill="#fff7bc")+
  coord_equal()+theme_bw()+labs(x=NULL,y=NULL)+
  scale_x_continuous(breaks=seq(9,17, 2.5), labels=c(paste(seq(9,17, 2.5),"°E", sep="")))+
  scale_y_continuous(breaks=seq(1.9,14, 3), labels=c(paste(seq(1.9,14, 3),"°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank())
p2
png(file="centre.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.2, height = 0.2, x = 0.70, y = 0.23)


# Installation du package legenMap depuis github----
devtools::install_github("3wen/legendMap")

# Carte des tavg du mois de janvier (Cameroun)---- 

# Carte directe avec rose des vents et echelle
p=ggplot()+
  geom_raster(aes(x=camer_frame$x,y=camer_frame$y,fill=camer_frame$CMR_wc2.1_30s_tavg_1))+
  geom_sf(fill='transparent',data = counties3)+
  scale_fill_viridis_c(name='tavg',direction = -1)+
  labs(x='Longitude',y='Latitude',
       title = "Carte climatique du Cameroun",
       subtitle="temperatures moyennes du mois de janvier",
       caption = 'Source worldclim,2022')+
  scale_bar(lon = 10, lat = 12, 
            distance_lon = 100, distance_lat = 20, 
            distance_legend = 40, dist_unit = "km", 
            arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)
p


ggplot()+
  geom_raster(aes(x=camer_frame$x,y=camer_frame$y,fill=camer_frame$CMR_wc2.1_30s_tavg_3))+
  geom_sf(fill='transparent',data = counties3)+
  scale_fill_viridis_c(name='tavg',direction = 1,option = "magma")+
  labs(x='Longitude',y='Latitude',
       title = "Carte climatique du Cameroun",
       subtitle="temperatures moyennes du mois de janvier",
       caption = 'Source worldclim,2022')+
  theme_bw()+
  scale_bar(lon = 10, lat = 12, 
            distance_lon = 100, distance_lat = 20, 
            distance_legend = 40, dist_unit = "km", 
            arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)

breaks

# Carte des précipitations sous les conditions climatiques actuelles----
ggplot()+
  geom_raster(aes(x=camer_precact$x,y=camer_precact$y,fill=camer_precact$CMR_wc2.1_30s_prec_4))+
  geom_sf(fill='transparent',data = counties3)+
  scale_fill_viridis_c(name='mm/month',direction = -1,option = "magma")+
  labs(x='Longitude',y='Latitude',
       title = "Carte climatique du Cameroun",
       subtitle="temperatures moyennes du mois de janvier",
       caption = 'Source worldclim,2022')+
  scale_bar(lon = 10, lat = 12, 
            distance_lon = 100, distance_lat = 20, 
            distance_legend = 40, dist_unit = "km", 
            arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)


# Après la carte, ajout de l'echelle

p + scale_bar(lon = 10, lat = 12, 
                 distance_lon = 100, distance_lat = 20, 
                 distance_legend = 40, dist_unit = "km", 
                 arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)


# Creation d'une fonction pour générer les cartes en boucles----

ccactual <- list()

mapcam.tavg<-function(tavg){
  ggplot()+
    geom_raster(aes(x=camer_frame$x,y=camer_frame$y,fill=camer_frame$CMR_wc2.1_30s_tavg_1))+
    geom_sf(fill='transparent',data = counties3)+
    scale_fill_viridis_c(name='tavg',direction = -1)+
    labs(x='Longitude',y='Latitude',
         caption = 'Source worldclim,2022')+
    scale_bar(lon = 10, lat = 12, 
              distance_lon = 100, distance_lat = 20, 
              distance_legend = 40, dist_unit = "km", 
              arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)
}




  



