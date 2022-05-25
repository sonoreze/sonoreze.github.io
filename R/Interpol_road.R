tags_table <- as.data.frame(noisecapture_data[,c("Id","Date","x","y","leq_mean","tags","accuracy")]) %>%
  arrange(Id,Date) %>%
  filter(tags != "") %>%
  filter(x != "NA") %>%
  filter(accuracy<20) %>%
  group_by(Id) %>%
  arrange(Date) %>%
  mutate(IdTraceReset=cumsum(c(TRUE, as.integer(diff(as.POSIXct(Date)), units = "secs") >= 2L))) %>%
  ungroup() %>%
  mutate(IdGlobal=str_c(Id,IdTraceReset)) %>%
  arrange(IdGlobal) %>%
  group_by(Id) %>%
  arrange(IdTraceReset) %>%
  group_by(IdGlobal) %>%
  mutate(IdTrace=cur_group_id()) %>%
  ungroup() %>%
  select(-c(accuracy,IdGlobal,IdTraceReset)) %>%
  st_as_sf(coords = c("x","y"),remove = F) %>%
  st_set_crs(4326) %>%
  group_by(IdTrace) %>%
  mutate(
    lead = geometry[row_number() + 1],
    Dist = st_distance(geometry, lead, by_element = T),
  ) %>%
  mutate(DistTot = sum(as.numeric(Dist),na.rm=TRUE)) %>%
  ungroup() %>%
  select(-c(lead)) %>%
  filter(DistTot<500)


tags_table_taggroup <- tags_table %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  transform(tags = strsplit(as.character(tags),",")) %>%
  unnest(tags) %>%
  mutate(TagGroup = case_when(tags == "indoor" | tags == "test" ~ "Condition",
                              tags == "wind" | tags == "rain" ~ "Meteo",
                              tags == "marine_traffic" | tags == "air_traffic" | tags == "rail" | tags == "road" ~ "Trafic",
                              TRUE ~ "Ambiance"))


Reze_sf_4326 <- st_as_sf(tags_table, coords = c("x", "y"), crs = 4326, agr = "constant", remove = F)
st_crs(Reze_sf_4326) <- 4326 #EPSG WGS84
Reze_sf_2154 <- st_transform(Reze_sf_4326, 2154) #LAMBERT 93
# on créé 4 fichiers points_road_reze_2154 d'extension dbf, prj, shp et shx
# dans /geo/RoadReze
st_write(Reze_sf_2154, here("geo","RoadReze","points_road_reze_2154.shp"), append=FALSE)

#read layer shapefile
Pts_Reze_2154 <- st_read(here("geo","RoadReze","points_road_reze_2154.shp"))

#########
## PIERRE : pourquoi la variable leq_mean s'appelle maintenant leq_men ?
## J'ai trouvé : le package st (ou gdal) tronque les noms de chanmps
# supérieurs à 7 caractères
########

# Pts_Reze_2154 <- Pts_Reze_2154 %>%
#   rename(leq_mean = leq_men)

# add Time correction
Pts_Reze_2154 <- Pts_Reze_2154 %>%
  mutate(DateJour=as.Date(Date)) %>%
  mutate(DateHeure=hour(as.POSIXct(Date))) %>%
  mutate(JourJ=wday(as.POSIXct(DateJour), label = TRUE, abbr=FALSE)) %>%
  mutate(JourSem = case_when(JourJ == "Samedi" ~ "S",
                             JourJ == "Dimanche" ~ "D",
                             TRUE ~ "LV")) %>%
  mutate(Correctif = case_when(JourSem == "LV" &&  DateHeure == 0 ~ 3.82629871477906,
                               JourSem == "LV" &&  DateHeure == 1 ~ 6.15994336459467,
                               JourSem == "LV" &&  DateHeure == 2 ~ 7.95864096513497,
                               JourSem == "LV" &&  DateHeure == 3 ~ 8.83512319743405,
                               JourSem == "LV" &&  DateHeure == 4 ~ 7.99185994519303,
                               JourSem == "LV" &&  DateHeure == 5 ~ 5.16928070062316,
                               JourSem == "LV" &&  DateHeure == 6 ~ 1.04663871023969,
                               JourSem == "LV" &&  DateHeure == 7 ~ -2.14561788976771,
                               JourSem == "LV" &&  DateHeure == 8 ~ -4.6464965444681,
                               JourSem == "LV" &&  DateHeure == 9 ~ -4.54152190004787,
                               JourSem == "LV" &&  DateHeure == 10 ~ -4.28640014209438,
                               JourSem == "LV" &&  DateHeure == 11 ~ -3.84647144855295,
                               JourSem == "LV" &&  DateHeure == 12 ~ -3.18634609926391,
                               JourSem == "LV" &&  DateHeure == 13 ~ -3.4288556614073,
                               JourSem == "LV" &&  DateHeure == 14 ~ -3.26224274719903,
                               JourSem == "LV" &&  DateHeure == 15 ~ -3.39232475189917,
                               JourSem == "LV" &&  DateHeure == 16 ~ -3.53448672074911,
                               JourSem == "LV" &&  DateHeure == 17 ~ -3.62662202332034,
                               JourSem == "LV" &&  DateHeure == 18 ~ -3.57040447264549,
                               JourSem == "LV" &&  DateHeure == 19 ~ -3.10009582047105,
                               JourSem == "LV" &&  DateHeure == 20 ~ -2.19209986542425,
                               JourSem == "LV" &&  DateHeure == 21 ~ -0.445100982591946,
                               JourSem == "LV" &&  DateHeure == 22 ~ 0.654308664171879,
                               JourSem == "LV" &&  DateHeure == 23 ~ 1.46642945533455,
                               
                               JourSem == "S" &&  DateHeure == 0 ~ 1.48780692362609,
                               JourSem == "S" &&  DateHeure == 1 ~ 3.09915923846212,
                               JourSem == "S" &&  DateHeure == 2 ~ 4.62903516543763,
                               JourSem == "S" &&  DateHeure == 3 ~ 6.13993778082778,
                               JourSem == "S" &&  DateHeure == 4 ~ 6.40244175194746,
                               JourSem == "S" &&  DateHeure == 5 ~ 5.17642512147307,
                               JourSem == "S" &&  DateHeure == 6 ~ 2.9491430853308,
                               JourSem == "S" &&  DateHeure == 7 ~ -1.42561176025025,
                               JourSem == "S" &&  DateHeure == 8 ~ -0.440026522327699,
                               JourSem == "S" &&  DateHeure == 9 ~ -1.48013339524666,
                               JourSem == "S" &&  DateHeure == 10 ~ -2.29622099586781,
                               JourSem == "S" &&  DateHeure == 11 ~ -2.67927910001206,
                               JourSem == "S" &&  DateHeure == 12 ~ -2.27183464187897,
                               JourSem == "S" &&  DateHeure == 13 ~ -2.01266519396376,
                               JourSem == "S" &&  DateHeure == 14 ~ -2.05246937255653,
                               JourSem == "S" &&  DateHeure == 15 ~ -2.13855296898559,
                               JourSem == "S" &&  DateHeure == 16 ~ -2.32840417325521,
                               JourSem == "S" &&  DateHeure == 17 ~ -2.61733878769439,
                               JourSem == "S" &&  DateHeure == 18 ~ -2.7481531670295,
                               JourSem == "S" &&  DateHeure == 19 ~ -2.65541445542067,
                               JourSem == "S" &&  DateHeure == 20 ~ -2.31008690958376,
                               JourSem == "S" &&  DateHeure == 21 ~ -1.01611865483581,
                               JourSem == "S" &&  DateHeure == 22 ~ -0.203574157285303,
                               JourSem == "S" &&  DateHeure == 23 ~ 0.285955775220081,
                               
                               JourSem == "D" &&  DateHeure == 0 ~ 1.07226853137743,
                               JourSem == "D" &&  DateHeure == 1 ~ 2.7327988750193,
                               JourSem == "D" &&  DateHeure == 2 ~ 4.35561515977382,
                               JourSem == "D" &&  DateHeure == 3 ~ 5.77008117491968,
                               JourSem == "D" &&  DateHeure == 4 ~ 6.33399045178363,
                               JourSem == "D" &&  DateHeure == 5 ~ 6.085218482836,
                               JourSem == "D" &&  DateHeure == 6 ~ 4.72012797591152,
                               JourSem == "D" &&  DateHeure == 7 ~ 3.68290925218579,
                               JourSem == "D" &&  DateHeure == 8 ~ 2.64380333439784,
                               JourSem == "D" &&  DateHeure == 9 ~ 1.09524976601212,
                               JourSem == "D" &&  DateHeure == 10 ~ -0.383394748825573,
                               JourSem == "D" &&  DateHeure == 11 ~ -0.857908225095848,
                               JourSem == "D" &&  DateHeure == 12 ~ -0.999227003048439,
                               JourSem == "D" &&  DateHeure == 13 ~ -0.905092243566273,
                               JourSem == "D" &&  DateHeure == 14 ~ -1.01840271281069,
                               JourSem == "D" &&  DateHeure == 15 ~ -1.59823569047699,
                               JourSem == "D" &&  DateHeure == 16 ~ -2.22671074482504,
                               JourSem == "D" &&  DateHeure == 17 ~ -2.16763540507277,
                               JourSem == "D" &&  DateHeure == 18 ~ -2.00389609002814,
                               JourSem == "D" &&  DateHeure == 19 ~ -1.79717136215053,
                               JourSem == "D" &&  DateHeure == 20 ~ -0.927374508864929,
                               JourSem == "D" &&  DateHeure == 21 ~ 0.384182112260127,
                               JourSem == "D" &&  DateHeure == 22 ~ 1.47996624855368,
                               TRUE ~ 2.66641002509156)) %>%
  mutate(leq_mean_corr = leq_mean + Correctif)

Pts_Reze_2154_sp <- as_Spatial(Pts_Reze_2154)
proj4string(Pts_Reze_2154_sp) <- CRS("+init=epsg:2154")
Pts_Reze_WGS84 <- spTransform(Pts_Reze_2154_sp ,CRS("+init=epsg:4326"))
proj4string(Pts_Reze_WGS84) <- CRS("+init=epsg:4326")


######################################
# Spatial analysis - variogram - 50000 points
#Pts_50000_2154$LEQ1s <- Pts_50000_2154$leq_mean_corr
var_Reze <- variogram(leq_mean_corr ~ 1, Pts_Reze_2154_sp, cutoff = 500, width = 25)

#plot(var_Reze)

lzn.fit = fit.variogram(var_Reze, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
lzn.fit

var_sph.fit <- fit.variogram(var_Reze, vgm(sill = 60, "Sph", nugget = 60, range = 200))
#attr(var_sph.fit, "SSErr")
#plot(var_Reze,var_sph.fit)

######################################
## GRILLE DE PROJECTION
#grid
bb <- sp::bbox(Pts_Reze_2154_sp)
cs <- c(1, 1)*25
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(Pts_Reze_2154_sp)))
summary(sp_grd)

proj4string(sp_grd) <- CRS("+init=epsg:2154")
sp_grd84 <- spTransform(sp_grd ,CRS("+init=epsg:4326"))
sp_grd84 <-st_as_sf(sp_grd84)




sp_grd <- as_Spatial(st_read(here("geo","RoadReze","points_10m_road_reze_2154.shp")))
proj4string(sp_grd) <- CRS("+init=epsg:2154")
sp_grd84 <- spTransform(sp_grd ,CRS("+init=epsg:4326"))
sp_grd84 <-st_as_sf(sp_grd84)

## MESSAGE
# 1: Dans spTransform(sp_grd, CRS("+init=epsg:4326")) :
#   Grid warping not available, coercing to points
# 2: Dans spTransform(as(x, "SpatialPixelsDataFrame"), CRSobj, ...) :
#   Grid warping not available, coercing to points


## spatial prediction
sf_grd<- st_as_sf(sp_grd)
lz.skk <- krige(leq_mean_corr  ~ 1, Pts_Reze_2154, sf_grd, var_sph.fit,nmin=1,nmax=10, maxdist =50) #maxdist =150
# MESSAGE
# Erreur dans .local(formula, locations, ...) : 
#   sf::st_crs(locations) == sf::st_crs(newdata) n'est pas TRUE

#dim(lz.sk[lz.sk$var1.var == "NA",])[1]/dim(lz.sk)[1]*100

##########

lz.reze <- st_transform(lz.skk, 4326)
lz.reze <- na.omit(lz.reze)

#source(here("R","color.R"))

palsound100 <- colorNumeric(
  palette = pal_sound100,
  #n = 100,
  domain = lz.reze$var1.pred,
  #na.color = "transparent",
  reverse = FALSE
)

options(OutDec=".")
library(leaflet.providers)
library(leaflet.extras)
#source(here("docs","leaflet-providers.js"))

map_interpo_road <- leaflet(lz.reze) %>%
  #addTiles() %>%
  #addProviderTiles("Jawg.Matrix") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    data = lz.reze,
    color = ~palsound100(lz.reze$var1.pred),
    radius = 2,
    stroke = FALSE, fillOpacity = 0.6
  ) %>%
  addLegend("bottomleft",
            pal = palsound100,
            #colors = ~pal2,
            values = lz.reze$var1.pred,
            title = "Niveaux sonores",
            labFormat = labelFormat(suffix = " dB",big.mark = " ", transform = identity),
            opacity = 1
  )

options("viewer" = function(url, ...) utils::browseURL(url))
map_interpo_road