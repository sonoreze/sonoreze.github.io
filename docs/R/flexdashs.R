part_hist <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  summarise(n = n()) %>%
  mutate(Pourcentage=n/sum(n)*100) %>%
  arrange(desc(n)) %>%
  ggplot(., aes(x=reorder(Id, -Pourcentage), y=Pourcentage)) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Participants") +
  theme(axis.text.x = element_text(size=6, angle=45)) +
  scale_y_continuous("Pourcentages des contributions en nombre de mesures")


part_hist_ply <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  summarise(n = n()) %>%
  mutate(Pourcentage=n/sum(n)*100) %>%
  arrange(desc(n)) %>%
  plot_ly(., x = ~Id, y = ~Pourcentage, type = 'bar') %>%
  layout(xaxis = list(title = 'Participants', tickangle = -45), yaxis = list(title = 'Pourcentage sur le total des mesures')) %>%
  layout(xaxis = list(titlefont = list(size = 6), tickfont = list(size = 12)))

################################

data_trace_part <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DureeTot = sum(DureeMinutes/60)) %>%
  distinct(Id, .keep_all = TRUE)

data_trace_part <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  add_count(IdTrace) %>%
  mutate(NbreTot = sum(n)) %>%
  mutate(DureeTot = sum(DureeMinutes)) %>%
  ungroup() %>%
  arrange(Id)

p9_ply <- plot_ly(data_trace_part, x=~log(NbreTot),y=~DureeTot/60, marker = list(size = 10),type="scatter") %>%
  layout(xaxis = list(title = 'Logarithme du nombre de mesures par participant'),yaxis = list(title = 'Durée (en heures) totale des mesures par participant'))

###################################
Record_ly <- data_trace_jour %>%
  as.data.frame() %>%
  plot_ly(. , x = ~DateJour, y = ~NbTraceParJour, type = 'bar', name = 'Journalier') %>%
  add_trace(data , y = ~NbTraceParJourCum, name = 'Cumulé', marker = list(color = rgb(243/255, 150/255, 154/255))) %>%
  layout(xaxis = list(title = 'Date')) %>%
  layout(yaxis = list(title = "Nombre de mesures")) %>%
  config(locale = 'fr')

###################################
p55_ply <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  plot_ly(., x = ~factor(DateHeure)) %>%
  layout(xaxis = list(title = 'Heure de la journée'),yaxis = list(title = 'Effectif des mesures'))

####################################
## CARTES

##### Données brutes
Data_numerique <- data_nc[,c("x","y","leq_mean")]
Data_numerique$leq_mean <- as.numeric(Data_numerique$leq_mean)
Data_numerique <- Data_numerique %>%
  drop_na()

SPDF <- SpatialPointsDataFrame(coords=Data_numerique[,1:2], data=as.data.frame(Data_numerique))
SPDF_sf <- st_as_sf(SPDF, crs = 4326, agr = "constant", remove = F)
st_crs(SPDF_sf) <- 4326 #EPSG WGS84

SPDF_sf_2154 <- st_transform(SPDF_sf, 2154) #LAMBERT 93

bbox <- st_bbox(SPDF_sf_2154)
grid <- sf::st_make_grid(st_as_sfc(bbox),cellsize = 50, square = FALSE)  #Grid of 50 meters
st_crs(grid) <- 2154

stations <- aggregate(x = SPDF_sf_2154, by = grid, FUN = median)         #Calculate median

stations <- stations %>%
  drop_na(geometry) %>%
  drop_na(leq_mean)

stations_sf <- st_as_sf(stations, crs = 2154, agr = "constant",
                        remove = F)

stations_sf_4326 <- stations_sf %>%
  st_transform(4326) # repasse en WGS84 (LATti LONgi )

# Palettes
palsound10 <- colorNumeric(
  palette = pal_sound10,
  #n = 10,
  domain = stations_sf_4326$leq_mean,
  #na.color = "transparent",
  reverse = FALSE
)

palsound100 <- colorNumeric(
  palette = pal_sound100,
  #n = 100,
  domain = stations_sf_4326$leq_mean,
  #na.color = "transparent",
  reverse = FALSE
)

# Carto avec les polygones
options(OutDec=".")
map1 <- leaflet(stations_sf_4326) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar( position = c("bottomright"))%>%
  addPolygons(color = ~palsound100(stations_sf_4326$leq_mean), weight = 0, smoothFactor = 0.,
              opacity = 0.0, fillOpacity = 0.7
  ) %>%
  addLegend("bottomleft",
            pal = palsound100,
            values = stations_sf_4326$leq_mean,
            na.label = "NA",
            title = "Niveaux sonores",
            #labels = pal2,
            opacity = .8,
            labFormat = labelFormat(suffix = " dB",big.mark = " ", transform = identity),
            #position = "topleft"
  )
map1

######################################


###################################
Data_numerique_pleasant <- data_nc[,c("x","y","pleasantness")]
Data_numerique_pleasant$pleasantness <- as.numeric(Data_numerique_pleasant$pleasantness)
Data_numerique_pleasant <- Data_numerique_pleasant %>%
  drop_na()

SPDF <- SpatialPointsDataFrame(coords=Data_numerique_pleasant[,1:2], data=as.data.frame(Data_numerique_pleasant))
SPDF_sf <- st_as_sf(SPDF, crs = 4326, agr = "constant", remove = F)
st_crs(SPDF_sf) <- 4326 #EPSG WGS84

SPDF_sf_2154 <- st_transform(SPDF_sf, 2154) #LAMBERT 93

bbox <- st_bbox(SPDF_sf_2154)
grid <- sf::st_make_grid(st_as_sfc(bbox),cellsize = 50, square = FALSE)  #Grid of 50 meters
st_crs(grid) <- 2154

stations <- aggregate(x = SPDF_sf_2154, by = grid, FUN = median)         #Calculate median

stations <- stations %>%
  drop_na(geometry) %>%
  drop_na(pleasantness)

stations_sf <- st_as_sf(stations, crs = 2154, agr = "constant",
                        remove = F)

stations_sf_4326 <- stations_sf %>%
  st_transform(4326) # repasse en WGS84 (LATti LONgi )

options(OutDec=".")
map4 <- leaflet(stations_sf_4326) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar( position = c("bottomright"))%>%
  addPolygons(color = ~palsound10(stations$pleasantness), weight = 0, smoothFactor = 0.,
              opacity = 0.0, fillOpacity = 0.7,
  ) %>%
  addLegend("bottomleft",
            pal = palsound10,
            #colors = ~pal2,
            values = stations$pleasantness,
            title = "Indice d'agrément",
            #labels = pal2,
            na.label = "NA",
            labFormat = labelFormat(suffix = "",big.mark = " ", transform = identity),
            opacity = 1,
            #position = "topleft"
  )
map4
