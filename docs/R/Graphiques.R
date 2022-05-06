# Virgule pour les décimales
options(OutDec=",")
Sys.setlocale("LC_CTYPE","fr_FR.UTF-8")
#Sys.setlocale("LC_TIME", "de_DE.UTF-8")
options(encoding = "UTF-8")

# Sys.setenv("LANGUAGE"="En")
# Sys.setlocale("LC_ALL", "en_GB.UTF-8")

# Pourcentages des mesures attribués à chaque participant
AnaMesDonut <- AnaMes %>%
  arrange(desc(Id)) %>%
  mutate(lab.ypos = cumsum(Pourcentage) - 0.5*Pourcentage) %>%
  arrange(desc(Pourcentage))

colourCount <- length(unique(AnaMesDonut$Id))
getPalette <- colorRampPalette(brewer.pal(9, "Blues"))
colgrey <- colorRampPalette(c("black", "white"))(colourCount)

p1 <- ggplot(AnaMesDonut, aes(x = 2, y = round(Pourcentage,2), fill = Id)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = ifelse(round(Pourcentage,2)>3,round(Pourcentage,2),"")), col = colgrey) +
  guides(colour = colgrey) +
  scale_fill_manual(name="Participant(e)", values=getPalette(colourCount)) +
  theme_void()+
  xlim(0.5, 2.5)
ggsave(device="pdf", here("Images","Pourcent_participant.pdf"),p1)
ggsave(device="jpeg", here("Images","Pourcent_participant.jpeg"),p1)

# Répartition des mesures en fonction de la période de la journée
p2 <- ggplot(data_mes, aes(x = factor(DateHeure))) +
  geom_bar(fill="steelblue4") +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), name="Effectif") +
  scale_x_discrete("Heure")
ggsave(device="pdf", here("Images","Distrib_heure.pdf"),p2)
ggsave(device="jpeg", here("Images","Distrib_heure.jpeg"),p2)

# Représentation graphique conjointe des nbre/nbres cumulés de mesures par jour
p3 <-data_mes_jour %>%
  gather(., key="type",value="value",NbMes, NbMesCum) %>% #on passe au format long des données
  mutate(DateJour=as.Date(DateJour, format = "%d.%m.%Y")) %>%
  mutate(type = recode(type, NbMes = "Journalier", NbMesCum = "Cumulé")) %>%
  as.data.frame() %>%
  ggplot(aes(x=DateJour, y=value, fill=type)) +
  geom_col(width=0.4, position=position_dodge(width=0.5)) +
  scale_fill_manual(values=c("#10A8E3","#0A749F"), name="Nombre de mesures") +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), name="Effectif") +
  scale_x_date(breaks = data_mes_jour$DateJour, name="Date", date_labels = "%d/%m") +
  theme(axis.text.x = element_text(size=6, angle=45)) +
  #hrbrthemes::theme_ipsum() +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(device="pdf", here("Images","Mesures_distrib_jour.pdf"),p3)
ggsave(device="jpeg", here("Images","Mesures_distrib_jour.jpeg"),p3)


##########################
##########################
# TRACES
#########################
#########################

# Pourcentages des traces attribués à chaque participant
AnaTraceDonut <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  summarise(n = n()) %>%
  mutate(Pourcentage=n/sum(n)*100) %>%
  arrange(desc(n)) %>%
  arrange(desc(Id)) %>%
  mutate(lab.ypos = cumsum(Pourcentage) - 0.5*Pourcentage) %>%
  arrange(desc(Pourcentage))

colourCount <- length(unique(AnaTraceDonut$Id))
getPalette <- colorRampPalette(brewer.pal(9, "Blues"))
colgrey <- colorRampPalette(c("black", "white"))(colourCount)

p1 <- ggplot(AnaTraceDonut, aes(x = 2, y = round(Pourcentage,2), fill = Id)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = ifelse(round(Pourcentage,2)>3,round(Pourcentage,2),"")), col = colgrey) +
  guides(colour = colgrey) +
  scale_fill_manual(name="Participant(e)", values=getPalette(colourCount)) +
  theme_void()+
  xlim(0.5, 2.5)
ggsave(device="pdf", here("Images","Pourcent_participant_trace.pdf"),p1)
ggsave(device="jpeg", here("Images","Pourcent_participant_trace.jpeg"),p1)


# Représentation graphique conjointe des nbre/nbres cumulés de traces par jour
p4 <- data_trace_jour %>%
  gather(., key="type",value="value",NbTraceParJour, NbTraceParJourCum) %>% #on passe au format long des données
  mutate(DateJour=as.Date(DateJour, format = "%d.%m.%Y")) %>%
  mutate(type = recode(type, NbTraceParJour = "Journalier", NbTraceParJourCum = "Cumulé")) %>%
  as.data.frame() %>%
  ggplot(aes(x=DateJour, y=value, fill=type)) +
  geom_col(width=0.4, position=position_dodge(width=0.5)) +
  scale_fill_manual(values=c("#10A8E3","#0A749F"), name="Nombre de traces") +
  scale_y_continuous(name="Effectif") +
  scale_x_date(breaks = date_breaks("1 month"), name="Date", date_labels = "%d/%m") +
  #scale_x_date(breaks = data_trace_jour$DateJour, name="Date", date_labels = "%d/%m") +
  theme(axis.text.x = element_text(size=6, angle=45)) +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(device="pdf", here("Images","Traces_distrib_jour.pdf"),p4)
ggsave(device="jpeg", here("Images","Traces_distrib_jour.jpeg"),p4)


# data_trace_jour_gather <- data_trace_jour %>%
#   gather(., key="type",value="value",NbTraceParJour, NbTraceParJourCum) %>%
#   rename(Critere = type)
#
#   p4_ply <- plot_ly(data_trace_jour_gather, x=~DateJour, y=~value, color=~Critere, type = "scatter", mode="markers", name = ifelse(Critere == "NbTraceParJour", "Journalier", "Cumulé")) %>%
#   layout(xaxis = list(title =NA),
#          legend = list(orientation = "h",xanchor = "center",x=0.5),
#          yaxis = list(title = 'Nombre')
#   )


# Représentation graphique conjointe des nbre/nbres cumulés de traces par jour de la semaine
# À FAIRE : GRADIENT BLUES
p5 <- data_trace_jour %>%
  mutate(JourJ=wday(as.POSIXct(DateJour), label = TRUE, abbr=FALSE)) %>%
  mutate(JourJ = factor(JourJ)) %>%
  mutate(JourJ=fct_relevel(JourJ,c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche"))) %>%
  arrange(JourJ) %>%
  as.data.frame() %>%
  ggplot(aes(x=JourJ,y=NbTraceParJour,fill=JourJ)) +
  geom_col(width=0.4, fill="#0A749F") +
  scale_y_continuous(name="Effectif") +
  scale_x_discrete(name="Jour de la semaine") +
  theme(axis.text.x = element_text(size=8)) +
  theme(legend.position = "none")
ggsave(device="pdf", here("Images","Traces_distrib_joursem.pdf"),p5)
ggsave(device="jpeg", here("Images","Traces_distrib_joursem.jpeg"),p5)

# Répartition des traces en fonction de la période de la journée
p55 <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  ggplot(., aes(x = factor(DateHeure))) +
  geom_bar(fill="#0A749F") +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), name="Effectif") +
  scale_x_discrete("Heure")
ggsave(device="pdf", here("Images","Distrib_trace_heure.pdf"),p55)
ggsave(device="jpeg", here("Images","Distrib_trace_heure.jpeg"),p55)

p55_ply <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  plot_ly(., x = ~factor(DateHeure)) %>%
  layout(xaxis = list(title = 'Heure de la journée'),yaxis = list(title = 'Effectif des mesures'))


# Répartition des traces en fonction de la durée
# trace_classe <- data_trace %>%
#   distinct(IdTrace, .keep_all = TRUE) %>%
#   select(Classe)
#
# # 1. Pour les moins de 5 min
# trace_classe_inf5 <- data_trace %>%
#   filter(DureeMinutes <= 5) %>%
#   mutate(Classe = case_when(DureeMinutes < 1 ~ "0-1",
#                             DureeMinutes < 2 ~ "1-2",
#                             DureeMinutes < 3 ~ "2-3",
#                             DureeMinutes < 4 ~ "3-4",
#                             TRUE ~ "4-5")) %>%
#   mutate(Classe = factor(Classe)) %>%
#   mutate(Classe = forcats::fct_relevel(Classe,c("0-1","1-2","2-3","3-4","4-5"))) %>%
#   distinct(IdTrace, .keep_all = TRUE)
#
# p6 <- ggplot(trace_classe_inf5,aes(x = Classe)) +
#   geom_bar(fill="#0A749F") +
#   scale_x_discrete(name="Plages de durée (en minutes)") +
#   scale_y_continuous(name="Effectif", breaks= scales::pretty_breaks())
# ggsave(device="pdf", here("Images","Traces_distrib_dureemin.pdf"),p6)
# ggsave(device="jpeg", here("Images","Traces_distrib_dureemin.jpeg"),p6)
#
# # 2. Pour les plus de 5 min
# trace_classe_sup5 <- trace_classe %>%
#   distinct(IdTrace, .keep_all = TRUE) %>%
#   filter(Classe != "0-5")
# levels(trace_classe_sup5$Classe)
#
# trace_classe_sup5 <- trace_classe_sup5 %>%
#   distinct(IdTrace, .keep_all = TRUE) %>%
#   mutate(Classe = forcats::fct_relevel(Classe,c("0-5","5-10","15-20","20-25","45-50","> 60"))) %>%
#   arrange(Classe)
# levels(trace_classe_sup5$Classe)
#
#
# #****************
# trace_classe_sup5 <- data_trace %>%
#   distinct(IdTrace, .keep_all = TRUE) %>%
#   filter(Classe != "0-5") %>%
#   arrange(Classe)
# #**************
#
# p7 <- ggplot(trace_classe_sup5,aes(x = Classe)) +
#   geom_bar(fill="#0A749F") +
#   scale_x_discrete(name="Plages de durée (en minutes)") +
#   scale_y_continuous(name="Effectif", breaks= scales::pretty_breaks())
# ggsave(device="pdf", here("Images","Traces_distrib_dureemax.pdf"),p7)
# ggsave(device="jpeg", here("Images","Traces_distrib_dureemax.jpeg"),p7)


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

######## INTERNOISE
inter_part <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DureeTot = sum(DureeMinutes)) %>%
  mutate(n = n()) %>%
  distinct(Id, .keep_all = TRUE) %>%
  arrange(desc(n)) %>%
  filter(n > 9) %>%
  select(Id,n,DureeTot)

ggplot(inter_part, aes(x=Id, y=n, label=n)) +
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0,
                   x = Id,
                   yend = n,
                   xend = Id),
               color = "black") +
  geom_text(aes(label=floor(DureeTot)), size = 2.5,color="white",fontface = "bold")

##################

# Somme durées trace par participant avec distinguo calibrage
data_trace_part <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DureeTot = sum(DureeMinutes/60)) %>%
  distinct(Id, .keep_all = TRUE)

p8 <- ggplot(data_trace_part, aes(x=Id,y=DureeTot, color=method_calibration)) + geom_point(size = 1) +
  theme(axis.text.x = element_text(size=4, angle=45)) +
  scale_x_discrete(name="Participants") +
  scale_y_continuous(name="Durée totale des mesures (en heures)") +
  scale_color_discrete(name="Méthode de calibration")

ggsave(device="pdf", here("Images","Traces_durees-part.pdf"),p8)

data_trace_part <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  add_count(IdTrace) %>%
  mutate(NbreTot = sum(n)) %>%
  mutate(DureeTot = sum(DureeMinutes)) %>%
  ungroup() %>%
  arrange(Id)

p9 <- ggplot(data_trace_part, aes(x=log(NbreTot),y=DureeTot/60)) + geom_point(size = 1) +
  theme(axis.text.x = element_text(size=4)) +
  scale_y_continuous(name="Durée (en heure) d'enregistrement") +
  #scale_color_discrete(name="Participants") +
  #scale_shape_discrete(name="Méthode de calibration", solid = FALSE) + #,values=c(1,15,16,17,18)) +
  scale_x_continuous(name="Logarithme du nombre de mesures par participant") +
  guides(color = "none")


ggsave(device="pdf", here("Images","Traces_nbre_durees-part.pdf"),p9)

p9_ply <- plot_ly(data_trace_part, x=~log(NbreTot),y=~DureeTot/60, marker = list(size = 10),type="scatter") %>%
  layout(xaxis = list(title = 'Logarithme du nombre de mesures par participant'),yaxis = list(title = 'Durée (en heures) totale des mesures par participant'))


####################################################################################
# Cartography


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

pal2 <- colorNumeric(
  #palette = "Blues", #"RdYlGn",
  palette = as.character(wes_palette("Zissou1")),
  #n = 9,
  domain = stations_sf_4326$leq_mean,
  #na.color = "transparent",
  reverse = FALSE
  )

# Carto avec les polygones
options(OutDec=".")
map1 <- leaflet(stations_sf_4326) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar( position = c("bottomright"))%>%
  addPolygons(color = ~pal2(stations_sf_4326$leq_mean), weight = 0, smoothFactor = 0.,
              opacity = 0.0, fillOpacity = 0.7
              ) %>%
  addLegend("bottomleft",
            pal = pal2,
            values = stations_sf_4326$leq_mean,
            na.label = "NA",
            title = "Levels",
            #labels = pal2,
            opacity = .8,
            labFormat = labelFormat(suffix = " dB",big.mark = " ", transform = identity),
            )
map1
#
#mapshot(map1, here("Images","Mesures_Polygones.png"),remove_controls = c("zoomControl"))


## ZOZO / B.A.-B.A.BA
palbo <- colorNumeric(
  palette = "Blues",
  domain = stations$leq_mean
)

map2 <- leaflet(stations_sf_4326) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar( position = c("bottomright"))%>%
  addPolygons(color = ~palbo(leq_mean), weight = 0, smoothFactor = 0.,
              opacity = 0.0, fillOpacity = 0.7,
  ) %>%
  addLegend("bottomleft",
            pal = palbo,
            values = stations$leq_mean,
            title = "Niveau moyen de bruit",
            opacity = 1
  )

map2
#####################


# Carte de chaleur
pal3 <- colorNumeric(
  palette = "Blues",
  domain = stations$leq_mean,
  reverse = TRUE)

# Ronds dynamiques
leaflet(SPDF_sf) %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addMarkers(
    clusterOptions = markerClusterOptions()
  ) %>%
  addCircleMarkers(
    radius = 5,
    color = ~pal3(stations$leq_mean),
    stroke = FALSE, fillOpacity = 0.2
  )



### PLEASANTNESS
############# CARTO
# Data_numerique_pleasant <- data_nc[,c("x","y","pleasantness")]
# Data_numerique_pleasant$pleasantness <- as.numeric(Data_numerique_pleasant$pleasantness)
# summary(Data_numerique_pleasant$pleasantness)
# summary(factor(Data_numerique_pleasant$pleasantness))
#
# Data_numerique_pleasant <- Data_numerique_pleasant %>%
#   drop_na()
# summary(Data_numerique_pleasant$pleasantness)
#
# SPDF <- SpatialPointsDataFrame(coords=Data_numerique_pleasant[,1:2], data=as.data.frame(Data_numerique_pleasant))
# SPDF_sf <- st_as_sf(SPDF, crs = 4326, agr = "constant", remove = F)
# st_crs(SPDF_sf) <- 4326 #EPSG WGS84
#
# SPDF_sf_2154 <- st_transform(SPDF_sf, 2154) #LAMBERT 93
#
# bbox <- st_bbox(SPDF_sf_2154)
# grid <- sf::st_make_grid(st_as_sfc(bbox),cellsize = 50, square = FALSE)  #Grid of 50 meters
# st_crs(grid) <- 2154
#
# stations <- aggregate(x = SPDF_sf_2154, by = grid, FUN = median)         #Calculate median
#
# stations <- stations %>%
#   drop_na(geometry) %>%
#   drop_na(pleasantness)
#
# stations_sf <- st_as_sf(stations, crs = 2154, agr = "constant",
#                         remove = F)
#
# stations_sf_4326 <- stations_sf %>%
#   st_transform(4326) # repasse en WGS84 (LATti LONgi )
#
# pal2 <- colorNumeric(
#   palette = "Blues", #"RdYlGn",
#   #n = 9,
#   domain = stations$pleasantness,
#   #na.color = "transparent",
#   #reverse = TRUE
# )
#
# map4 <- leaflet(stations_sf_4326) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addScaleBar( position = c("bottomright"))%>%
#   addPolygons(color = ~pal2(pleasantness), weight = 0, smoothFactor = 0.,
#               opacity = 0.0, fillOpacity = 0.7,
#   ) %>%
#   addLegend("bottomleft",
#             pal = pal2,
#             #colors = ~pal2,
#             values = stations$pleasantness,
#             title = "Index d'agrément",
#             #labels = pal2,
#             labFormat = labelFormat(suffix = "",big.mark = " ", transform = identity),
#             opacity = 1
#   )
#
# map4
