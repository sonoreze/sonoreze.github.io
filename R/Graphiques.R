# Virgule pour les décimales
options(OutDec=",")

# Pourcentages des mesures attribués à chaque participant
# À FAIRE : DÉGRADÉ DES LABELS AVEC LA PALETTE COLDGREY ET DÉCIMALE VIRGULE
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
  scale_x_date(breaks = data_trace_jour$DateJour, name="Date", date_labels = "%d/%m") +
  theme(axis.text.x = element_text(size=6, angle=45)) +
  guides(fill = guide_legend(reverse=TRUE))
ggsave(device="pdf", here("Images","Traces_distrib_jour.pdf"),p4)
ggsave(device="jpeg", here("Images","Traces_distrib_jour.jpeg"),p4)

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

# Répartition des mesures en fonction de la période de la journée
p55 <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  ggplot(., aes(x = factor(DateHeure))) +
  geom_bar(fill="#0A749F") +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE), name="Effectif") +
  scale_x_discrete("Heure")
ggsave(device="pdf", here("Images","Distrib_trace_heure.pdf"),p55)
ggsave(device="jpeg", here("Images","Distrib_trace_heure.jpeg"),p55)

# Répartition des traces en fonction de la durée
trace_classe <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  select(Classe)

# 1. Pour les moins de 5 min
trace_classe_inf5 <- data_trace %>%
  filter(DureeMinutes <= 5) %>%
  mutate(Classe = case_when(DureeMinutes < 1 ~ "0-1",
                            DureeMinutes < 2 ~ "1-2",
                            DureeMinutes < 3 ~ "2-3",
                            DureeMinutes < 4 ~ "3-4",
                            TRUE ~ "4-5")) %>%
  mutate(Classe = factor(Classe)) %>%
  mutate(Classe = forcats::fct_relevel(Classe,c("0-1","1-2","2-3","3-4","4-5"))) %>%
  distinct(IdTrace, .keep_all = TRUE)

p6 <- ggplot(trace_classe_inf5,aes(x = Classe)) +
  geom_bar(fill="#0A749F") +
  scale_x_discrete(name="Plages de durée (en minutes)") +
  scale_y_continuous(name="Effectif", breaks= scales::pretty_breaks())
ggsave(device="pdf", here("Images","Traces_distrib_dureemin.pdf"),p6)
ggsave(device="jpeg", here("Images","Traces_distrib_dureemin.jpeg"),p6)

# 2. Pour les plus de 5 min
trace_classe_sup5 <- trace_classe %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  filter(Classe != "0-5")
levels(trace_classe_sup5$Classe)

trace_classe_sup5 <- trace_classe_sup5 %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  mutate(Classe = forcats::fct_relevel(Classe,c("0-5","5-10","15-20","20-25","45-50","> 60"))) %>%
  arrange(Classe)
levels(trace_classe_sup5$Classe)


#****************
trace_classe_sup5 <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  filter(Classe != "0-5") %>%
  arrange(Classe)
#**************

p7 <- ggplot(trace_classe_sup5,aes(x = Classe)) +
  geom_bar(fill="#0A749F") +
  scale_x_discrete(name="Plages de durée (en minutes)") +
  scale_y_continuous(name="Effectif", breaks= scales::pretty_breaks())
ggsave(device="pdf", here("Images","Traces_distrib_dureemax.pdf"),p7)
ggsave(device="jpeg", here("Images","Traces_distrib_dureemax.jpeg"),p7)


####################################################################################
# Cartography


Data_numerique <- data_nc[,c("x","y","leq_mean")]

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
  palette = "Blues", #"RdYlGn",
  #n = 9,
  domain = stations$leq_mean,
  #na.color = "transparent",
  #reverse = TRUE
  )

# Carto avec les polygones
map1 <- leaflet(stations_sf_4326) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar( position = c("bottomright"))%>%
  addPolygons(color = ~pal2(leq_mean), weight = 0, smoothFactor = 0.,
              opacity = 0.0, fillOpacity = 0.7,
              ) %>%
  addLegend("bottomleft",
            pal = pal2,
            #colors = ~pal2,
            values = stations$leq_mean,
            title = "Niveau moyen de bruit",
            #labels = pal2,
            labFormat = labelFormat(suffix = " dB",big.mark = " ", transform = identity),
            opacity = 1
            )
map1
#
mapshot(map1, here("Images","Mesures_Polygones.png"),remove_controls = c("zoomControl"))


## ZOZO / B.A.-B.A.BA
palbo <- colorNumeric(
  palette = "Blues",
  domain = stations$leq_mean
)
leaflet(stations_sf_4326) %>%
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
#####################


# Carte de chaleur


pal3 <- colorNumeric(
  palette = "Blues",
  domain = stations$leq_mean,
  reverse = TRUE)

leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addWebGLHeatmap(data = SPDF_sf, size = 150, units = "m", intensity = 0.008,
                  alphaRange = -1, gradientTexture="deep-sea")


# Ronds dynamiques
map2 <- leaflet(SPDF_sf) %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addMarkers(
    clusterOptions = markerClusterOptions()
  ) %>%
  addCircleMarkers(
    radius = 10,
    color = ~pal2(leq_mean),
    stroke = FALSE, fillOpacity = 0.5
  )

map2
