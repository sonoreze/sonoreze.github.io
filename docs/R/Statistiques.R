if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(here,thematic,lubridate,dplyr,tidyr,stringr,ggplot2,ggforce,forcats,rgdal,sp,sf,rgeos,geoR,tidyr,devtools,leaflet,leaflet.extras,jsonlite,RColorBrewer,viridis,forcats,mapview,scales,flexdashboard,fontawesome,rprojroot,ggthemes,plotly,spacetime,gstat)

# Données aberrantes ?
load("noisecapture_data.Rda")
#write.csv(noisecapture_data,file="noisecapture_data.csv")

noisecapture_data <-as.data.frame(noisecapture_data)
summary(noisecapture_data)
class(noisecapture_data)

# Changement de nom du dataframe
data_nc <- noisecapture_data
dim(data_nc)[1]

# Dates aberrantes : inférieures à novembre 2021
databer_date <- data_nc[data_nc$Date < "2021-11-01 00:00:01",]
dim(databer_date)

# Géolocalisation manquante
databer_geo <- data_nc[data_nc$x == "NA" | data_nc$y =="NA",]
dim(databer_geo)[1]

# Sont-ce les mêmes ? Oui
databer <- data_nc[data_nc$Date < "2021-11-01 00:00:01" | data_nc$x == "NA" | data_nc$y =="NA",]
dim(databer)[1]

# Précision
data_accuracy <- data_nc[data_nc$accuracy >= 20,]
dim(data_accuracy)[1]

# Retrait des valeurs manquantes/aberrantes et précision supérieure à 20
data_nc <- data_nc %>%
  filter(x != "NA") %>%
  filter(accuracy<20)

dim(data_nc)[1]
class(data_nc)
summary(data_nc)

# Nombre de participants
data_nc <- data_nc %>%
  mutate(Id=factor(Id))

nb_participant <- data_nc %>%
  distinct(Id) %>%
  summarise(n = n())

# Nombre de mesures
nb_mesure <- dim(data_nc)[1]
nb_mesure_h <- round(dim(data_nc)[1]/3600,0)

# Nombre de mesures par participants, avec pourcentage sur l'ensemble
AnaMes <- data_nc %>%
  group_by(Id) %>%
  summarise(n = n()) %>%
  mutate(Pourcentage=n/sum(n)*100) %>%
  arrange(desc(n))
AnaMes

# Sélection d'un enregistrement par participant et par seconde
# mais devenu inutile : le nombre de lignes est le même
data_mes <- data_nc %>%
  group_by(Id,Date) %>%
  sample_n(1)

# Ajout de la variable jour et de la variable heure
data_mes <- data_mes %>%
  mutate(DateJour=as.Date(Date)) %>%
  mutate(DateHeure=hour(as.POSIXct(Date)))

# Évolution du nombre de mesures par jour
data_mes_jour <- as.data.frame(data_mes) %>%
  group_by(DateJour) %>%
  summarise(NbMes = n()) %>%
  mutate(NbMesCum=cumsum(NbMes))

# Évolution du nombre de participants par jour
data_ind <- data_mes %>%
  group_by(DateJour) %>%
  summarise(NbId = n_distinct(Id))

# Création d'un identifiant des durée d'enregistrements consécutifs,
# ainsi que la variable jour
# et calcul du nombre d'enregistrements (pas d'1 s) pour chaque durée
data_trace <- as.data.frame(data_mes) %>%
  arrange(Id,Date) %>%
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
  arrange(Id,IdTrace) %>%
  mutate(DateJour=as.Date(Date)) %>%
  mutate(DateJourSem=wday(Date)) %>%
  mutate(DateHeure=hour(as.POSIXct(Date))) %>%
  arrange(Id) %>%
  group_by(IdTrace) %>%
  mutate(DureeSec=n()) %>%
  arrange(Id,desc(DureeSec)) %>%
  mutate(DureeMinutes=DureeSec/60)

summary(data_trace$IdTrace)

# Nombre de traces
nb_trace <- max(data_trace$IdTrace)

# Nombre de traces par participants, avec pourcentage sur l'ensemble
data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  summarise(n = n()) %>%
  mutate(Pourcentage=n/sum(n)*100) %>%
  arrange(desc(n))

# Participants actifs dans les 7 derniers jours
id_recent <- data_trace %>%
  select(Id,Date,IdTrace) %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DiffTime = (as.POSIXct(Sys.Date())-as.POSIXct(Date))/(60*60*48)) %>%
  mutate(Recent = case_when(DiffTime < 8 ~ 1,
                            TRUE ~ 0)) %>%
  filter(Recent == 1) %>%
  distinct(Id, .keep_all = TRUE)

part_derniereSem <- nrow(id_recent)

# Évolution du nombre de traces par jour et en cumulé
data_trace_jour <- data_trace %>%
  group_by(DateJour) %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  summarise(NbTraceParJour = n()) %>%
  mutate(NbTraceParJourCum=cumsum(NbTraceParJour))

# Somme durées trace par participant avec distinguo calibrage
data_trace_part <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DureeTot = sum(DureeMinutes/60)) %>%
  distinct(Id, .keep_all = TRUE)

part_duree_max <- max(data_trace_part$DureeTot)
sum(data_trace_part$DureeTot)

#summary(data_trace_jour[data_trace_jour$ ])

# Évolution du nombre de traces par jour de la semaine
data_trace_summary <- data_trace %>%
  group_by(DateJourSem) %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  summarise(NbTraceParJourSem = n()) %>%
  mutate(NbTracParJourSemCum=cumsum(NbTraceParJourSem))

####################
## MYRIAM
####################

traces1 <- data_trace %>%
  group_by(Id) %>%
  summarise(DateDebut=min(ymd(as.Date(Date))),
            DateFin=max(ymd(as.Date(Date))),
            Duree=DateFin-DateDebut+1)

traces2 <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  summarise(NbreEnregist = n()) %>%
  mutate(Pourcentage=NbreEnregist/sum(NbreEnregist)*100)

trace3 <- data_trace %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DureeTot = round(sum(DureeMinutes/60),2)) %>%
  distinct(Id, .keep_all = TRUE) %>%
  select(Id,IdTrace,DureeTot)

new_df0 <-
  left_join(traces1,
            traces2,
            by = "Id")
new_df <-
  left_join(new_df0,
            trace3,
            by="Id") %>%
  arrange(desc(NbreEnregist))


new_def_2 <- data_trace %>%
  select(Id,IdTrace,Date) %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  group_by(Id) %>%
  mutate(DateAbbreg=ymd(as.Date(Date))) %>%
  ungroup() %>%
  group_by(Id,DateAbbreg) %>%
  mutate(NbreEnregist = n()) %>%
  distinct(Id,DateAbbreg,.keep_all= TRUE)
