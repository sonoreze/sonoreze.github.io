# Données aberrantes ?
#load("noisecapture_data.Rda")

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

#verif <- data_ind[data_ind$DateJour == "2021-12-13",]
#sum(rle(unique(verif$Id))$length)
#verif <- data_ind[data_ind$DateJour == "2021-12-06",]
#sum(rle(unique(verif$Id))$length)

# Création d'un identifiant des durée d'enregistrements consécutifs,
# ainsi que la variable jour
# et calcul du nombre d'enregistrements (pas d'1 s) pour chaque durée
data_trace <- as.data.frame(data_mes) %>%
  arrange(Id,Date) %>%
  mutate(IdTrace=cumsum(c(TRUE, as.integer(diff(as.POSIXct(Date)), units = "secs") >= 2L))) %>%
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

data_trace <- data_trace %>%
  mutate(Classe = case_when(DureeMinutes < 5 ~ "0-5",
                            DureeMinutes < 10 ~ "5-10",
                            DureeMinutes < 15 ~ "10-15",
                            DureeMinutes < 20 ~ "15-20",
                            DureeMinutes < 25 ~ "20-25",
                            DureeMinutes < 30 ~ "25-30",
                            DureeMinutes < 35 ~ "30-35",
                            DureeMinutes < 40 ~ "35-40",
                            DureeMinutes < 45 ~ "40-45",
                            DureeMinutes < 50 ~ "45-50",
                            DureeMinutes < 55 ~ "50-55",
                            DureeMinutes < 60 ~ "55-60",
                            TRUE ~ "> 60")) %>%
  mutate(Classe = factor(Classe)) %>%
  mutate(Classe = forcats::fct_relevel(Classe,c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60","> 60"))) %>%
  arrange(Classe)
levels(data_trace$Classe)

# Résumé
summary(data_trace$DureeMinutes)

# Évolution du nombre de traces par jour et en cumulé
data_trace_jour <- data_trace %>%
  group_by(DateJour) %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  summarise(NbTraceParJour = n()) %>%
  mutate(NbTraceParJourCum=cumsum(NbTraceParJour))

#summary(data_trace_jour[data_trace_jour$ ])

# Évolution du nombre de traces par jour de la semaine
data_trace %>%
  group_by(DateJourSem) %>%
  distinct(IdTrace, .keep_all = TRUE) %>%
  summarise(NbTraceParJourSem = n()) %>%
  mutate(NbTracParJourSemCum=cumsum(NbTraceParJourSem))
