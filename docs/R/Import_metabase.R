### D'après Pierre Aumond
### et Nicolas Fortin

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(here,thematic,lubridate,dplyr,tidyr,stringr,ggplot2,hrbrthemes,forcats,rgdal,sp,sf,
               rgeos,tidyr,devtools,leaflet,leaflet.extras,jsonlite,RColorBrewer,
               viridis,forcats,mapview,scales,flexdashboard)

## on suppose qu'il existe un dossier metabase dans lequel on va stocker
# les données issues de Noise-planet (après éventuelle requête) au format .csv,
# on va renommer ce fichier en query.csv : c'est le fichier contenant les url
# Cf. instructions site Noise-planet

# on créé le dossier data
dir.create(here("metabase","data"), showWarnings = FALSE)

# on créé le dossier /data/unzip/
dir.create(here("metabase/data","unzip"), showWarnings = FALSE)


fileName <- "query.csv"
# on en fait un data.frame
con <- read.csv(here("metabase","query.csv"))
con <- data.frame(con)

noisecapture_data<-data.frame()

# on récupère les fichiers zip que l'on stocke dans data
for (i in 1:dim(con)[1]){
  # par rapport au code initial, les url se retrouvent maintenant dans la 3e colonne
  #download.file(url = con[i,2], destfile = here("metabase/data",basename(con[i,2])))
  download.file(url = con[i,3], destfile = here("metabase/data",basename(con[i,3])))
}


# on récupère les noms des fichiers .zip
list_zip <- list.files(path = here("metabase","data"), pattern = "\\.zip$")

for(l in list_zip){

  # on retire l'extension .zip
  ll <- sub('\\.zip$', '', l)
  # on créé un dossier par archive zip
  dir.create(here("metabase/data/unzip",ll), showWarnings = FALSE)

  # on dézippe dans chaque dossier
  unzip(paste(here("metabase/data/"),l,sep=""), exdir= paste(here("metabase/data/unzip/"),ll,sep=""))

  # on lit le fichier geojson
  dirgeojson <- paste(paste(here("metabase/data/unzip"),ll,sep="/"),"track.geojson",sep="/")
  datajson <-jsonlite::read_json(dirgeojson, simplifyVector = TRUE)

  # on récupère le nom du fichier meta.properties
  meta0 <- paste(paste(here("metabase/data/unzip"),ll,sep="/"),"meta.properties",sep="/")

      data <- datajson$features$properties %>% as.data.frame(.)
      geom <- datajson$features$geometry %>% as.data.frame(.) %>% .$coordinates

      geom[sapply(geom, is.null)] <- NA

      data$x <- unlist(lapply(geom,function(x) x[1]))
      data$y <- unlist(lapply(geom,function(x) x[2]))

      #data <- type.convert(data)
      data <- data[c("leq_100", "leq_125", "leq_160","leq_200", "leq_250", "leq_315","leq_400", "leq_500", "leq_630",
                     "leq_800", "leq_1000", "leq_1250","leq_1600", "leq_2000", "leq_2500","leq_3150", "leq_4000", "leq_5000"
                     , "leq_6300", "leq_8000","leq_10000","leq_12500","leq_16000","leq_mean","x","y","leq_utc","accuracy")]

      data <- data.frame(data)

      # on récupère l'id de la personne en ne conservant que les 4 premiers caractères
      meta <- read.delim(meta0, header = FALSE) %>%
        as.data.frame(.)

      meta <<- meta

      # on récupère l'id de la personne en ne conservant que les 4 premiers caractères
      uuid <- meta %>%
        filter(startsWith(V1, 'uuid=')) %>%
        str_sub(., 6, 9)

      gain_cal<- meta %>%
        filter(startsWith(V1, 'gain_calibration=')) %>%
        str_remove(., "gain_calibration=")

      method_cal<- meta %>%
        filter(startsWith(V1, 'method_calibration=')) %>%
        str_remove(., "method_calibration=")

      # On récupère les 5 premiers caractères des objets de meta$V1 = meta[1]
      #prep <- c("#Nois","#Tue","uuid=","versi","metho","build","devic","recor","versi","user_","time_","devic","leq_m","gain_","tags=","devic")
      pls <- c("pleas")

      if (!(pls %in%  str_sub(meta[,1],1,5)))
        { pleasant<- c("NA") }
      else
        {
          pleasant<- meta %>%
          filter(startsWith(V1, 'pleasantness=')) %>%
          str_remove(., "pleasantness=")
        }

      timelength<- meta %>%
        filter(startsWith(V1, 'time_length=')) %>%
        str_remove(., "time_length=")
      #
      taggs<- meta %>%
        filter(startsWith(V1, 'tags=')) %>%
        str_remove(., "tags=")

      # on récupère la date et l'heure et on réordonne les colonnes
      temp <- data %>%
        mutate(Date = lubridate::as_datetime(.$leq_utc/1000, tz="UTC")) %>%
        select(!leq_utc) %>%
        mutate(Id=uuid) %>%
        mutate(gain_calibration=gain_cal) %>%
        mutate(method_calibration=method_cal) %>%
        mutate(pleasantness=pleasant) %>%
        mutate(time_length=timelength) %>%
        mutate(tags=taggs) %>%
        relocate(Id,Date,x,y)

      noisecapture_data <- bind_rows(temp, noisecapture_data)
      noisecapture_data
}

save(noisecapture_data, file ="noisecapture_data.Rda")
saveRDS(noisecapture_data, file = "noisecapture_data.rds")


#write.csv(noisecapture_data,file="noisecapture_data.csv")
