library("tm")
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")
library("here")
library("dplyr")
library("leaflet")
library("stringr")
library("sf")
library("viridis")
library("tidyr")

tags_table_init <- as.data.frame(noisecapture_data[,c("Id","Date","x","y","leq_mean","tags","accuracy")]) %>%
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
  select(-c(accuracy,IdGlobal,IdTraceReset))

tags_table <- tags_table_init %>%
  st_as_sf(coords = c("x","y"),remove = F) %>%
  st_set_crs(4326) %>%
  group_by(IdTrace) %>%
  mutate(
    lead = geometry[row_number() + 1],
    Dist = st_distance(geometry, lead, by_element = T),
  ) %>%
  mutate(DistTot = sum(as.numeric(Dist),na.rm=TRUE)) %>%
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

# Text mining
split_tags <- tm::Corpus(tm::VectorSource(tags_table_taggroup$tags))
inspect(split_tags)
reze_dtm <- tm::TermDocumentMatrix(split_tags)
m <- as.matrix(reze_dtm)
v <- sort(rowSums(m),decreasing=TRUE)
reze_freq <- data.frame(word = names(v),freq=v)
head(reze_freq, 10)

set.seed(1234)
wordcloud(words = reze_freq$word, freq = reze_freq$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

## Carte
Data_numerique <- tags_table_taggroup[,c("x","y","TagGroup")]
pal <- colorFactor(viridis(7), Data_numerique$TagGroup)

leaflet(Data_numerique) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1.5,
                   color = pal(Data_numerique$TagGroup),
                   stroke=FALSE,
                   fillOpacity = 0.8,
                   popup = ~TagGroup) %>%
  addLegend("bottomleft",
            pal = pal,
            values = Data_numerique$TagGroup,
            na.label = "NA",
            title = "Catégories",
            #labels = pal2,
            opacity = .8,
  )

## Deux
Data_numerique <- tags_table_taggroup[,c("x","y","tags")]
pal <- colorFactor(viridis(7), Data_numerique$tags)

leaflet(Data_numerique) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 1.5,
                   color = pal(Data_numerique$tags),
                   stroke=FALSE,
                   fillOpacity = 0.8,
                   popup = ~tags) %>%
  addLegend("bottomleft",
            pal = pal,
            values = Data_numerique$tags,
            na.label = "NA",
            title = "Catégories",
            #labels = pal2,
            opacity = .8,
  )
